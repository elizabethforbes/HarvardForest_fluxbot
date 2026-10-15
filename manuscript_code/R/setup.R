# Shared setup for every script: paths, loaders, data-hygiene (QC) rules, hourly assembly and the
# record() / write_numbers() helpers. Run all scripts from manuscript_code/ (run_all.R does this).
#
# Data flow
#   ../data_package/      raw inputs (read by 1_clean/01_fluxes.R and a few diagnostics)
#   ../data_clean/fluxes/ per-closure fluxes for every closure (1_clean/01-02)
#   ../data_clean/        closures with QC flags and status; chamber-hour analysis datasets (1_clean/03)
#   outputs/              results/, numbers/, figures/, si_tables/, diagnostics/

suppressPackageStartupMessages({
  library(lubridate)
  library(dplyr)
  library(tidyr)
  library(fuzzyjoin)
  library(readr)
})

source("R/filter_iqr.R")

pkg <- file.path("..", "data_package")                          # raw inputs
clean_dir <- file.path("..", "data_clean")                      # cleaned datasets (1_clean/)
flux_dir <- file.path(clean_dir, "fluxes")                      # per-closure fluxes, all closures
out_dir <- file.path("outputs", "results")                      # analysis results (tables, model objects)
fig_dir <- file.path("outputs", "figures")                      # manuscript and SI figures
diag_dir <- file.path("outputs", "diagnostics")                 # figures not in the paper
num_dir <- file.path("outputs", "numbers")                      # every number quoted in the paper, one file per script
si_dir <- file.path("outputs", "si_tables")                     # formatted SI tables
for (d in c(out_dir, fig_dir, diag_dir, num_dir, si_dir)) dir.create(d, showWarnings = FALSE, recursive = TRUE)

# ---- met data (Fisher station, HF001) ----------------------------------------
# data_package/ancillary holds the 2023 extract of HF001 (15-min). HF001 timestamps
# are Eastern Standard Time year-round (tz = "Etc/GMT+5").
load_met <- function(doy_range = c(273, 308), tz = "Etc/GMT+5") {
  read.csv(file.path(pkg, "ancillary", "hf001-10-15min-m_2023.csv")) %>%
    mutate(doy = yday(datetime), year = year(datetime)) %>%
    filter(year == 2023, doy >= doy_range[1], doy <= doy_range[2]) %>%
    mutate(Time = as.POSIXct(datetime, format = "%Y-%m-%dT%H:%M", tz = tz))
}

round_hour <- function(ts) {
  dt <- if (inherits(ts, "POSIXct")) with_tz(ts, "America/New_York") else
    as.POSIXct(ts, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York")
  if_else(minute(dt) >= 30, ceiling_date(dt, unit = "hour"), floor_date(dt, unit = "hour"))
}

# ---- flux estimates -------------------------------------------------------------
# Fluxes recomputed for both systems from the raw records in data_package/ by
# 1_clean/01_fluxes.R (goFlux). flux_col: "LM.flux" (linear fit; main analysis),
# "best.flux" (goFlux LM/HM model selection; SI), "HM.flux".
lab_sys <- c(autochamber = "Autochamber", fluxbot = "Fluxbot 2.0")   # system labels used in tables and figures
# ---- main analysis choices --------------------------------------------------------
# Flux model: goFlux best-model selection (linear or Hutchinson-Mosier, per closure). The smaller
# Fluxbot headspace saturates sooner, so its concentration curves bend more; a linear fit would
# underestimate its flux. Linear fits for both systems are a sensitivity analysis.
# Dataset: RH-screened (wet-sensor Fluxbot closures removed). The as-deployed dataset (all
# conditions) is a sensitivity analysis.
main_flux <- "best.flux"
main_qc <- "screened"
analysis_start <- as.POSIXct("2023-10-02 00:00:00", tz = "America/New_York")
analysis_end   <- as.POSIXct("2023-11-01 00:00:00", tz = "America/New_York")   # autochamber record ends 31 Oct
# Data-hygiene flags for one flux model (flux_col). Used by 1_clean/03_clean_datasets.R to write
# data_clean/closures_*.csv, and by the loaders below (flags are recomputed for the flux model asked for).
flag_closures <- function(x, flux_col = main_flux) {
  x %>%
    mutate(start_timestamp = start_local, hour_of_obs = round_hour(start_local),
           date = as.Date(hour_of_obs, tz = "America/New_York"), hour = hour(hour_of_obs),
           flux = .data[[flux_col]], stand = stand_code, method = system,
           starting_concen = co2_start, short = grepl("nb.obs", quality.check),
           # CO2 falling significantly in a dark soil chamber = chamber failure (lid not
           # sealed or not vented between closures), not uptake. The chamber-failure flags use the
           # linear-fit diagnostics for every flux model, so they do not depend on the model.
           decline = LM.flux < 0 & !grepl("p-value", quality.check),
           # no detectable CO2 accumulation (goFlux: slope not significant). At October fluxes of ~2
           # umol m-2 s-1 a sealed chamber always accumulates CO2, so this is a chamber that did not seal
           no_accum = grepl("p-value", quality.check),
           wet = if ("wet" %in% names(.)) as.logical(wet) else FALSE,
           # lid stuck shut (headspace not venting; flagged in 1_clean/01_fluxes.R): also a chamber failure
           lid_fail = if ("lid_fail" %in% names(.)) as.logical(lid_fail) else FALSE) %>%
    # whole-array failure (autochambers only): the six chambers of an array share one pump, pneumatic
    # system and power supply, and when these fail every chamber stops sealing at once (stand 1 on 5 and
    # 25-30 Oct). An array-hour is invalid when >= 3 of its chambers show a CO2 decline, no accumulation
    # or weak, noisy accumulation (linear-fit R2 < 0.5) in that hour. Individual closures are never
    # removed for a weak fit alone, so low fluxes are kept.
    group_by(stand, hour_of_obs) %>%
    mutate(array_fail = method == "autochamber" &
             n_distinct(id[decline | no_accum | (!is.na(LM.r2) & LM.r2 < 0.5)]) >= 3) %>% ungroup() %>%
    filter(hour_of_obs >= analysis_start, hour_of_obs < analysis_end)
}

# Per-closure data with QC flags. Main runs read the cleaned closure tables (data_clean/); the
# Fluxbot fit-window sensitivity (option afm.fluxbot_file = "fluxbot_fluxes_w56.csv") reads that
# flux file from data_clean/fluxes/.
load_reprocessed <- function(system, flux_col) {
  alt <- getOption("afm.fluxbot_file", "fluxbot_fluxes.csv")
  x <- if (system == "fluxbot" && alt != "fluxbot_fluxes.csv") read.csv(file.path(flux_dir, alt), colClasses = c(id = "character")) else
    read_clean(paste0("closures_", system, ".csv"), cols(.default = col_guess(), id = "c", UniqueID = "c", start_local = "c", start_utc = "c",
                                                          quality.check = "c", model = "c", hour_of_obs = "c", qc_status = "c")) %>% as.data.frame()
  flag_closures(x, flux_col)
}

# data_clean/ files are written with readr::write_csv and read with readr::read_csv, which round-trip
# doubles exactly (base read.csv does not on all platforms)
read_clean <- function(file, col_types) {
  x <- readr::read_csv(file.path(clean_dir, file), col_types = col_types, na = "NA", guess_max = Inf, lazy = FALSE, progress = FALSE)
  attr(x, "spec") <- NULL; attr(x, "problems") <- NULL
  x
}

load_fluxbot <- function(flux_col = main_flux) {
  load_reprocessed("fluxbot", flux_col) %>% mutate(id = paste0("fluxes_bot", id))
}

load_autochamber <- function(flux_col = main_flux) {
  load_reprocessed("autochamber", flux_col)
}

# ---- QC ------------------------------------------------------------------------
# Two nested datasets are compared with the autochambers throughout:
# qc = "deployed" : "as deployed" (all conditions; sensitivity analysis). Drop closures goFlux flags as too
#                   short (nb.obs) and chamber failures: a statistically significant CO2 decline, no
#                   significant CO2 accumulation (chamber not sealed), a stuck lid, or (autochambers) a whole-array failure (open-lid CO2
#                   > 500 ppm above the other units through a saturated episode; 1_clean/01_fluxes.R). Then
#                   per-chamber robust fences (median +/- 5 MAD) remove isolated spikes. Wet-sensor
#                   closures are kept. No value-based trimming of the pooled data.
# qc = "screened" : MAIN, "RH-screened". The deployed dataset minus Fluxbot closures with in-chamber
#                   RH >= 99% in the open-lid minute (wet K30; Pan et al. 2024). A strict subset.
# qc = "valid" / "computed": intermediate stages (filtering flow).
# qc = "iqr"  : alternative QC. Drop negative fluxes, then Tukey 1.5 x IQR fences on the
#               pooled fluxes of each system.
# qc = "none" : drop negative fluxes only.
# qc = "mad"  : drop negatives, then per-chamber median +/- 5 MAD.
# Old names kept as aliases: "fit_nowet" = "deployed", "fit" = "screened", "dry" = valid minus wet.
apply_qc <- function(d, qc = c("deployed", "screened", "iqr", "none", "mad", "computed", "valid", "dry", "fit", "fit_nowet")) {
  qc <- match.arg(qc)
  if (qc == "fit_nowet") qc <- "deployed"
  if (qc == "fit") qc <- "screened"
  d <- d[!is.na(d$flux), ]
  if (qc == "computed") return(d)                                  # every computable closure
  if (!"lid_fail" %in% names(d)) d$lid_fail <- FALSE
  if (!"wet" %in% names(d)) d$wet <- FALSE
  if (!"no_accum" %in% names(d)) d$no_accum <- FALSE
  if (!"array_fail" %in% names(d)) d$array_fail <- FALSE
  if ("decline" %in% names(d)) d$decline <- d$decline | d$no_accum | d$lid_fail | d$array_fail  # chamber failures
  if (qc == "valid") return(d[!d$short & !d$decline, ])            # chamber failures removed
  if (qc == "dry") return(d[!d$short & !d$decline & !d$wet, ])     # valid minus wet-sensor closures
  if (qc %in% c("deployed", "screened")) {
    if ("short" %in% names(d)) d <- d[!d$short & !d$decline, ]
    d <- d %>% group_by(id) %>% filter(abs(flux - median(flux)) <= 5 * mad(flux)) %>% ungroup()
    if (qc == "screened") d <- d[!d$wet, ]
    return(d)
  }
  d <- d[d$flux >= 0, ]
  if (qc == "iqr") {
    d <- filter_iqr(d, "flux")
  } else if (qc == "mad") {
    d <- d %>% group_by(id) %>%
      filter(abs(flux - median(flux)) <= 5 * mad(flux)) %>% ungroup()
  }
  d
}

# ---- build analysis dataset ----------------------------------------------------
# Returns one row per chamber-hour (before the stand
# relabelling), with column flux_umolm2sec holding the chosen flux.
# The two datasets (RH-screened = main, as deployed; main flux model) are read from data_clean/, where
# 1_clean/03_clean_datasets.R wrote them with this same function (from_clean = FALSE). Every other
# combination (sensitivity analyses) is built on the fly from the cleaned closure tables.
build_dataset <- function(qc = main_qc, flux_col = main_flux, met = load_met(), from_clean = TRUE) {
  if (from_clean && qc %in% c("deployed", "screened") && flux_col == main_flux && missing(met) &&
      getOption("afm.fluxbot_file", "fluxbot_fluxes.csv") == "fluxbot_fluxes.csv") return(load_chamber_hours(qc))
  fb <- apply_qc(load_fluxbot(flux_col), qc)
  ac <- apply_qc(load_autochamber(flux_col), qc)
  assemble_dataset(fb, ac, met)
}

# chamber-hour analysis dataset written by 1_clean/03_clean_datasets.R, with column types restored
load_chamber_hours <- function(qc = main_qc) {
  read_clean(paste0("chamber_hours_", qc, ".csv"),
             cols(id = "c", hour_of_obs = "c", stand = "c", method = "c", day_of_year = "d", hour = "i", flux_umolm2sec = "d",
                  s10t = "d", bar = "d", air_t = "d", precip = "d", stand_label = "c")) %>%
    mutate(hour_of_obs = as.POSIXct(hour_of_obs, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC") %>% with_tz("America/New_York"),
           id = factor(id), method = factor(method, levels = c("autochamber", "fluxbot")),
           stand = factor(stand, levels = c("healthy", "unhealthy")))
}

# autochamber fluxes as published by the Harvard Forest team (HF293-07; best 1-min window,
# visually checked, fixed P and T), with the reprocessed Fluxbot fluxes. Reference only.
load_hf293 <- function() {
  read.csv(file.path(pkg, "ancillary", "hf293-07-soil-resp-2023.csv")) %>%
    mutate(time = as.POSIXct(datetime, format = "%Y-%m-%dT%H:%M", tz = "Etc/GMT+5"),
           hour_of_obs = round_hour(time), date = as.Date(hour_of_obs, tz = "America/New_York"),
           hour = hour(hour_of_obs), id = as.character(chamber), flux = rs,
           stand = if_else(chamber <= 6, "unhealthy", "healthy"), method = "autochamber") %>%
    filter(!is.na(flux), hour_of_obs >= analysis_start, hour_of_obs < analysis_end)
}
build_dataset_hf293 <- function(met = load_met(), flux_col = main_flux) {
  fb <- apply_qc(load_fluxbot(flux_col), main_qc)
  ac <- load_hf293() %>% group_by(id) %>% filter(abs(flux - median(flux)) <= 5 * mad(flux)) %>% ungroup()
  assemble_dataset(fb, ac, met)
}

assemble_dataset <- function(fb, ac, met) {

  # autochambers measure twice per hour; average to one value per chamber-hour
  ac <- ac %>%
    group_by(id, hour_of_obs, date, hour, stand, method) %>%
    summarise(flux = mean(flux), .groups = "drop")

  merged <- bind_rows(
    fb %>% select(id, hour_of_obs, date, hour, flux, stand, method),
    ac %>% select(id, hour_of_obs, date, hour, flux, stand, method)
  ) %>% mutate(day_of_year = yday(hour_of_obs))

  # join met records within +/- 60 min of the flux hour and average them
  out <- difference_left_join(merged, met, by = c("hour_of_obs" = "Time"),
                              max_dist = as.difftime(60, units = "mins")) %>%
    reframe(s10t = mean(s10t), bar = mean(bar), air_t = mean(airt), precip = mean(prec),
            .by = c(id, hour_of_obs, stand, method, day_of_year, hour, flux)) %>%
    mutate(id = case_when(grepl("^\\d+$", id) ~ paste0("autochamber", id),
                          grepl("^fluxes_bot", id) ~ gsub("fluxes_bot", "fluxbot", id),
                          TRUE ~ id),
           id = factor(id),
           method = factor(method, levels = c("autochamber", "fluxbot")),
           stand = factor(stand, levels = c("healthy", "unhealthy")),
           stand_label = if_else(stand == "healthy", "stand 1", "stand 2")) %>%
    rename(flux_umolm2sec = flux)
  out
}

# ---- helpers used by several scripts -------------------------------------------
# hours where both systems have >= k chambers reporting in both stands (compared hours)
# compared hours: >= k units of each system reporting in each stand (k = 3 throughout, except the
# threshold-sensitivity analysis in 2_analysis/04_agreement_metrics.R)
matched_hours <- function(d, k = 3) {
  d %>% count(hour_of_obs, method, stand) %>%
    complete(hour_of_obs, method, stand, fill = list(n = 0)) %>%
    group_by(hour_of_obs) %>% filter(all(n >= k)) %>% ungroup() %>%
    distinct(hour_of_obs) %>% pull(hour_of_obs)
}

script_name <- function() {   # path of the running script, e.g. "2_analysis/02_main_analyses.R"
  a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(a)) sub("^--file=", "", a[1]) else "interactive"
}
numbers <- new.env()
numbers$rows <- list()
record <- function(key, value, section = "", note = "") {
  value <- unname(value)
  if (length(value) != 1) stop("record(): '", key, "' has length ", length(value))
  numbers$rows[[length(numbers$rows) + 1]] <-
    data.frame(key = key, value = signif(value, 5), section = section, note = note, script = script_name())
  invisible(value)
}
# one numbers file per script, named after it (e.g. outputs/numbers/2_analysis__02_main_analyses.csv)
write_numbers <- function(file = NULL) {
  tab <- do.call(rbind, numbers$rows)
  if (is.null(file)) file <- paste0(sub("\\.R$", "", gsub("/", "__", script_name())), ".csv")
  write.csv(tab, file.path(num_dir, file), row.names = FALSE)
  tab
}
# a number recorded by an earlier script (used by figure scripts for labels)
get_number <- function(key) {
  v <- unlist(lapply(setdiff(list.files(num_dir, full.names = TRUE), file.path(num_dir, "numbers_all.csv")), function(f) { x <- read.csv(f); x$value[x$key == key] }))
  if (length(v) != 1) stop("get_number(): '", key, "' found ", length(v), " times")
  v
}
