# Shared data preparation for the AFM revision analyses.
# Builds the analysis dataset (fluxes, QC, hourly rounding, met join) from the
# reprocessed fluxes (afm_revision/10_fluxes.R, which reads data_package/ only), or
# from the submitted flux files to reproduce the submitted numbers.
#
# Run scripts from manuscript_code/ (the working directory the .qmd uses).

suppressPackageStartupMessages({
  library(lubridate)
  library(dplyr)
  library(tidyr)
  library(fuzzyjoin)
})

source("filter_iqr.R")

out_dir <- file.path("outputs", "afm_revision")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

pkg <- file.path("..", "data_package")

# ---- met data (Fisher station, HF001) ----------------------------------------
# data_package/ancillary holds the 2023 extract of HF001 (15-min). HF001 timestamps
# are Eastern Standard Time year-round. The submitted .qmd parsed them in the
# machine's local time zone (America/New_York reproduces the submitted numbers).
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
# source = "reprocessed" (default): fluxes recomputed for both systems from the raw
#          records in data_package/ by afm_revision/10_fluxes.R (goFlux).
#          flux_col: "LM.flux" (linear fit; main analysis), "best.flux" (goFlux LM/HM
#          model selection; SI), "HM.flux".
# source = "submitted": the flux files behind the submitted manuscript
#          (flux_col "fluxL_umolm2sec" or "fluxQ_umolm2sec"); used only to reproduce and
#          compare with the submitted numbers.
analysis_start <- as.POSIXct("2023-10-02 00:00:00", tz = "America/New_York")
analysis_end   <- as.POSIXct("2023-11-05 00:00:00", tz = "America/New_York")
flux_dir <- file.path("outputs", "afm_revision", "fluxes")

load_reprocessed <- function(system, flux_col) {
  f <- if (system == "fluxbot") getOption("afm.fluxbot_file", "fluxbot_fluxes.csv") else paste0(system, "_fluxes.csv")
  read.csv(file.path(flux_dir, f), colClasses = c(id = "character")) %>%
    mutate(start_timestamp = start_local, hour_of_obs = round_hour(start_local),
           date = as.Date(hour_of_obs, tz = "America/New_York"), hour = hour(hour_of_obs),
           flux = .data[[flux_col]], stand = stand_code, method = system,
           starting_concen = co2_start, short = grepl("nb.obs", quality.check),
           # CO2 falling significantly in a dark soil chamber = chamber failure (lid not
           # sealed or not vented between closures), not uptake
           decline = .data[[flux_col]] < 0 & !grepl("p-value", quality.check),
           wet = if ("wet" %in% names(.)) as.logical(wet) else FALSE,
           # lid stuck shut (headspace not venting; flagged in 10_fluxes.R): also a chamber failure
           lid_fail = if ("lid_fail" %in% names(.)) as.logical(lid_fail) else FALSE) %>%
    filter(hour_of_obs >= analysis_start, hour_of_obs < analysis_end)
}

load_fluxbot <- function(flux_col = "LM.flux", source = "reprocessed") {
  if (source == "reprocessed") {
    load_reprocessed("fluxbot", flux_col) %>% mutate(id = paste0("fluxes_bot", id))
  } else {
    read.csv("HarvardForest_fluxestimates_fall2023_withstartendconcens.csv") %>%
      mutate(hour_of_obs = round_hour(start_timestamp),
             date = as.Date(hour_of_obs), hour = hour(hour_of_obs),
             flux = .data[[flux_col]], method = "fluxbot") %>%
      select(id, start_timestamp, hour_of_obs, date, hour, flux, stand, method,
             starting_concen, ending_concen, length_interval)
  }
}

load_autochamber <- function(flux_col = "LM.flux", source = "reprocessed") {
  if (source == "reprocessed") {
    load_reprocessed("autochamber", flux_col)
  } else {
    read.csv("HFarray_fluxes_calculatedusingfluxbotcode.csv") %>%
      # chambers 1-6 = Hemlock tower ("unhealthy", stand 2); 7-12 = Bigelow Brook ("healthy", stand 1)
      mutate(stand = if_else(autochamber <= 6, "unhealthy", "healthy"),
             id = as.character(autochamber),
             hour_of_obs = round_hour(start_timestamp),
             date = as.Date(hour_of_obs), hour = hour(hour_of_obs),
             flux = .data[[flux_col]], method = "autochamber") %>%
      select(id, start_timestamp, hour_of_obs, date, hour, flux, stand, method, length_interval)
  }
}

# ---- QC ------------------------------------------------------------------------
# qc = "fit"  : main analysis (reprocessed fluxes). Drop closures goFlux flags as too
#               short (nb.obs), closures with a statistically significant CO2 decline
#               (chamber failure) and Fluxbot closures with in-chamber RH >= 99% in the
#               open-lid minute (wet K30; Pan et al. 2024), then per-chamber robust fences (median +/- 5 MAD) to remove
#               isolated spikes. Non-significant negative values are kept (noise around zero);
#               no value-based trimming of the pooled data.
# qc = "iqr"  : submitted QC. Drop negative fluxes, then Tukey 1.5 x IQR fences on the
#               pooled fluxes of each system.
# qc = "none" : drop negative fluxes only.
# qc = "mad"  : drop negatives, then per-chamber median +/- 5 MAD.
# qc = "computed" / "valid": intermediate stages of the "fit" rule (for the filtering flow).
apply_qc <- function(d, qc = c("fit", "iqr", "none", "mad", "computed", "valid", "dry", "fit_nowet")) {
  qc <- match.arg(qc)
  d <- d[!is.na(d$flux), ]
  if (qc == "computed") return(d)                                  # every computable closure
  if (!"lid_fail" %in% names(d)) d$lid_fail <- FALSE
  if ("decline" %in% names(d)) d$decline <- d$decline | d$lid_fail  # chamber failures: CO2 decline or stuck lid
  if (qc == "valid") return(d[!d$short & !d$decline, ])            # chamber failures removed
  if (qc == "dry") return(d[!d$short & !d$decline & !d$wet, ])     # + wet-sensor closures removed
  if (qc == "fit_nowet") {                                          # sensitivity: keep wet-sensor closures
    d <- d[!d$short & !d$decline, ]
    return(d %>% group_by(id) %>% filter(abs(flux - median(flux)) <= 5 * mad(flux)) %>% ungroup())
  }
  if (qc == "fit") {
    if ("short" %in% names(d)) d <- d[!d$short & !d$decline & !d$wet, ]
    return(d %>% group_by(id) %>% filter(abs(flux - median(flux)) <= 5 * mad(flux)) %>% ungroup())
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
# Returns the equivalent of `merged_data_with_met` in the .qmd (before the stand
# relabelling), with column fluxL_umolm2sec holding the chosen flux.
build_dataset <- function(qc = "fit", flux_col = "LM.flux", source = "reprocessed", met = load_met()) {
  fb <- apply_qc(load_fluxbot(flux_col, source), qc)
  ac <- apply_qc(load_autochamber(flux_col, source), qc)
  assemble_dataset(fb, ac, met)
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
build_dataset_hf293 <- function(met = load_met(), flux_col = "LM.flux") {
  fb <- apply_qc(load_fluxbot(flux_col), "fit")
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

  # join met records within +/- 60 min of the flux hour and average them (as in the .qmd)
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
    rename(fluxL_umolm2sec = flux)
  out
}

# ---- helpers used by several scripts -------------------------------------------
# hours where both systems have >= k chambers reporting in both stands (Fig 4/5 subset)
matched_hours <- function(d, k = 5) {
  d %>% count(hour_of_obs, method, stand) %>%
    complete(hour_of_obs, method, stand, fill = list(n = 0)) %>%
    group_by(hour_of_obs) %>% filter(all(n >= k)) %>% ungroup() %>%
    distinct(hour_of_obs) %>% pull(hour_of_obs)
}

numbers <- new.env()
numbers$rows <- list()
record <- function(key, value, section = "", note = "") {
  value <- unname(value)
  if (length(value) != 1) stop("record(): '", key, "' has length ", length(value))
  numbers$rows[[length(numbers$rows) + 1]] <-
    data.frame(key = key, value = signif(value, 5), section = section, note = note)
  invisible(value)
}
write_numbers <- function(file) {
  tab <- do.call(rbind, numbers$rows)
  write.csv(tab, file.path(out_dir, file), row.names = FALSE)
  tab
}
