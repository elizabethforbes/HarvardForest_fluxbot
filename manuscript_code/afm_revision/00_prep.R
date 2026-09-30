# Shared data preparation for the AFM revision analyses.
# Reproduces the data-preparation chunks of HF_fluxbotautochamber_analysis.qmd
# (flux import, QC, hourly rounding, met-data join) as functions, so every
# downstream script can rebuild the analysis dataset under alternative QC rules.
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

# ---- met data (Fisher station, HF001) ----------------------------------------
# A 2023 extract is cached in data/ so the analysis runs offline; the full file
# is at https://harvardforest.fas.harvard.edu/data/p00/hf001/hf001-10-15min-m.csv
# HF001 timestamps are Eastern Standard Time year-round. The .qmd parsed them in
# the machine's local time zone, so the soil-temperature join depended on where
# it was run (America/New_York reproduces the submitted numbers exactly).
load_met <- function(doy_range = c(273, 308), tz = "Etc/GMT+5") {
  f <- file.path("data", "hf001-10-15min-m_2023.csv")
  met <- if (file.exists(f)) read.csv(f) else
    read.csv("https://harvardforest.fas.harvard.edu/data/p00/hf001/hf001-10-15min-m.csv")
  met %>%
    mutate(doy = yday(datetime), year = year(datetime)) %>%
    filter(year == 2023, doy >= doy_range[1], doy <= doy_range[2]) %>%
    mutate(Time = as.POSIXct(datetime, format = "%Y-%m-%dT%H:%M", tz = tz))
}

round_hour <- function(ts) {
  dt <- as.POSIXct(ts, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York")
  if_else(minute(dt) >= 30, ceiling_date(dt, unit = "hour"), floor_date(dt, unit = "hour"))
}

# ---- raw flux estimates -------------------------------------------------------
# flux_col: "fluxL_umolm2sec" (linear slope; used in the manuscript) or
#           "fluxQ_umolm2sec" (initial slope of the quadratic fit)
load_fluxbot <- function(flux_col = "fluxL_umolm2sec") {
  read.csv("HarvardForest_fluxestimates_fall2023_withstartendconcens.csv") %>%
    mutate(hour_of_obs = round_hour(start_timestamp),
           date = as.Date(hour_of_obs), hour = hour(hour_of_obs),
           flux = .data[[flux_col]], method = "fluxbot") %>%
    select(id, start_timestamp, hour_of_obs, date, hour, flux, stand, method,
           starting_concen, ending_concen, length_interval)
}

load_autochamber <- function(flux_col = "fluxL_umolm2sec") {
  read.csv("HFarray_fluxes_calculatedusingfluxbotcode.csv") %>%
    # chambers 1-6 = Hemlock tower ("unhealthy", stand 2); 7-12 = Bigelow Brook ("healthy", stand 1)
    mutate(stand = if_else(autochamber <= 6, "unhealthy", "healthy"),
           id = as.character(autochamber),
           hour_of_obs = round_hour(start_timestamp),
           date = as.Date(hour_of_obs), hour = hour(hour_of_obs),
           flux = .data[[flux_col]], method = "autochamber") %>%
    select(id, start_timestamp, hour_of_obs, date, hour, flux, stand, method, length_interval)
}

# ---- QC ------------------------------------------------------------------------
# qc = "iqr"  : manuscript QC. Drop negative fluxes, then Tukey 1.5 x IQR fences on
#               the pooled flux values of each system.
# qc = "none" : drop negative fluxes only (no value-based trimming). Intervals were
#               already screened at flux calculation (>= 5 ppm rise, positive
#               slope, >= 15 points, plausible pressure).
# qc = "mad"  : drop negatives, then per-chamber robust fences (median +/- 5 MAD);
#               removes only extreme spikes relative to that chamber's own record.
apply_qc <- function(d, qc = c("iqr", "none", "mad")) {
  qc <- match.arg(qc)
  d <- d[!is.na(d$flux) & d$flux >= 0, ]
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
build_dataset <- function(qc = "iqr", flux_col = "fluxL_umolm2sec", met = load_met()) {
  fb <- apply_qc(load_fluxbot(flux_col), qc)
  ac <- apply_qc(load_autochamber(flux_col), qc)

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
