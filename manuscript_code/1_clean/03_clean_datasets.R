# Step 1c. Data hygiene: from per-closure fluxes (data_clean/fluxes/) to the cleaned datasets used by
# every analysis. Writes, to data_clean/:
#   closures_fluxbot.csv, closures_autochamber.csv
#       every closure in the analysis period (2-31 Oct 2023), all flux-calculation columns, the QC
#       flags, and qc_status = the first rule that removed it ("retained" if none), for the main
#       (linear) flux model. in_deployed / in_screened mark the two datasets compared.
#   chamber_hours_deployed.csv, chamber_hours_screened.csv
#       the analysis datasets: one row per chamber-hour (autochamber closures averaged within the
#       hour), with HF001 met data joined (+/- 60 min mean). "deployed" = main dataset (as deployed,
#       all conditions); "screened" = RH-screened subset (wet-sensor Fluxbot closures removed).
#   qc_log.csv            closures by system and QC status (counts and % of computed closures)
#   data_dictionary.csv   every column of the files above
#
# QC rules, in order (R/setup.R: flag_closures(), apply_qc()):
#   1. no flux computed                     LM flux missing
#   2. too few records                       goFlux nb.obs flag
#   3. chamber failure: stuck lid            open-lid CO2 > 500 ppm above the stand's other units through a
#                                            saturated episode (1_clean/01_fluxes.R)
#   4. chamber failure: no CO2 accumulation  goFlux p-value flag (slope not significant)
#   5. chamber failure: CO2 decline          significant negative slope
#   6. chamber failure: poor linear fit      LM R2 < 0.5
#   7. spike                                 outside the chamber's median +/- 5 MAD (among closures passing 1-6)
#   retained = as-deployed dataset; RH-screened = retained minus Fluxbot closures with RH >= 99% in the
#   open-lid minute (wet K30 sensor).

source("R/setup.R")
suppressPackageStartupMessages(library(readr))

status_closures <- function(system) {
  raw <- read.csv(file.path(flux_dir, paste0(system, "_fluxes.csv")), colClasses = c(id = "character"))
  x <- raw %>% flag_closures("LM.flux") %>%
    mutate(chamber_failure = decline | no_accum | poor_fit | lid_fail,
           pre_spike = !is.na(flux) & !short & !chamber_failure)
  # spike screen among closures passing rules 1-6, per chamber (as apply_qc)
  sp <- x %>% filter(pre_spike) %>% group_by(id) %>% mutate(spike = abs(flux - median(flux)) > 5 * mad(flux)) %>% ungroup() %>%
    select(UniqueID, spike)
  x <- x %>% left_join(sp, by = "UniqueID") %>% mutate(spike = coalesce(spike, FALSE),
    qc_status = case_when(is.na(flux) ~ "no flux computed",
                          short ~ "too few records",
                          lid_fail ~ "chamber failure: stuck lid",
                          no_accum ~ "chamber failure: no CO2 accumulation",
                          decline ~ "chamber failure: CO2 decline",
                          poor_fit ~ "chamber failure: poor linear fit",
                          spike ~ "spike",
                          TRUE ~ "retained"),
    in_deployed = qc_status == "retained", in_screened = in_deployed & !wet)
  # the closure table keeps every column of the flux file plus the QC columns
  out <- x %>% select(all_of(names(raw)), hour_of_obs, short, decline, no_accum, poor_fit, lid_fail, wet, spike, qc_status, in_deployed, in_screened) %>%
    mutate(hour_of_obs = format(with_tz(hour_of_obs, "UTC"), "%Y-%m-%dT%H:%M:%SZ"))
  # check: identical to the QC applied on the fly (apply_qc) to the same closures
  ref <- apply_qc(x %>% select(-spike), "deployed")
  stopifnot(setequal(ref$UniqueID, x$UniqueID[x$in_deployed]))
  ref_s <- apply_qc(x %>% select(-spike), "screened")
  stopifnot(setequal(ref_s$UniqueID, x$UniqueID[x$in_screened]))
  out
}

cl <- list(fluxbot = status_closures("fluxbot"), autochamber = status_closures("autochamber"))
for (s in names(cl)) write_csv(cl[[s]], file.path(clean_dir, paste0("closures_", s, ".csv")))

# QC log
log <- bind_rows(lapply(names(cl), function(s) cl[[s]] %>% count(qc_status) %>% mutate(system = s))) %>%
  mutate(qc_status = factor(qc_status, levels = c("retained", "spike", "chamber failure: poor linear fit", "chamber failure: CO2 decline",
                                                  "chamber failure: no CO2 accumulation", "chamber failure: stuck lid", "too few records", "no flux computed"))) %>%
  arrange(system, qc_status) %>% group_by(system) %>% mutate(pct_of_closures = round(100 * n / sum(n), 2)) %>% ungroup() %>%
  select(system, qc_status, n, pct_of_closures)
wet_log <- tibble(system = "fluxbot", qc_status = "retained, wet sensor (removed in RH-screened subset)",
                  n = sum(cl$fluxbot$in_deployed & !cl$fluxbot$in_screened), pct_of_closures = round(100 * n / nrow(cl$fluxbot), 2))
log <- bind_rows(log %>% mutate(qc_status = as.character(qc_status)), wet_log)
write_csv(log, file.path(clean_dir, "qc_log.csv"))
print(as.data.frame(log))

# chamber-hour analysis datasets, built with the same code the analyses used before (from_clean = FALSE),
# written, read back and checked. Decimal text does not round-trip doubles to the last bit, so the check
# allows a relative difference of 1e-12 (observed: ~1e-15, the last digit of a double).
for (q in c("deployed", "screened")) {
  d <- build_dataset(qc = q, from_clean = FALSE)
  write_csv(d %>% mutate(hour_of_obs = format(with_tz(hour_of_obs, "UTC"), "%Y-%m-%dT%H:%M:%S")),
            file.path(clean_dir, paste0("chamber_hours_", q, ".csv")))
  back <- load_chamber_hours(q)
  chk <- all.equal(as.data.frame(back), as.data.frame(d), check.attributes = TRUE, tolerance = 1e-12)
  if (!isTRUE(chk)) stop("chamber_hours_", q, " does not round-trip: ", paste(chk, collapse = "; "))
  num <- sapply(d, is.numeric)
  message(q, ": ", nrow(d), " chamber-hours; max relative difference after round trip ",
          signif(max(abs(unlist(back[num]) - unlist(d[num])) / pmax(abs(unlist(d[num])), 1e-300), na.rm = TRUE), 2))
}

# data dictionary
dict <- tribble(~file, ~column, ~description,
  "closures_*.csv", "system", "fluxbot or autochamber",
  "closures_*.csv", "id", "Fluxbot unit number or autochamber chamber number",
  "closures_*.csv", "UniqueID", "closure identifier: system, unit and closure start (UTC, YYYYMMDDHHMM)",
  "closures_*.csv", "start_local", "start of the fit window, local time (America/New_York)",
  "closures_*.csv", "start_utc", "start of the fit window, UTC",
  "closures_*.csv", "LM.flux, LM.SE, LM.r2", "linear-model flux (umol m-2 s-1; main analysis), its standard error and R2 (goFlux)",
  "closures_*.csv", "HM.flux, HM.SE, HM.r2, HM.k, k.max, g.fact", "Hutchinson-Mosier flux (umol m-2 s-1), standard error, R2, curvature parameter kappa, its maximum, and g-factor (HM/LM)",
  "closures_*.csv", "MDF", "minimal detectable flux (umol m-2 s-1)",
  "closures_*.csv", "best.flux, model", "goFlux best-model flux and the model chosen (LM or HM; sensitivity analysis)",
  "closures_*.csv", "quality.check", "goFlux quality flags (nb.obs = too few records; p-value = slope not significant; others informational)",
  "closures_*.csv", "n_obs", "CO2 records in the fit window",
  "closures_*.csv", "co2_start", "CO2 at the start of the fit window (ppm)",
  "closures_*.csv", "Tcham", "temperature used in the gas law (C): Fluxbot in-chamber SHT-30; autochamber HF001 air temperature",
  "closures_*.csv", "Pcham_kPa", "station pressure used in the gas law (kPa)",
  "closures_*.csv", "Vtot_L, Area_cm2", "system volume (L) and collar area (cm2)",
  "closures_*.csv", "curvature", "HM/LM flux ratio (g-factor) used as a curvature index",
  "closures_*.csv", "stand_code", "healthy = stand 1 (Bigelow Brook); unhealthy = stand 2 (Hemlock tower)",
  "closures_*.csv", "rh_open", "Fluxbot in-chamber RH in the open-lid minute 54:00-55:00 (%); NA for autochambers",
  "closures_*.csv", "hour_of_obs", "closure hour (start rounded to the nearest hour), UTC; analyses use local time",
  "closures_*.csv", "short", "QC flag: too few records to fit (goFlux nb.obs)",
  "closures_*.csv", "decline", "QC flag: significant CO2 decline (chamber failure)",
  "closures_*.csv", "no_accum", "QC flag: no significant CO2 accumulation (chamber failure)",
  "closures_*.csv", "poor_fit", "QC flag: linear-fit R2 < 0.5 (chamber failure)",
  "closures_*.csv", "lid_fail", "QC flag: Fluxbot lid stuck shut (chamber failure); FALSE for autochambers",
  "closures_*.csv", "wet", "Fluxbot wet-sensor flag: RH >= 99% in the open-lid minute; FALSE for autochambers",
  "closures_*.csv", "spike", "QC flag: outside the chamber's median +/- 5 MAD among closures passing the earlier rules",
  "closures_*.csv", "qc_status", "first QC rule that removed the closure, or retained",
  "closures_*.csv", "in_deployed", "closure is in the main (as-deployed) dataset",
  "closures_*.csv", "in_screened", "closure is in the RH-screened subset",
  "chamber_hours_*.csv", "id", "autochamber1-12 or fluxbot<unit>",
  "chamber_hours_*.csv", "hour_of_obs", "hour, UTC (ISO 8601); analyses use local time (America/New_York)",
  "chamber_hours_*.csv", "stand", "healthy = stand 1; unhealthy = stand 2",
  "chamber_hours_*.csv", "method", "autochamber or fluxbot",
  "chamber_hours_*.csv", "day_of_year", "day of year of hour_of_obs (local)",
  "chamber_hours_*.csv", "hour", "hour of day (local, 0-23)",
  "chamber_hours_*.csv", "fluxL_umolm2sec", "linear-model CO2 flux (umol m-2 s-1); autochambers: mean of the hour's two closures",
  "chamber_hours_*.csv", "s10t", "HF001 soil temperature at 10 cm (C), mean of 15-min records within +/- 60 min",
  "chamber_hours_*.csv", "bar", "HF001 barometric pressure reduced to sea level (mbar), same window",
  "chamber_hours_*.csv", "air_t", "HF001 air temperature (C), same window",
  "chamber_hours_*.csv", "precip", "HF001 precipitation (mm per 15 min), mean over the same window",
  "chamber_hours_*.csv", "stand_label", "stand 1 or stand 2",
  "qc_log.csv", "system, qc_status, n, pct_of_closures", "closures per QC status (analysis period), % of computed closures")
write_csv(dict, file.path(clean_dir, "data_dictionary.csv"))
