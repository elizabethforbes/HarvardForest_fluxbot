# Flux calculation for BOTH systems from the raw records in data_package/, with one
# shared procedure (goFlux 0.4.0: linear model, Hutchinson-Mosier non-linear model and
# best.flux model selection; Rheault et al. 2024).
#
# Shared rules
#  - Raw rows with CO2 error codes (65535/65533) or outside 0-10000 ppm are removed row-wise
#    (all variables together), before any calculation.
#  - Time: Fluxbot device timestamps are UNIX (absolute); autochamber logger clocks are EST
#    (Harvard Forest convention, confirmed by matching to HF293). All output times are local
#    (America/New_York).
#  - Pressure: station pressure at the stand = hourly median of the Fluxbot LPS22 sensors in
#    that stand (sensors within 5 hPa of the stand median; 965-1050 hPa), used for both
#    systems. Where unavailable: HF001 sea-level pressure x the median local/HF001 ratio.
#  - Temperature: Fluxbot in-chamber SHT-30 mean over the window (read-failure values and
#    readings outside -10..45 C removed); autochambers HF001 air temperature (no chamber sensor).
#  - No water-vapour correction for either system (autochamber raw records carry no H2O).
#  - Windows: Fluxbot lid closes at 55:00 (firmware); the headspace is mixed by diffusion only,
#    so 56:00-60:00 is used (60 s dead band). Autochamber lid closes 45 s into each 5-min slot
#    and the CO2 rise reaches the analyzer at ~63 s; 75-295 s into the slot is used.
#  - Instrument precision (for MDF and the HM curvature limit) is estimated empirically for
#    each system as the median robust residual SD of the linear fits.
#
# Output: outputs/afm_revision/fluxes/{fluxbot,autochamber}_fluxes.csv (one row per closure)

suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(readr); library(lubridate); library(goFlux) })
pkg <- file.path("..", "data_package")
out_dir <- file.path("outputs", "afm_revision", "fluxes"); dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
cores <- max(1, parallel::detectCores() - 2)
test_n <- as.integer(Sys.getenv("FLUX_TEST_N", "0"))   # >0: process only the first n closures per system

met <- read_csv(file.path(pkg, "ancillary", "hf001-10-15min-m_2023.csv"), show_col_types = FALSE) %>%
  transmute(time = as.POSIXct(datetime, format = "%Y-%m-%dT%H:%M", tz = "Etc/GMT+5"), airt = as.numeric(airt), bar = as.numeric(bar)) %>%
  filter(!is.na(time))
met_at <- function(t, var) approx(as.numeric(met$time), met[[var]], xout = as.numeric(t), rule = 2)$y

units <- read_csv(file.path(pkg, "metadata", "fluxbot_units.csv"), show_col_types = FALSE, col_types = cols(unit = col_character()))
chambers <- read_csv(file.path(pkg, "metadata", "autochamber_chambers.csv"), show_col_types = FALSE)

# ---- Fluxbot raw ------------------------------------------------------------------------------
fbr <- read_csv(file.path(pkg, "raw", "fluxbot_sensor_records_2023.csv.gz"), show_col_types = FALSE,
                col_types = cols(unit = col_character())) %>%
  mutate(time = as.POSIXct(unix_time, origin = "1970-01-01", tz = "UTC")) %>%
  left_join(units %>% select(unit, stand_code), by = "unit") %>%
  filter(!is.na(co2_ppm), !co2_ppm %in% c(65535, 65533), co2_ppm > 0, co2_ppm < 10000) %>%
  mutate(air_temp_c = if_else(air_temp_c > -10 & air_temp_c < 45, air_temp_c, NA_real_),
         pressure_hpa = if_else(pressure_hpa > 965 & pressure_hpa < 1050, pressure_hpa, NA_real_))

# station pressure per stand-hour from the LPS22 sensors
ph <- fbr %>% filter(!is.na(pressure_hpa)) %>% mutate(hr = floor_date(time, "hour")) %>%
  group_by(stand_code, unit, hr) %>% summarise(p = median(pressure_hpa), .groups = "drop") %>%
  group_by(stand_code, hr) %>% mutate(dev = p - median(p)) %>% ungroup()
bad_p <- ph %>% group_by(unit) %>% summarise(d = median(dev)) %>% filter(abs(d) > 5) %>% pull(unit)
ph <- ph %>% filter(!unit %in% bad_p) %>% group_by(stand_code, hr) %>% summarise(p_local = median(p), .groups = "drop")
p_ratio <- median(ph$p_local / met_at(ph$hr, "bar"), na.rm = TRUE)
p_at <- function(stand, t) {
  x <- tibble(stand_code = stand, hr = floor_date(t, "hour")) %>% left_join(ph, by = c("stand_code", "hr"))
  coalesce(x$p_local, met_at(t, "bar") * p_ratio)
}

# Fluxbot closures: window [HH:56:00, HH+1:00:00) in UTC-aligned clock (minutes are tz-independent)
fb <- fbr %>% mutate(wstart = floor_date(time, "hour") + 56 * 60) %>%
  filter(time >= wstart, time < wstart + 240) %>%
  mutate(UniqueID = paste0("FB_", unit, "_", format(wstart, "%Y%m%d%H%M", tz = "UTC")),
         Etime = as.numeric(difftime(time, wstart, units = "secs"))) %>%
  group_by(UniqueID) %>% filter(n() >= 20, max(Etime) - min(Etime) >= 180) %>%
  mutate(Tcham = { tt <- mean(air_temp_c, na.rm = TRUE); if (is.finite(tt)) tt else met_at(first(time), "airt") }) %>%
  ungroup() %>%
  mutate(Pcham = p_at(stand_code, wstart) / 10, Vtot = 0.768, Area = 81, flag = 1,
         CO2dry_ppm = co2_ppm, POSIX.time = time)
record_meta <- list(fluxbot_bad_pressure_units = paste(bad_p, collapse = ";"), pressure_ratio_local_hf001 = p_ratio)

# ---- Autochamber raw ---------------------------------------------------------------------------
acr <- read_csv(file.path(pkg, "raw", "autochamber_co2_1hz_oct2023.csv.gz"),
                col_types = cols(datetime_est = col_character(), chamber = col_integer(), co2_ppm = col_double())) %>%
  filter(chamber %in% 1:12, !is.na(co2_ppm), co2_ppm > 0, co2_ppm < 10000) %>%
  mutate(time = as.POSIXct(datetime_est, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT+5")) %>%
  left_join(chambers %>% select(chamber, stand_code, total_volume_cm3, collar_area_cm2, slot_minute), by = "chamber")
ac <- acr %>% mutate(m = minute(time), s = second(time),
                     sec_in_half = ((m %% 30) - slot_minute) * 60 + s) %>%          # seconds since this chamber's slot start
  filter(sec_in_half >= 75, sec_in_half < 295) %>%
  mutate(slot_start = floor_date(time, "30 mins") + slot_minute * 60,
         UniqueID = paste0("AC_", chamber, "_", format(slot_start, "%Y%m%d%H%M", tz = "UTC")),
         Etime = sec_in_half - 75) %>%
  group_by(UniqueID) %>% filter(n() >= 120) %>% ungroup() %>%
  mutate(Tcham = met_at(time, "airt"), Pcham = p_at(stand_code, time) / 10,
         Vtot = total_volume_cm3 / 1000, Area = collar_area_cm2, flag = 1, CO2dry_ppm = co2_ppm, POSIX.time = time) %>%
  group_by(UniqueID) %>% mutate(Tcham = mean(Tcham), Pcham = mean(Pcham)) %>% ungroup()

# ---- empirical precision ----------------------------------------------------------------------------
robust_sigma <- function(d) {
  ids <- sample(unique(d$UniqueID), min(2000, n_distinct(d$UniqueID)))
  median(sapply(split(d[d$UniqueID %in% ids, ], d$UniqueID[d$UniqueID %in% ids]), function(x)
    if (nrow(x) > 10) mad(residuals(lm(CO2dry_ppm ~ Etime, data = x))) else NA), na.rm = TRUE)
}
set.seed(1)
prec_fb <- robust_sigma(fb); prec_ac <- robust_sigma(ac)
message(sprintf("precision: Fluxbot %.2f ppm, autochamber %.2f ppm", prec_fb, prec_ac))

# ---- goFlux in parallel ------------------------------------------------------------------------------
run_goflux <- function(d, prec, warn_length) {
  ids <- unique(d$UniqueID); if (test_n > 0) ids <- head(ids, test_n)
  chunks <- split(ids, cut(seq_along(ids), min(length(ids), cores * 4), labels = FALSE))
  res <- parallel::mclapply(chunks, function(ch) {
    x <- d %>% filter(UniqueID %in% ch) %>% select(UniqueID, POSIX.time, Etime, flag, CO2dry_ppm, Vtot, Area, Pcham, Tcham) %>%
      as.data.frame()
    invisible(capture.output(f <- suppressWarnings(suppressMessages(
      goFlux(x, gastype = "CO2dry_ppm", H2O_col = NULL, prec = prec, warn.length = warn_length)))))
    suppressWarnings(suppressMessages(best.flux(f, warn.length = warn_length)))
  }, mc.cores = cores)
  bind_rows(res)
}
summ <- function(d) d %>% group_by(UniqueID) %>%
  summarise(start_utc = min(POSIX.time), n_obs = n(), co2_start = first(CO2dry_ppm), Tcham = first(Tcham),
            Pcham_kPa = first(Pcham), Vtot_L = first(Vtot), Area_cm2 = first(Area), .groups = "drop")

t0 <- Sys.time()
gf_fb <- run_goflux(fb, prec_fb, warn_length = 20)
message("Fluxbot goFlux: ", nrow(gf_fb), " closures, ", round(difftime(Sys.time(), t0, units = "mins"), 1), " min")
t0 <- Sys.time()
gf_ac <- run_goflux(ac, prec_ac, warn_length = 120)
message("Autochamber goFlux: ", nrow(gf_ac), " closures, ", round(difftime(Sys.time(), t0, units = "mins"), 1), " min")

keep <- c("UniqueID", "LM.flux", "LM.SE", "LM.r2", "HM.flux", "HM.SE", "HM.r2", "HM.k", "k.max", "g.fact",
          "MDF", "best.flux", "model", "quality.check")
tidy_out <- function(gf, d, system) {
  gf %>% select(any_of(keep)) %>% left_join(summ(d), by = "UniqueID") %>%
    mutate(system = system, id = sub("^[A-Z]+_(\\d+)_.*$", "\\1", UniqueID),
           start_local = format(with_tz(start_utc, "America/New_York"), "%Y-%m-%d %H:%M:%S"),
           curvature = HM.flux / LM.flux) %>%
    relocate(system, id, UniqueID, start_local)
}
fb_out <- tidy_out(gf_fb, fb, "fluxbot") %>% left_join(units %>% select(id = unit, stand_code), by = "id")
ac_out <- tidy_out(gf_ac, ac, "autochamber") %>%
  left_join(chambers %>% transmute(id = as.character(chamber), stand_code), by = "id")
sfx <- if (test_n > 0) "_test" else ""
write_csv(fb_out, file.path(out_dir, paste0("fluxbot_fluxes", sfx, ".csv")))
write_csv(ac_out, file.path(out_dir, paste0("autochamber_fluxes", sfx, ".csv")))
write_csv(tibble(key = c("precision_fluxbot_ppm", "precision_autochamber_ppm", names(record_meta)),
                 value = c(prec_fb, prec_ac, unlist(record_meta))), file.path(out_dir, paste0("flux_run_metadata", sfx, ".csv")))
message("Done.")
