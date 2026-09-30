# How much do the Fluxbot's lower sampling rate (~6 s vs 1 Hz) and noisier sensor (K30)
# matter? Emulation on the autochamber records: each 1 Hz closure is (a) used as is,
# (b) thinned to one record every 6 s, and (c) thinned and given extra Gaussian noise so
# that its residual SD matches the Fluxbot's empirical precision. Fluxes are recomputed
# with the same goFlux call as 10_fluxes.R and compared with (a).

source("afm_revision/00_prep.R")
suppressPackageStartupMessages({ library(readr); library(goFlux) })
set.seed(20260930)
cores <- max(1, parallel::detectCores() - 2)
meta <- read.csv(file.path(flux_dir, "flux_run_metadata.csv"))
prec_fb <- as.numeric(meta$value[meta$key == "precision_fluxbot_ppm"])
prec_ac <- as.numeric(meta$value[meta$key == "precision_autochamber_ppm"])
chambers <- read_csv(file.path(pkg, "metadata", "autochamber_chambers.csv"), show_col_types = FALSE)
flx <- read.csv(file.path(flux_dir, "autochamber_fluxes.csv"))
ids <- flx$UniqueID

ac <- read_csv(file.path(pkg, "raw", "autochamber_co2_1hz_oct2023.csv.gz"),
               col_types = cols(datetime_est = col_character(), chamber = col_integer(), co2_ppm = col_double())) %>%
  filter(chamber %in% 1:12, !is.na(co2_ppm), co2_ppm > 0, co2_ppm < 10000) %>%
  left_join(chambers %>% select(chamber, total_volume_cm3, collar_area_cm2, slot_minute), by = "chamber") %>%
  mutate(time = as.POSIXct(datetime_est, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT+5"),
         sec = ((minute(time) %% 30) - slot_minute) * 60 + second(time)) %>%
  filter(sec >= 75, sec < 295) %>%
  mutate(slot_start = floor_date(time, "30 mins") + slot_minute * 60,
         UniqueID = paste0("AC_", chamber, "_", format(slot_start, "%Y%m%d%H%M", tz = "UTC")), Etime = sec - 75) %>%
  filter(UniqueID %in% ids) %>%
  left_join(flx %>% select(UniqueID, Tcham, Pcham_kPa), by = "UniqueID") %>%
  transmute(UniqueID, POSIX.time = time, Etime, flag = 1, CO2dry_ppm = co2_ppm, Vtot = total_volume_cm3 / 1000,
            Area = collar_area_cm2, Pcham = Pcham_kPa, Tcham)

run <- function(d, prec, wl) {
  u <- unique(d$UniqueID); ch <- split(u, cut(seq_along(u), cores * 4, labels = FALSE))
  bind_rows(parallel::mclapply(ch, function(k) {
    x <- as.data.frame(d[d$UniqueID %in% k, ])
    invisible(capture.output(f <- suppressWarnings(suppressMessages(goFlux(x, gastype = "CO2dry_ppm", H2O_col = NULL, prec = prec, warn.length = wl)))))
    suppressWarnings(suppressMessages(best.flux(f, warn.length = wl)))
  }, mc.cores = cores)) %>% select(UniqueID, LM.flux, LM.SE, HM.flux, best.flux, model, MDF)
}
phase <- ac %>% distinct(UniqueID) %>% mutate(ph = sample(0:5, n(), replace = TRUE))
thin <- ac %>% left_join(phase, by = "UniqueID") %>% filter((Etime - ph) %% 6 == 0) %>% select(-ph)
noisy <- thin %>% mutate(CO2dry_ppm = CO2dry_ppm + rnorm(n(), 0, sqrt(max(prec_fb^2 - prec_ac^2, 0))))
sc <- list(`1 Hz (original)` = run(ac, prec_ac, 120), `6 s` = run(thin, prec_ac, 20), `6 s + K30 noise` = run(noisy, prec_fb, 20))
cmp <- bind_rows(lapply(names(sc)[-1], function(k) {
  j <- inner_join(sc[[1]], sc[[k]], by = "UniqueID", suffix = c("_ref", "_x")) %>% filter(LM.flux_ref > 0.3)
  tibble(scenario = k, n = nrow(j),
         LM_ratio_median = median(j$LM.flux_x / j$LM.flux_ref), LM_ratio_cv = sd(j$LM.flux_x / j$LM.flux_ref) / mean(j$LM.flux_x / j$LM.flux_ref),
         LM_SE_ratio = median(j$LM.SE_x / j$LM.SE_ref), LM_r = cor(j$LM.flux_ref, j$LM.flux_x),
         best_ratio_median = median(j$best.flux_x / j$best.flux_ref), best_ratio_cv = sd(j$best.flux_x / j$best.flux_ref) / mean(j$best.flux_x / j$best.flux_ref),
         share_HM_ref = mean(j$model_ref == "HM"), share_HM_x = mean(j$model_x == "HM"),
         MDF_ratio = median(j$MDF_x / j$MDF_ref))
}))
print(cmp, width = 200)
write.csv(cmp, file.path(out_dir, "sampling_rate_emulation.csv"), row.names = FALSE)
for (i in seq_len(nrow(cmp))) { tag <- c("thin", "thin_noise")[i]
  for (k in setdiff(names(cmp), "scenario")) record(paste0("srate_", tag, "_", k), cmp[[k]][i], "sampling_rate", cmp$scenario[i]) }

# effect on array-level agreement: emulated-Fluxbot autochambers vs original autochambers
flx_t <- flx %>% select(UniqueID, id, stand_code, start_local)
arr <- function(s) s %>% inner_join(flx_t, by = "UniqueID") %>%
  mutate(hour_of_obs = round_hour(start_local)) %>% group_by(id, stand_code, hour_of_obs) %>% summarise(f = mean(LM.flux), .groups = "drop") %>%
  group_by(stand_code, hour_of_obs) %>% filter(n() >= 3) %>% summarise(f = mean(f), .groups = "drop") %>%
  group_by(hour_of_obs) %>% filter(n() == 2) %>% summarise(f = mean(f))
a0 <- arr(sc[[1]]); a2 <- arr(sc[[3]]); j <- inner_join(a0, a2, by = "hour_of_obs")
record("srate_array_hourly_r_thin_noise_vs_1hz", cor(j$f.x, j$f.y), "sampling_rate", "autochamber array, LM")
record("srate_array_hourly_nrmse_thin_noise_vs_1hz", 100 * sqrt(mean((j$f.y - j$f.x)^2)) / mean(j$f.x), "sampling_rate")
write_numbers("numbers_sampling_rate.csv")
