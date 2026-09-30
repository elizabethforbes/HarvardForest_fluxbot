# A7 (local drivers): compare the Fisher met-station 10 cm soil temperature
# (HF001 s10t, used for all analyses) with the soil temperature logged at each
# autochamber (HF293-07 in data_package/ancillary; `tsoil` = mean of the logger's soil
# probes at that chamber). There is no in-soil sensor on the Fluxbots.

source("afm_revision/00_prep.R")

ts <- read.csv(file.path(pkg, "ancillary", "hf293-07-soil-resp-2023.csv")) %>%
  filter(month %in% 10:11) %>%
  # logger timestamps are EST, the HF convention
  mutate(time = as.POSIXct(datetime, format = "%Y-%m-%dT%H:%M", tz = "Etc/GMT+5"),
         hour_of_obs = floor_date(with_tz(time, "America/New_York"), "hour"),
         stand = if_else(chamber <= 6, "unhealthy", "healthy")) %>%
  filter(!is.na(tsoil))

local_hourly <- ts %>% group_by(stand, hour_of_obs) %>%
  summarise(tsoil_local = mean(tsoil), n_chambers = n_distinct(chamber), .groups = "drop")
met <- load_met() %>% mutate(hour_of_obs = floor_date(with_tz(Time, "America/New_York"), "hour")) %>%
  group_by(hour_of_obs) %>% summarise(s10t = mean(s10t), .groups = "drop")
cmp <- inner_join(local_hourly, met, by = "hour_of_obs") %>% mutate(diff = tsoil_local - s10t)
write.csv(cmp, file.path(out_dir, "soiltemp_local_vs_hf001.csv"), row.names = FALSE)
saveRDS(cmp, file.path(out_dir, "soiltemp_local_vs_hf001.rds"))

for (st in c("healthy", "unhealthy")) {
  x <- cmp %>% filter(stand == st)
  record(paste0("tsoil_local_mean_", st), mean(x$tsoil_local), "A7")
  record(paste0("tsoil_local_min_", st), min(x$tsoil_local), "A7")
  record(paste0("tsoil_local_max_", st), max(x$tsoil_local), "A7")
  record(paste0("tsoil_minus_s10t_mean_", st), mean(x$diff), "A7")
  record(paste0("tsoil_minus_s10t_sd_", st), sd(x$diff), "A7")
  record(paste0("tsoil_vs_s10t_r_", st), cor(x$tsoil_local, x$s10t), "A7")
  record(paste0("tsoil_local_range_", st), max(x$tsoil_local) - min(x$tsoil_local), "A7")
}
record("s10t_range_same_hours", max(cmp$s10t) - min(cmp$s10t), "A7")

# Q10 of both systems with the local (stand-mean autochamber) soil temperature
d <- readRDS(file.path(out_dir, "dataset_main.rds")) %>%
  mutate(hour_of_obs = as.POSIXct(hour_of_obs, tz = "America/New_York"), stand = as.character(stand)) %>%
  inner_join(local_hourly, by = c("stand", "hour_of_obs")) %>% filter(fluxL_umolm2sec > 0)
common <- d %>% distinct(stand, hour_of_obs, method) %>% count(stand, hour_of_obs) %>% filter(n == 2)
dq <- d %>% semi_join(common, by = c("stand", "hour_of_obs"))
for (m in c("autochamber", "fluxbot")) {
  x <- dq %>% filter(method == m)
  fit <- nls(fluxL_umolm2sec ~ a * exp(b * tsoil_local), data = x, start = list(a = 1, b = 0.1))
  b <- unname(coef(fit)["b"]); se <- sqrt(vcov(fit)["b", "b"])
  record(paste0("q10_localT_", m), exp(10 * b), "A7", "common stand-hours, local autochamber soil T")
  record(paste0("q10_localT_", m, "_lo"), exp(10 * (b - 1.96 * se)), "A7")
  record(paste0("q10_localT_", m, "_hi"), exp(10 * (b + 1.96 * se)), "A7")
  fit2 <- nls(fluxL_umolm2sec ~ a * exp(b * s10t), data = x, start = list(a = 1, b = 0.1))
  record(paste0("q10_s10t_samerows_", m), exp(10 * coef(fit2)["b"]), "A7", "same rows, HF001 s10t")
}
record("q10_localT_tmin", min(dq$tsoil_local), "A7"); record("q10_localT_tmax", max(dq$tsoil_local), "A7")
print(write_numbers("numbers_A7_soiltemp.csv"), row.names = FALSE)
