# Instrument-state diagnostics for the Fluxbots, from the open-lid minute (54:00-55:00)
# recorded before every closure:
#  - baseline anomaly = unit's open-lid CO2 minus the median open-lid CO2 of the other units in
#    the same stand-hour. A large positive anomaly means either the headspace did not vent
#    (lid stuck or partly closed) or a wet K30 reading high (condensation on the optics;
#    Pan et al. 2024, section 4.1).
#  - Tests: how often, when (temperature, RH, rain), and whether affected closures read low
#    relative to the autochambers and account for the Q10 / rain differences.

source("afm_revision/00_prep.R")
suppressPackageStartupMessages({ library(readr); library(lme4) })

units <- read_csv(file.path(pkg, "metadata", "fluxbot_units.csv"), col_types = cols(unit = col_character()))
met <- load_met(doy_range = c(260, 320)) %>% mutate(hour_of_obs = floor_date(with_tz(Time, "America/New_York"), "hour")) %>%
  group_by(hour_of_obs) %>% summarise(airt = mean(airt), s10t = mean(s10t), prec = sum(prec), .groups = "drop") %>%
  arrange(hour_of_obs) %>% mutate(p72 = zoo::rollapply(prec, 72, sum, fill = NA, align = "right", partial = TRUE))
raw <- read_csv(file.path(pkg, "raw", "fluxbot_sensor_records_2023.csv.gz"), col_types = cols(unit = col_character())) %>%
  filter(!co2_ppm %in% c(65535, 65533), co2_ppm > 0, co2_ppm < 10000) %>%
  mutate(time = as.POSIXct(unix_time, origin = "1970-01-01", tz = "America/New_York"), mm = minute(time) + second(time) / 60,
         rh_pct = if_else(rh_pct >= 0 & rh_pct <= 100.5, rh_pct, NA_real_),
         air_temp_c = if_else(air_temp_c > -10 & air_temp_c < 45, air_temp_c, NA_real_)) %>%
  filter(mm >= 54, time >= as.POSIXct("2023-10-02", tz = "America/New_York"), time < as.POSIXct("2023-11-05", tz = "America/New_York")) %>%
  mutate(hour_of_obs = floor_date(time, "hour") + 3600) %>% left_join(units %>% select(unit, stand = stand_code), by = "unit")
cl <- raw %>% group_by(stand, unit, hour_of_obs) %>% filter(sum(mm < 55) >= 3) %>%
  summarise(base = median(co2_ppm[mm < 55]), rh = mean(rh_pct[mm < 55], na.rm = TRUE), tch = mean(air_temp_c[mm < 55], na.rm = TRUE), .groups = "drop") %>%
  group_by(stand, hour_of_obs) %>% filter(n() >= 3) %>%
  mutate(ref = sapply(seq_along(base), function(i) median(base[-i])), anom = base - ref) %>% ungroup() %>%
  left_join(met, by = "hour_of_obs")
saveRDS(cl, file.path(out_dir, "fluxbot_baseline_anomaly.rds"))
record("baseline_ref_median_ppm", median(cl$ref), "vent_wet", "stand-hour median open-lid CO2")
record("baseline_anom_p50", median(cl$anom), "vent_wet"); record("baseline_anom_p90", quantile(cl$anom, .9), "vent_wet")
for (thr in c(50, 100)) record(paste0("baseline_anom_pct_gt", thr), 100 * mean(cl$anom > thr), "vent_wet", "% closures with open-lid CO2 > thr ppm above other units")
hi <- cl %>% mutate(high = anom > 100)
record("anom100_pct_rh_ge99", 100 * mean(hi$high[hi$rh >= 99], na.rm = TRUE), "vent_wet")
record("anom100_pct_rh_lt90", 100 * mean(hi$high[hi$rh < 90], na.rm = TRUE), "vent_wet")
mh <- glm(high ~ I(rh >= 99) + s10t + log1p(p72), family = binomial, data = hi)
for (k in 2:4) { record(paste0("anom100_logOR_", c("", "rh99", "s10t", "p72")[k]), coef(mh)[k], "vent_wet")
                 record(paste0("anom100_p_", c("", "rh99", "s10t", "p72")[k]), summary(mh)$coefficients[k, 4], "vent_wet") }
by_unit <- hi %>% group_by(unit) %>% summarise(pct_high = 100 * mean(high), n = n()) %>% arrange(-pct_high)
write.csv(by_unit, file.path(out_dir, "baseline_anomaly_by_unit.csv"), row.names = FALSE); print(by_unit)

# does a high baseline depress the measured flux? (closure flux relative to the stand-hour autochamber mean)
# diagnostics need the wet-sensor closures, which the main QC removes
d <- build_dataset(qc = "fit_nowet")
ach <- d %>% filter(method == "autochamber") %>% group_by(stand, hour_of_obs) %>% filter(n() >= 3) %>%
  summarise(ac = mean(fluxL_umolm2sec), .groups = "drop") %>% mutate(stand = as.character(stand))
fbx <- d %>% filter(method == "fluxbot", fluxL_umolm2sec > 0.1) %>% mutate(unit = sub("fluxbot", "", as.character(id)), stand = as.character(stand)) %>%
  inner_join(cl %>% select(unit, hour_of_obs, anom, rh, base), by = c("unit", "hour_of_obs")) %>%
  inner_join(ach %>% filter(ac > 0.1), by = c("stand", "hour_of_obs")) %>% mutate(lr = log(fluxL_umolm2sec / ac),
    anom_bin = cut(anom, c(-Inf, 25, 50, 100, 200, Inf), labels = c("<25", "25-50", "50-100", "100-200", ">200")))
bins <- fbx %>% group_by(anom_bin) %>% summarise(n = n(), ratio_median = exp(median(lr)), .groups = "drop")
write.csv(bins, file.path(out_dir, "flux_ratio_by_baseline_anomaly.csv"), row.names = FALSE); print(bins)
mr <- lmer(lr ~ I(anom > 100) + I(rh >= 99) + (1 | unit), data = fbx)
cf <- summary(mr)$coefficients
record("fluxratio_pct_anom100", 100 * (exp(cf[2, 1]) - 1), "vent_wet", "Fluxbot/autochamber ratio when open-lid CO2 > 100 ppm above other units (unit RE)")
record("fluxratio_t_anom100", cf[2, 3], "vent_wet")
record("fluxratio_pct_rh99", 100 * (exp(cf[3, 1]) - 1), "vent_wet"); record("fluxratio_t_rh99", cf[3, 3], "vent_wet")

# Q10 and rain response with and without affected closures
common <- d %>% distinct(stand, hour_of_obs, method) %>% count(stand, hour_of_obs) %>% filter(n == 2)
flag <- cl %>% transmute(id = paste0("fluxbot", unit), hour_of_obs, bad = anom > 100 | rh >= 99)
dq <- d %>% semi_join(common, by = c("stand", "hour_of_obs")) %>% filter(fluxL_umolm2sec > 0.3, !is.na(s10t)) %>%
  mutate(id = as.character(id)) %>% left_join(flag, by = c("id", "hour_of_obs")) %>% mutate(bad = coalesce(bad, FALSE)) %>%
  left_join(met %>% select(hour_of_obs, airt, p72), by = "hour_of_obs")
q10s <- function(x) { cq <- x %>% group_by(method, id) %>% filter(n() >= 100) %>% summarise(b = coef(lm(log(fluxL_umolm2sec) ~ s10t))[2], .groups = "drop")
  c(ac = exp(10 * mean(cq$b[cq$method == "autochamber"])), fb = exp(10 * mean(cq$b[cq$method == "fluxbot"])), p = t.test(b ~ method, cq)$p.value) }
for (sc in list(list("all", dq), list("excl_anom_or_wet", dq %>% filter(!(method == "fluxbot" & bad))))) {
  q <- q10s(sc[[2]]); for (k in names(q)) record(paste0("q10_", sc[[1]], "_", k), q[k], "vent_wet", "chamber-level, HF001 s10t")
  fx <- sc[[2]] %>% filter(method == "fluxbot")
  aics <- sapply(c("s10t", "airt"), function(v) AIC(lmer(as.formula(paste("log(fluxL_umolm2sec) ~", v, "+ (1|id)")), data = fx, REML = FALSE)))
  record(paste0("aic_air_minus_soil_fb_", sc[[1]]), aics["airt"] - aics["s10t"], "vent_wet", "negative = air temperature fits better")
  ax <- sc[[2]] %>% filter(method == "autochamber")
  aics <- sapply(c("s10t", "airt"), function(v) AIC(lmer(as.formula(paste("log(fluxL_umolm2sec) ~", v, "+ (1|id)")), data = ax, REML = FALSE)))
  record(paste0("aic_air_minus_soil_ac_", sc[[1]]), aics["airt"] - aics["s10t"], "vent_wet")
}
print(write_numbers("numbers_vent_wet.csv") %>% select(key, value), row.names = FALSE)
