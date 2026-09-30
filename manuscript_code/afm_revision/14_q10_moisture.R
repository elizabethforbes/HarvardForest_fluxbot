# Why do Q10 values differ between systems, and is there evidence for a moisture effect
# on the Fluxbot NDIR measurement?
#  1. Site expectation: Q10 of the HF-published autochamber fluxes against chamber soil
#     temperature over the 2023 season (Apr-Dec) and in October only.
#  2. Chamber-level Q10 split into a between-day component (daily means) and a within-day
#     component (diel anomalies), per system.
#  3. Confounding: day-of-season trend and antecedent rain added to the chamber-level
#     log-linear model; which temperature (air, 10-cm soil, lagged soil) best explains each system.
#  4. Moisture diagnostics for the Fluxbot K30: in-chamber RH during closures; Fluxbot/
#     autochamber ratio vs RH; CO2 onset delay after lid closure (a wet PTFE membrane or
#     condensation would slow diffusion into the sensor) vs RH and rain.

source("afm_revision/00_prep.R")
suppressPackageStartupMessages({ library(readr); library(lme4) })

met <- load_met(doy_range = c(260, 320)) %>% mutate(hour_of_obs = floor_date(with_tz(Time, "America/New_York"), "hour")) %>%
  group_by(hour_of_obs) %>% summarise(airt = mean(airt), s10t = mean(s10t), rh_hf = mean(rh), prec = sum(prec), .groups = "drop") %>%
  arrange(hour_of_obs) %>% mutate(p72 = zoo::rollapply(prec, 72, sum, fill = NA, align = "right", partial = TRUE),
                                  s10t_lag6 = lag(s10t, 6))

# ---- 1. site expectation ---------------------------------------------------------------------------
hf <- read.csv(file.path(pkg, "ancillary", "hf293-07-soil-resp-2023.csv")) %>% filter(!is.na(rs), !is.na(tsoil), rs > 0.2, rs < 30)
q10_logfit <- function(x) { m <- lmer(log(rs) ~ tsoil + (1 | chamber), data = x); b <- fixef(m)["tsoil"]; se <- sqrt(vcov(m)[2, 2])
  c(q10 = unname(exp(10 * b)), lo = unname(exp(10 * (b - 1.96 * se))), hi = unname(exp(10 * (b + 1.96 * se)))) }
for (per in list(list("season", 4:12), list("october", 10), list("sep_nov", 9:11))) {
  q <- q10_logfit(hf %>% filter(month %in% per[[2]]))
  for (k in names(q)) record(paste0("site_q10_hf293_", per[[1]], "_", k), q[k], "q10_expect", "HF-published autochamber rs vs chamber tsoil, log-linear LMM")
}
record("site_tsoil_range_season_lo", min(hf$tsoil), "q10_expect"); record("site_tsoil_range_season_hi", max(hf$tsoil), "q10_expect")

# ---- 2 & 3. chamber-level Q10 components and confounders -------------------------------------------------
d <- readRDS(file.path(out_dir, "dataset_main.rds")) %>% select(-s10t) %>%
  left_join(met, by = "hour_of_obs") %>% filter(!is.na(s10t), fluxL_umolm2sec > 0.3)
common <- d %>% distinct(stand, hour_of_obs, method) %>% count(stand, hour_of_obs) %>% filter(n == 2)
dq <- d %>% semi_join(common, by = c("stand", "hour_of_obs")) %>% mutate(lf = log(fluxL_umolm2sec),
      day = as.Date(hour_of_obs, tz = "America/New_York"), doy = yday(hour_of_obs))
comp <- dq %>% group_by(method, id) %>% filter(n() >= 100) %>% group_by(method, id, day) %>%
  mutate(lf_anom = lf - mean(lf), t_anom = s10t - mean(s10t), ta_anom = airt - mean(airt)) %>% ungroup()
slopes <- function(x) {
  dm <- x %>% group_by(day) %>% filter(n() >= 12) %>% summarise(l = mean(lf), t = mean(s10t), .groups = "drop")
  tibble(method = x$method[1], id = x$id[1],
         b_total = coef(lm(lf ~ s10t, x))[2],
         b_between = if (nrow(dm) >= 5) coef(lm(l ~ t, dm))[2] else NA_real_,
         b_within = coef(lm(lf_anom ~ t_anom, x))[2],
         b_within_air = coef(lm(lf_anom ~ ta_anom, x))[2],
         b_trend_adj = coef(lm(lf ~ s10t + doy, x))[2],
         b_rain_adj = coef(lm(lf ~ s10t + log1p(p72), x))[2],
         b_both_adj = coef(lm(lf ~ s10t + doy + log1p(p72), x))[2])
}
cq <- bind_rows(lapply(split(comp, comp$id, drop = TRUE), slopes))
write.csv(cq, file.path(out_dir, "q10_components_by_chamber.csv"), row.names = FALSE)
cs <- cq %>% group_by(method) %>% summarise(across(starts_with("b_"), ~ exp(10 * mean(., na.rm = TRUE))), n = n())
print(cs, width = 200)
for (m in cs$method) for (k in grep("^b_", names(cs), value = TRUE))
  record(paste0("q10comp_", m, "_", sub("b_", "", k)), cs[[k]][cs$method == m], "q10_components", "exp(10 x mean chamber slope)")
for (k in c("b_total", "b_between", "b_within", "b_trend_adj", "b_rain_adj", "b_both_adj")) {
  tt <- t.test(as.formula(paste(k, "~ method")), data = cq)
  record(paste0("q10comp_p_", sub("b_", "", k)), tt$p.value, "q10_components", "Welch t, chamber slopes")
}
# which temperature best explains each system (chamber random intercepts; AIC)
for (m in c("autochamber", "fluxbot")) {
  x <- dq %>% filter(method == m, !is.na(s10t_lag6))
  aics <- sapply(c("s10t", "airt", "s10t_lag6"), function(v) AIC(lmer(as.formula(paste("lf ~", v, "+ (1|id)")), data = x, REML = FALSE)))
  for (v in names(aics)) record(paste0("q10_aic_", m, "_", v), aics[v] - min(aics), "q10_driver", "delta AIC vs best temperature driver")
}

# ---- 4. moisture diagnostics (Fluxbot K30) ------------------------------------------------------------------
units <- read_csv(file.path(pkg, "metadata", "fluxbot_units.csv"), col_types = cols(unit = col_character()))
fbr <- read_csv(file.path(pkg, "raw", "fluxbot_sensor_records_2023.csv.gz"), col_types = cols(unit = col_character())) %>%
  filter(!co2_ppm %in% c(65535, 65533), co2_ppm > 0, co2_ppm < 10000) %>%
  mutate(time = as.POSIXct(unix_time, origin = "1970-01-01", tz = "America/New_York"), mm = minute(time) + second(time) / 60,
         rh_pct = if_else(rh_pct >= 0 & rh_pct <= 100.5, rh_pct, NA_real_)) %>%
  filter(mm >= 54, time >= as.POSIXct("2023-10-02", tz = "America/New_York"), time < as.POSIXct("2023-11-05", tz = "America/New_York")) %>%
  mutate(hour_of_obs = floor_date(time, "hour") + 3600)
cl <- fbr %>% group_by(unit, hour_of_obs) %>% filter(sum(mm < 55) >= 3, sum(mm >= 55) >= 20) %>%
  summarise(base = median(co2_ppm[mm < 55]), noise = mad(co2_ppm[mm < 55]),
            rh = mean(rh_pct[mm >= 55], na.rm = TRUE), rh_open = mean(rh_pct[mm < 55], na.rm = TRUE),
            # onset: first time after lid closure (55:00) when CO2 exceeds baseline by max(10 ppm, 3 x noise) for 2 consecutive records
            onset_s = { x <- co2_ppm[mm >= 55]; t <- (mm[mm >= 55] - 55) * 60; thr <- max(10, 3 * mad(co2_ppm[mm < 55]))
                        up <- x > median(co2_ppm[mm < 55]) + thr; i <- which(up & c(up[-1], FALSE))[1]; if (is.na(i)) NA_real_ else t[i] },
            .groups = "drop") %>%
  left_join(met %>% select(hour_of_obs, p72, rh_hf), by = "hour_of_obs")
record("rh_closure_median", median(cl$rh, na.rm = TRUE), "moisture", "Fluxbot in-chamber RH during closures, %")
record("rh_closure_pct_ge95", 100 * mean(cl$rh >= 95, na.rm = TRUE), "moisture")
record("rh_closure_pct_ge99", 100 * mean(cl$rh >= 99, na.rm = TRUE), "moisture")
record("onset_s_median", median(cl$onset_s, na.rm = TRUE), "moisture", "s after lid closure until CO2 rise detected")
mo <- lm(onset_s ~ I(rh >= 95) + log1p(p72), data = cl)
record("onset_s_diff_rh95", coef(mo)[2], "moisture", "extra onset delay when RH >= 95%, s")
record("onset_s_diff_rh95_p", summary(mo)$coefficients[2, 4], "moisture")
record("onset_s_per_log1p_p72", coef(mo)[3], "moisture"); record("onset_s_p72_p", summary(mo)$coefficients[3, 4], "moisture")
# Fluxbot/autochamber ratio vs in-chamber RH (stand-hour)
sh <- d %>% group_by(method, stand, hour_of_obs) %>% filter(n() >= 3) %>% summarise(f = mean(fluxL_umolm2sec), .groups = "drop") %>%
  pivot_wider(names_from = method, values_from = f) %>% filter(!is.na(autochamber), !is.na(fluxbot))
rhs <- cl %>% left_join(units %>% select(unit, stand = stand_code), by = "unit") %>% group_by(stand, hour_of_obs) %>%
  summarise(rh = mean(rh, na.rm = TRUE), onset = mean(onset_s, na.rm = TRUE), .groups = "drop")
rr <- sh %>% mutate(stand = as.character(stand)) %>% inner_join(rhs, by = c("stand", "hour_of_obs")) %>%
  left_join(met %>% select(hour_of_obs, p72, airt, s10t), by = "hour_of_obs") %>% mutate(lr = log(fluxbot / autochamber))
m1 <- lm(lr ~ I(rh >= 95) + log1p(p72) + I(airt - s10t), data = rr)
record("ratio_pct_rh95", 100 * (exp(coef(m1)[2]) - 1), "moisture", "Fluxbot/autochamber ratio change when chamber RH >= 95%")
record("ratio_rh95_p", summary(m1)$coefficients[2, 4], "moisture")
record("ratio_pct_per_log1p_p72_adj", 100 * (exp(coef(m1)[3]) - 1), "moisture", "controlling for RH and air-soil gradient")
record("ratio_p72_p_adj", summary(m1)$coefficients[3, 4], "moisture")
record("ratio_pct_per_C_airsoil_adj", 100 * (exp(coef(m1)[4]) - 1), "moisture")
m2 <- lm(lr ~ onset, data = rr)
record("ratio_pct_per_10s_onset", 100 * (exp(10 * coef(m2)[2]) - 1), "moisture", "ratio vs mean onset delay")
record("ratio_onset_p", summary(m2)$coefficients[2, 4], "moisture")
print(write_numbers("numbers_q10_moisture.csv") %>% select(key, value), row.names = FALSE)

# ---- 4b. sensor delay by breakpoint fit (independent of flux magnitude) -------------------------------------
# For closures with a clear rise (>= 30 ppm over the closure), fit CO2 = c0 for t < t0 and
# c0 + b (t - t0) afterwards, t0 on a 6-s grid 0-180 s after lid closure (55:00); keep the
# t0 minimizing SSE. A wetted PTFE membrane or condensation slows diffusion into the K30 and
# lengthens t0.
seg <- fbr %>% group_by(unit, hour_of_obs) %>% filter(sum(mm >= 55) >= 30) %>%
  summarise(t0 = { t <- (mm[mm >= 55] - 55) * 60; y <- co2_ppm[mm >= 55]
                   if (diff(range(y)) < 30) NA_real_ else {
                     grid <- seq(0, 180, 6)
                     sse <- sapply(grid, function(g) { x <- pmax(t - g, 0); sum(residuals(lm(y ~ x))^2) })
                     grid[which.min(sse)] } },
            rise = diff(range(co2_ppm[mm >= 55])), rh = mean(rh_pct[mm >= 55], na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(t0)) %>% left_join(met %>% select(hour_of_obs, p72), by = "hour_of_obs")
saveRDS(seg, file.path(out_dir, "fluxbot_sensor_delay.rds"))
record("t0_median_s", median(seg$t0), "moisture", "breakpoint delay after lid closure")
record("t0_pct_gt60", 100 * mean(seg$t0 > 60), "moisture", "% closures whose rise starts after the fit window opens")
ms <- lm(t0 ~ I(rh >= 95) + log(rise) + log1p(p72), data = seg)
record("t0_diff_rh95_s", coef(ms)[2], "moisture", "extra delay at RH >= 95%, controlling for rise size and rain")
record("t0_diff_rh95_p", summary(ms)$coefficients[2, 4], "moisture")
record("t0_median_rh_lt90", median(seg$t0[seg$rh < 90], na.rm = TRUE), "moisture")
record("t0_median_rh_ge99", median(seg$t0[seg$rh >= 99], na.rm = TRUE), "moisture")
print(write_numbers("numbers_q10_moisture.csv") %>% filter(grepl("^t0", key)) %>% select(key, value), row.names = FALSE)
