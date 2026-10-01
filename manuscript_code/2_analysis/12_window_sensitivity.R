# Sensitivity of the comparison to the Fluxbot fit window: main 57:00-60:00 vs the submitted
# 56:00-60:00, whose first minute can contain pre-rise records (K30 diffusion delay).
source("R/setup.R")
suppressPackageStartupMessages(library(lme4))
summ <- function(tag) {
  d <- build_dataset() %>% filter(hour_of_obs < as.POSIXct("2023-11-01", tz = "America/New_York"))
  hrs <- matched_hours(d, 3)
  s <- d %>% filter(hour_of_obs %in% hrs) %>% group_by(hour_of_obs, stand, method) %>% summarise(f = mean(fluxL_umolm2sec), .groups = "drop") %>%
    group_by(hour_of_obs, method) %>% summarise(f = mean(f), .groups = "drop") %>% pivot_wider(names_from = method, values_from = f)
  dd <- s %>% mutate(day = as.Date(hour_of_obs, tz = "America/New_York")) %>% group_by(day) %>% filter(n() >= 12) %>% summarise(a = mean(autochamber), f = mean(fluxbot))
  common <- d %>% distinct(stand, hour_of_obs, method) %>% count(stand, hour_of_obs) %>% filter(n == 2)
  dq <- d %>% semi_join(common, by = c("stand", "hour_of_obs")) %>% filter(fluxL_umolm2sec > 0.3, !is.na(s10t))
  cq <- dq %>% group_by(method, id) %>% filter(n() >= 100) %>% summarise(b = coef(lm(log(fluxL_umolm2sec) ~ s10t))[2], .groups = "drop")
  diel <- dq %>% group_by(method, hour) %>% summarise(f = mean(fluxL_umolm2sec), .groups = "drop") %>% group_by(method) %>%
    summarise(amp = 100 * diff(range(f)) / mean(f))
  tibble(window = tag, offset_pct = 100 * (mean(s$fluxbot) / mean(s$autochamber) - 1), r_hourly = cor(s$autochamber, s$fluxbot),
         r_daily = cor(dd$a, dd$f), q10_ac = exp(10 * mean(cq$b[cq$method == "autochamber"])),
         q10_fb = exp(10 * mean(cq$b[cq$method == "fluxbot"])), q10_p = t.test(b ~ method, cq)$p.value,
         diel_amp_ac = diel$amp[diel$method == "autochamber"], diel_amp_fb = diel$amp[diel$method == "fluxbot"],
         fb_mean = mean(s$fluxbot))
}
options(afm.fluxbot_file = "fluxbot_fluxes_w56.csv"); a <- summ("56:00-60:00 (submitted)")
options(afm.fluxbot_file = "fluxbot_fluxes.csv"); b <- summ("57:00-60:00 (main)")
res <- bind_rows(a, b); print(res, width = 200)
write.csv(res, file.path(out_dir, "window_sensitivity.csv"), row.names = FALSE)
for (i in 1:2) for (k in setdiff(names(res), "window")) record(paste0("win", c(56, 57)[i], "_", k), res[[k]][i], "window", res$window[i])

# ---- closure level: 56:00 vs 57:00 window flux, by sensor delay t0 (2_analysis/11_q10_moisture.R) ----
seg <- readRDS(file.path(out_dir, "fluxbot_sensor_delay.rds"))
f57 <- read.csv(file.path(flux_dir, "fluxbot_fluxes.csv"), colClasses = c(id = "character")) %>% mutate(hour_of_obs = round_hour(start_local))
f56 <- read.csv(file.path(flux_dir, "fluxbot_fluxes_w56.csv"), colClasses = c(id = "character")) %>% mutate(hour_of_obs = round_hour(start_local))
both <- inner_join(f57 %>% select(unit = id, hour_of_obs, lm57 = LM.flux, wet, lid_fail),
                   f56 %>% select(unit = id, hour_of_obs, lm56 = LM.flux), by = c("unit", "hour_of_obs")) %>%
  inner_join(seg %>% select(unit, hour_of_obs, t0, rise, rh), by = c("unit", "hour_of_obs")) %>%
  filter(!wet, !lid_fail, lm57 > 0.5) %>% mutate(ratio = lm56 / lm57)
record("win_ratio56_57_median", median(both$ratio), "fit_window", "closure-level LM flux, 56:00 / 57:00 window, dry closures")
for (b in list(c(0, 30), c(31, 60), c(61, 120), c(121, 180))) {
  z <- both %>% filter(t0 >= b[1], t0 <= b[2])
  record(sprintf("win_ratio56_57_t0_%d_%d", b[1], b[2]), median(z$ratio), "fit_window", paste("n =", nrow(z)))
}
record("t0_pct_gt120", 100 * mean(seg$t0 > 120), "fit_window", "% closures whose rise starts after 57:00")
write.csv(both %>% mutate(hour_of_obs = format(with_tz(hour_of_obs, "UTC"), "%Y-%m-%dT%H:%M:%SZ")), file.path(out_dir, "fit_window_closures.csv"), row.names = FALSE)
write_numbers()
