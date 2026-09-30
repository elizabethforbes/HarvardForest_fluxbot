# Sensitivity of the comparison to the Fluxbot fit window: main 57:00-60:00 vs the submitted
# 56:00-60:00, whose first minute can contain pre-rise records (K30 diffusion delay).
source("afm_revision/00_prep.R")
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
write_numbers("numbers_window.csv")
