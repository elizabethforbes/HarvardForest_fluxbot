# Fig. 5. Diel cycle of each system over common stand-hours (2_analysis/03_main_analyses.R).
source("R/setup.R")
source("R/fig_style.R")
suppressPackageStartupMessages({ library(ggplot2); library(patchwork) })
pal <- pal_sys
save_fig <- function(p, name, width_mm, height_mm) save_afm(p, name, width_mm, height_mm)
d <- load_chamber_hours("deployed") %>%
  mutate(stand_label = factor(if_else(stand == "healthy", "Stand 1", "Stand 2")),
         method_label = factor(lab_sys[as.character(method)], levels = lab_sys))
num <- get_number
set.seed(20260930)

# ---- Fig 5: diel pattern (common stand-hours) --------------------------------------------
# points and bars: hourly mean over days +/- 95% CI (2_analysis/03_main_analyses.R); line and band:
# cyclic GAM smooth of the day x hour means, with its 95% confidence band
diel <- read.csv(file.path(out_dir, "diel_common_window.csv"))
common <- d %>% distinct(stand, hour_of_obs, method) %>% count(stand, hour_of_obs) %>% filter(n == 2)
dh <- d %>% semi_join(common, by = c("stand", "hour_of_obs")) %>% mutate(day = as.Date(hour_of_obs, tz = "America/New_York")) %>%
  group_by(method, day, hour) %>% summarise(f = mean(fluxL_umolm2sec), .groups = "drop")
sm <- bind_rows(lapply(split(dh, dh$method), function(x) {
  g <- mgcv::gam(f ~ s(hour, bs = "cc", k = 10), data = x, knots = list(hour = c(-0.5, 23.5)))
  nd <- data.frame(hour = seq(0, 23, length.out = 200)); pr <- predict(g, nd, se.fit = TRUE)
  data.frame(method = x$method[1], hour = nd$hour, fit = pr$fit, lo = pr$fit - 1.96 * pr$se.fit, hi = pr$fit + 1.96 * pr$se.fit)
}))
p7 <- ggplot(diel, aes(hour, mean, colour = method, fill = method)) +
  geom_ribbon(data = sm, aes(x = hour, ymin = lo, ymax = hi, fill = method), inherit.aes = FALSE, alpha = 0.25) +
  geom_line(data = sm, aes(x = hour, y = fit, colour = method), inherit.aes = FALSE, linewidth = lw_main) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0, linewidth = lw_thin, alpha = 0.35) +
  geom_point(size = pt_mean) +
  scale_colour_manual(values = pal, labels = lab_sys, name = NULL) +
  scale_fill_manual(values = pal, labels = lab_sys, name = NULL) +
  scale_x_continuous(breaks = seq(0, 24, 4)) +
  labs(x = "Hour of day (EDT)", y = flux_lab) + theme(legend.position = c(0.2, 0.88))
save_fig(p7, "Fig5_diel", 90, 70)
