# Fig. S11. Unit-level reliability vs array-level resilience: per-unit success, coverage by number of units
# reporting (observed vs independent failures), and units reporting through time. Tables from
# 2_analysis/14_resilience.R.
source("R/setup.R")
source("R/fig_style.R")
suppressPackageStartupMessages({ library(ggplot2); library(patchwork) })
set.seed(20260930)
pal <- pal_sys
stl <- c(healthy = "Stand 1", unhealthy = "Stand 2")
p0 <- as.POSIXct("2023-10-02", tz = "America/New_York"); p1 <- as.POSIXct("2023-11-01", tz = "America/New_York")
unit_rate <- read.csv(file.path(out_dir, "unit_success.csv"))
st_h <- read.csv(file.path(out_dir, "stand_hour_states.csv")) %>%
  mutate(hour_of_obs = with_tz(as.POSIXct(hour_of_obs, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), "America/New_York"),
         state = factor(state, levels = c(">= 3 units", "1-2 units", "measured, removed by QC", "no data (down)")),
         series = factor(series, levels = c("Autochamber Stand 1", "Autochamber Stand 2", "Fluxbot 2.0 Stand 1", "Fluxbot 2.0 Stand 2")))
curves <- read.csv(file.path(out_dir, "resilience_curves.csv"))

# ---- figure ------------------------------------------------------------------------------------
pa <- ggplot(unit_rate, aes(lab_sys[method], 100 * success, colour = method)) +
  geom_jitter(width = 0.12, height = 0, size = 1.4, alpha = 0.8) +
  stat_summary(fun = median, geom = "crossbar", width = 0.4, colour = "black", linewidth = 0.3) +
  scale_colour_manual(values = pal, guide = "none") + labs(x = NULL, y = "Hours with a retained\nmeasurement (% per unit)") +
  theme_afm()
pb <- ggplot(curves %>% mutate(stand = stl[stand]), aes(k, 100 * observed, colour = method)) +
  geom_line(aes(y = 100 * independent), linetype = "22", linewidth = 0.4) + geom_line(linewidth = 0.6) + geom_point(size = 1) +
  facet_wrap(~stand) + scale_colour_manual(values = pal, labels = lab_sys, name = NULL) +
  scale_x_continuous(breaks = 1:8) + labs(x = "Units reporting (at least k)", y = "Stand-hours (%)") +
  theme_afm() + theme(legend.position = "bottom", strip.background = element_blank())
pc <- ggplot(st_h %>% mutate(row = paste(lab_sys[method], stl[stand]), frac = n_ok / n_units),
             aes(hour_of_obs, row, fill = n_ok)) + geom_tile() +
  scale_fill_viridis_c(name = "Units\nreporting", option = "D") + labs(x = NULL, y = NULL) +
  scale_x_datetime(date_labels = "%d %b", expand = c(0, 0)) + theme_afm()
fig <- (pa + pb + plot_layout(widths = c(1, 2.2))) / pc + plot_layout(heights = c(1.3, 1)) + tags_afm()
save_afm(fig, "FigS11_resilience", 190, 120, tif = FALSE)

