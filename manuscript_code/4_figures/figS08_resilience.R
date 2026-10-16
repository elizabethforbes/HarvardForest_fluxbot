# Fig. S8. Array-level resilience: coverage by number of units reporting, observed and expected under independent failures (per-unit success and units reporting over time are in main Fig. 6)
# Curves from 2_analysis/13_resilience.R.
source("R/setup.R")
source("R/fig_style.R")
suppressPackageStartupMessages({ library(ggplot2) })
pal <- pal_sys
stl <- c(healthy = "Stand 1", unhealthy = "Stand 2")
curves <- read.csv(file.path(out_dir, "resilience_curves.csv"))

# ---- figure ------------------------------------------------------------------------------------
pb <- ggplot(curves %>% mutate(stand = stl[stand]), aes(k, 100 * observed, colour = method)) +
  geom_line(aes(y = 100 * independent), linetype = "22", linewidth = 0.4) + geom_line(linewidth = 0.6) + geom_point(size = 1) +
  facet_wrap(~stand) + scale_colour_manual(values = pal, labels = lab_sys, name = NULL) +
  scale_x_continuous(breaks = 1:8) + labs(x = "Units reporting (at least k)", y = "Stand-hours (%)") +
  theme_afm() + theme(legend.position = "bottom", strip.background = element_blank())
save_afm(pb, "FigS08_resilience", 160, 75, tif = FALSE)
