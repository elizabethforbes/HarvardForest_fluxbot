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
diel <- read.csv(file.path(out_dir, "diel_common_window.csv"))
p7 <- ggplot(diel, aes(hour, mean, colour = method, fill = method)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = a_band, colour = NA) +
  geom_line(linewidth = lw_main) + geom_point(size = pt_mean) +
  scale_colour_manual(values = pal, labels = lab_sys, name = NULL) +
  scale_fill_manual(values = pal, labels = lab_sys, name = NULL) +
  scale_x_continuous(breaks = seq(0, 24, 4)) +
  labs(x = "Hour of day (EDT)", y = flux_lab) + theme(legend.position = c(0.2, 0.88))
save_fig(p7, "Fig5_diel", 90, 70)
