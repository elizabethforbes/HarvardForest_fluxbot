# Fig. S9. Failure-mode diagnostics from the two laboratory tests, per sensor and phase: response to CO2
# ramps, offset and scatter against each sensor's own dry calibration, and error codes
# (3_lab_tests/03_failure_modes.R).
source("R/setup.R")
source("R/fig_style.R")
suppressPackageStartupMessages({ library(ggplot2); library(patchwork) })
set.seed(20260930)
pp <- read.csv(file.path(out_dir, "lab_failure_modes_by_phase.csv"))
rp <- read.csv(file.path(out_dir, "lab_ramp_slopes.csv"))
# ---- figure: per sensor and phase, (a) ramp slope ratio, (b) bias vs dry calibration, (c) scatter,
#      (d) error-code share --------------------------------------------------------------------------
lvl <- c("Sep: Dry", "Sep: Covered sensor wetted", "Dec: Dry", "Dec: Wet PTFE", "Dec: Dry bracket", "Dec: Wet bracket", "Dec: Bare K30s sprayed")
lab_ph <- function(t, p) factor(paste0(substr(t, 4, 6), ": ", p), levels = lvl)
gcol <- c(covered = unname(pal_lab["covered"]), uncovered = unname(pal_lab["uncovered"]))
ppl <- pp %>% mutate(ph = lab_ph(test, phase), slab = if_else(test == "13 Dec", sensor, ""), noisy = sensor == "c2")
p_a <- ggplot(rp %>% filter(group == "covered") %>% mutate(ph = lab_ph(test, phase)), aes(ph, rel_unc, colour = group)) + geom_hline(yintercept = 1, colour = "grey50") +
  geom_point(position = position_jitter(width = 0.12, height = 0), size = 0.8, alpha = 0.6, show.legend = FALSE) +
  stat_summary(fun = median, geom = "point", shape = 95, size = 8, show.legend = FALSE) +
  scale_colour_manual(values = gcol, name = NULL) + coord_cartesian(ylim = c(0.4, 1.6)) +
  labs(x = NULL, y = "Covered / uncovered\nslope (90-s ramps)", title = "Response to CO2 ramps (diffusion barrier would lower this)")
p_b <- ggplot(ppl, aes(ph, bias_vs_dry, colour = group, label = slab, shape = noisy)) + geom_hline(yintercept = 0, colour = "grey50") +
  geom_point(position = position_dodge(0.6), size = 1.8) + geom_text(position = position_dodge(0.6), size = 1.9, hjust = -0.5, show.legend = FALSE) +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 1), labels = "c2 (noisy all day)", breaks = "TRUE", name = NULL) + scale_colour_manual(values = gcol, name = NULL) +
  labs(x = NULL, y = "Bias vs own dry\ncalibration (ppm)", title = "Offset when wet")
p_c <- ggplot(ppl, aes(ph, scatter_vs_dry, colour = group, label = slab, shape = noisy)) + geom_point(position = position_dodge(0.6), size = 1.8) + geom_text(position = position_dodge(0.6), size = 1.9, hjust = -0.5, show.legend = FALSE) +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 1), labels = "c2 (noisy all day)", breaks = "TRUE", name = NULL) +
  scale_colour_manual(values = gcol, name = NULL) + labs(x = NULL, y = "Scatter vs own dry\ncalibration (MAD, ppm)", title = "Erratic readings (scatter)")
p_d <- ggplot(ppl, aes(ph, err_pct, colour = group, label = slab, shape = noisy)) + geom_point(position = position_dodge(0.6), size = 1.8) + geom_text(position = position_dodge(0.6), size = 1.9, hjust = -0.5, show.legend = FALSE) +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 1), labels = "c2 (noisy all day)", breaks = "TRUE", name = NULL) +
  scale_colour_manual(values = gcol, name = NULL) + labs(x = NULL, y = "Error codes (%)", title = "Error codes (electronics)")
pfig <- (p_a / p_b / p_c / p_d) + plot_layout(guides = "collect") + plot_annotation(tag_levels = "a") &
  theme_afm() & theme(legend.position = "bottom", plot.title = element_text(size = 8), axis.text.x = element_text(angle = 25, hjust = 1))
save_afm(pfig, "FigS09_lab_failure_modes", 140, 230, tif = FALSE)
