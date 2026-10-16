# Fig. S6. Laboratory tests of wet K30 sensors: the two test time series (panels built by 3_lab_tests/01 and 02)
# and the diagnostics most relevant to field fluxes (3_lab_tests/03_failure_modes.R): response to CO2 ramps
# and offset against each sensor's own dry calibration.
#  (a) 22 Sep test, (b) 13 Dec test, (c) covered/uncovered slope on matched CO2 ramps, (d) offset vs dry calibration
source("R/setup.R")
source("R/fig_style.R")
suppressPackageStartupMessages({ library(ggplot2); library(patchwork) })
set.seed(20260930)
sep <- readRDS(file.path(out_dir, "panels_ptfe_sep.rds")); dec <- readRDS(file.path(out_dir, "panels_cover_dec.rds"))
pp <- read.csv(file.path(out_dir, "lab_failure_modes_by_phase.csv"))
rp <- read.csv(file.path(out_dir, "lab_ramp_slopes.csv"))
ttl <- function(p, t) p + ggtitle(t) + theme(plot.title = element_text(size = 8, face = "bold"))
pa <- ttl(sep$zoom, "22 Sep 2023: one PTFE-covered and one uncovered K30; covered sensor sprayed at 17:32") + theme(legend.position = "bottom")
pb <- ttl(dec$phases, "13 Dec 2023: two covered (t1, t2) and three uncovered (c1-c3) K30s") + theme(legend.position = "bottom")
lvl <- c("Sep: Dry", "Sep: Covered sensor wetted", "Dec: Dry", "Dec: Wet PTFE", "Dec: Dry bracket", "Dec: Wet bracket", "Dec: Bare K30s sprayed")
lab_ph <- function(t, p) factor(paste0(substr(t, 4, 6), ": ", p), levels = lvl)
gcol <- c(covered = unname(pal_lab["covered"]), uncovered = unname(pal_lab["uncovered"]))
ppl <- pp %>% mutate(ph = lab_ph(test, phase), slab = if_else(test == "13 Dec", sensor, ""), noisy = sensor == "c2")
pc <- ggplot(rp %>% filter(group == "covered") %>% mutate(ph = lab_ph(test, phase)), aes(ph, rel_unc, colour = group)) + geom_hline(yintercept = 1, colour = "grey50") +
  geom_point(position = position_jitter(width = 0.12, height = 0), size = 0.8, alpha = 0.6, show.legend = FALSE) +
  stat_summary(fun = median, geom = "point", shape = 95, size = 8, show.legend = FALSE) +
  scale_colour_manual(values = gcol, name = NULL) + coord_cartesian(ylim = c(0.4, 1.6)) +
  labs(x = NULL, y = "Covered / uncovered\nslope (90-s ramps)", title = "Response to CO2 ramps")
pd <- ggplot(ppl, aes(ph, bias_vs_dry, colour = group, label = slab, shape = noisy)) + geom_hline(yintercept = 0, colour = "grey50") +
  geom_point(position = position_dodge(0.6), size = 1.6) + geom_text(position = position_dodge(0.6), size = 1.8, hjust = -0.5, show.legend = FALSE) +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 1), labels = "c2 (noisy all day)", breaks = "TRUE", name = NULL) + scale_colour_manual(values = gcol, name = NULL) +
  labs(x = NULL, y = "Offset vs own dry\ncalibration (ppm)", title = "Offset when wet")
diag <- (pc | pd) & theme_afm() & theme(legend.position = "bottom", plot.title = element_text(size = 8, face = "bold"), axis.text.x = element_text(angle = 30, hjust = 1, size = 6))
pfig <- (pa / pb / diag) + plot_layout(heights = c(2, 3, 1.6)) + plot_annotation(tag_levels = "a")
save_afm(pfig, "FigS06_lab_tests", 190, 235, tif = FALSE)
