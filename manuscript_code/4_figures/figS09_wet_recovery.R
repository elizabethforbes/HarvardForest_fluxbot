# Fig. S9. Wet sensors in the field: episode lengths, recovery time, open-lid CO2 anomaly and flux ratio
# around the end of wet-sensor episodes (2_analysis/15_wet_recovery.R).
source("R/setup.R")
source("R/fig_style.R")
suppressPackageStartupMessages({ library(ggplot2); library(patchwork) })
pdat <- readRDS(file.path(out_dir, "wet_recovery_plotdata.rds"))
episodes <- pdat$episodes; comp_end <- pdat$comp_end; hi <- pdat$hi; thr <- pdat$thr
th <- theme_afm()
ep_len <- ggplot(episodes, aes(length_h, fill = type)) + geom_histogram(binwidth = 6, boundary = 0, colour = "white") +
  scale_fill_manual(values = c("wet sensor" = col_wet, "lid failure" = unname(pal_state["no data (down)"])), name = NULL) +
  labs(x = "Episode length (h, RH >= 99%)", y = "Episodes") + th + theme(legend.position = c(0.7, 0.8))
pa <- ggplot(comp_end, aes(t_end, anom)) + annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf, fill = col_wet, alpha = 0.12) +
  geom_hline(yintercept = c(0, thr), linetype = c("solid", "22"), colour = "grey50") +
  geom_ribbon(aes(ymin = anom_lo, ymax = anom_hi), fill = "grey80") + geom_line() + geom_point(size = 0.6) +
  labs(x = "Hours since last wet hour (wet-sensor episodes)", y = "Open-lid CO2 anomaly (ppm)\nmedian, IQR") + th
pb <- ggplot(comp_end %>% filter(n_ratio >= 10), aes(t_end, ratio)) + annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf, fill = col_wet, alpha = 0.12) +
  geom_hline(yintercept = c(1, pdat$ref_ratio), linetype = c("solid", "22"), colour = "grey50") + geom_line() + geom_point(size = 0.6) +
  labs(x = "Hours since last wet hour (wet-sensor episodes)", y = "Fluxbot / autochamber\n(median; dashed = dry reference)") + th
pc <- ggplot(hi %>% filter(!is.na(rec_h)), aes(factor(rec_h, levels = 1:6))) + geom_bar(fill = col_wet) + scale_x_discrete(drop = FALSE) +
  labs(x = paste0("Hours after drying until open-lid\nanomaly <= ", thr, " ppm (episodes ending > 100 ppm)"), y = "Wet-sensor episodes") + th
pfig <- (ep_len | pc) / pa / pb + plot_annotation(tag_levels = "a")
save_afm(pfig, "FigS09_wet_recovery", 160, 170, tif = FALSE)
