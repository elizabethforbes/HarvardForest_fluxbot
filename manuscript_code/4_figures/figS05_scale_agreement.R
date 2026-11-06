# Fig. S5. Agreement between systems (r, nRMSE) as a function of averaging block, with the within- and
# cross-system subset benchmark (2_analysis/08_scales_budget.R), and the offset between disjoint
# three-chamber subsets from the spatial-null benchmark (2_analysis/04_agreement_metrics.R).
source("R/setup.R")
source("R/fig_style.R")
suppressPackageStartupMessages({ library(ggplot2); library(patchwork) })
blocks <- c(1, 3, 6, 12, 24, 72)
cr <- read.csv(file.path(out_dir, "scale_agreement_main.csv")) %>% mutate(pair = "full arrays (autochamber vs Fluxbot)")
bs <- read.csv(file.path(out_dir, "scale_benchmark_main.csv")) %>%
  mutate(pair = recode(pair, autochamber = "autochamber subsets", fluxbot = "Fluxbot subsets", cross = "autochamber vs Fluxbot subsets"))
pal <- c("autochamber subsets" = unname(pal_sys["autochamber"]), "Fluxbot subsets" = unname(pal_sys["fluxbot"]), "autochamber vs Fluxbot subsets" = col_cross, "full arrays (autochamber vs Fluxbot)" = "black")
pp <- function(v, lab) ggplot(bs, aes(L, .data[[paste0(v, "_med")]], colour = pair, fill = pair)) +
  geom_ribbon(aes(ymin = .data[[paste0(v, "_lo")]], ymax = .data[[paste0(v, "_hi")]]), alpha = 0.12, colour = NA) +
  geom_line() + geom_point(size = 1) + geom_line(data = cr, aes(L, .data[[v]]), linewidth = 0.8) +
  geom_point(data = cr, aes(L, .data[[v]]), size = 1.5) +
  scale_x_log10(breaks = blocks) + scale_colour_manual(values = pal, name = NULL) + scale_fill_manual(values = pal, name = NULL) +
  labs(x = "Averaging block (h)", y = lab) + theme_afm()
# (c) offset between subsets: same-system pairs vs cross-system pairs; dashed line = full arrays
null <- readRDS(file.path(out_dir, "spatial_null.rds")) %>%
  mutate(pair = factor(recode(pair, "autochamber vs autochamber" = "autochamber subsets", "Fluxbot vs Fluxbot" = "Fluxbot subsets",
                              "autochamber vs Fluxbot" = "autochamber vs Fluxbot subsets"),
                       levels = c("autochamber subsets", "Fluxbot subsets", "autochamber vs Fluxbot subsets")),
         offset_pct = 100 * abs_bias)
full_offset <- abs(cr$offset_pct[cr$L == 1])
pc <- ggplot(null, aes(pair, offset_pct, fill = pair)) +
  geom_violin(colour = NA, alpha = 0.5) + geom_boxplot(width = 0.15, outliers = FALSE, fill = "white", linewidth = 0.3) +
  geom_hline(yintercept = full_offset, linetype = "dashed", linewidth = 0.4) +
  scale_fill_manual(values = pal, guide = "none") +
  scale_x_discrete(labels = c("Autochamber\nsubsets", "Fluxbot\nsubsets", "Across\nsystems")) +
  labs(x = NULL, y = "Offset between subsets (% of mean)") + theme_afm()
pscale <- (pp("r", "Correlation (r)") + pp("nrmse", "nRMSE (% of mean)") + pc + plot_layout(guides = "collect", widths = c(1, 1, 0.8)) +
  plot_annotation(tag_levels = "a")) & theme(legend.position = "bottom")
save_afm(pscale, "FigS05_scale_agreement", 190, 80, tif = FALSE)
