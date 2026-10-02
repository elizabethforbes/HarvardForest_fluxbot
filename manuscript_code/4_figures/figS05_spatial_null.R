# Fig. S5. Spatial-null benchmark: offset, daily RMSE, hourly r and daily CCC between disjoint subsets of
# chambers of the same system and of different systems (2_analysis/05_agreement_metrics.R).
source("R/setup.R")
source("R/fig_style.R")
suppressPackageStartupMessages({ library(ggplot2); library(patchwork) })
null <- readRDS(file.path(out_dir, "spatial_null.rds"))
pal3 <- c("autochamber vs autochamber" = unname(pal_sys["autochamber"]), "Fluxbot vs Fluxbot" = unname(pal_sys["fluxbot"]), "autochamber vs Fluxbot" = col_cross)
nl <- null %>% mutate(pair = factor(pair, levels = names(pal3)), abs_bias = 100 * abs_bias)
pan <- function(v, lab) ggplot(nl, aes(pair, .data[[v]], fill = pair)) +
  geom_violin(colour = NA, alpha = 0.6) + geom_boxplot(width = 0.15, outliers = FALSE, fill = "white", linewidth = 0.3) +
  scale_fill_manual(values = pal3, guide = "none") + labs(x = NULL, y = lab) +
  scale_x_discrete(labels = c("AC vs AC", "FB vs FB", "AC vs FB")) + theme_afm()
pS6 <- pan("abs_bias", "Offset between subsets\n(% of mean flux)") + pan("nrmse_daily", "Daily RMSE\n(% of mean flux)") +
  pan("r_hourly", "Hourly correlation (r)") + pan("ccc_daily", "Daily CCC") + plot_layout(ncol = 4) +
  plot_annotation(tag_levels = "a")
save_afm(pS6, "FigS05_spatial_null", 190, 60, tif = FALSE)
