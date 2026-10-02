# Fig. S4. Agreement between systems (r, RMSE) as a function of averaging block, with the within- and
# cross-system subset benchmark (2_analysis/09_scales_budget.R).
source("R/setup.R")
source("R/fig_style.R")
suppressPackageStartupMessages({ library(ggplot2); library(patchwork) })
blocks <- c(1, 3, 6, 12, 24, 72)
cr <- read.csv(file.path(out_dir, "scale_agreement_LM.csv")) %>% mutate(pair = "full arrays (AC vs FB)")
bs <- read.csv(file.path(out_dir, "scale_benchmark_LM.csv")) %>%
  mutate(pair = recode(pair, autochamber = "AC vs AC subsets", fluxbot = "FB vs FB subsets", cross = "AC vs FB subsets"))
pal <- c("AC vs AC subsets" = unname(pal_sys["autochamber"]), "FB vs FB subsets" = unname(pal_sys["fluxbot"]), "AC vs FB subsets" = col_cross, "full arrays (AC vs FB)" = "black")
pp <- function(v, lab) ggplot(bs, aes(L, .data[[paste0(v, "_med")]], colour = pair, fill = pair)) +
  geom_ribbon(aes(ymin = .data[[paste0(v, "_lo")]], ymax = .data[[paste0(v, "_hi")]]), alpha = 0.12, colour = NA) +
  geom_line() + geom_point(size = 1) + geom_line(data = cr, aes(L, .data[[v]]), linewidth = 0.8) +
  geom_point(data = cr, aes(L, .data[[v]]), size = 1.5) +
  scale_x_log10(breaks = blocks) + scale_colour_manual(values = pal, name = NULL) + scale_fill_manual(values = pal, name = NULL) +
  labs(x = "Averaging block (h)", y = lab) + theme_afm()
pscale <- pp("r", "Correlation (r)") + pp("nrmse", "RMSE (% of mean)") + plot_layout(guides = "collect") & theme(legend.position = "bottom")
save_afm(pscale, "FigS04_scale_agreement", 190, 80, tif = FALSE)
