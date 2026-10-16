# Fig. S5. Agreement between systems (r, nRMSE) as a function of averaging block, with the within- and
# cross-system subset benchmark (2_analysis/08_scales_budget.R).
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
pscale <- pp("r", "Correlation (r)") + pp("nrmse", "nRMSE (% of mean)") + plot_layout(guides = "collect") & theme(legend.position = "bottom")
save_afm(pscale, "FigS05_scale_agreement", 190, 80, tif = FALSE)
