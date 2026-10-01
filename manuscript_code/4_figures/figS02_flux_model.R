# Fig. S2. Flux model. Main analysis uses linear fits (LM) for both systems; the goFlux "best"
# selection (LM or Hutchinson-Mosier, HM) is the alternative.
#  (a) closure-level best / LM flux ratio by system (where goFlux chose HM, the ratio > 1)
#  (b) array offset and hourly r for each Fluxbot x autochamber model pairing (as-deployed data)
#  (c) gap-filled October budgets by model, with chamber-bootstrap 95% CIs
# Closure-level ratios and numbers: 2_analysis/07_flux_model_matrix.R; budgets: 2_analysis/09_scales_budget.R.

source("R/setup.R")
source("R/fig_style.R")
suppressPackageStartupMessages({ library(ggplot2); library(patchwork) })
cl <- read.csv(file.path(out_dir, "flux_model_closures.csv"))
s <- read.csv(file.path(out_dir, "flux_model_closure_summary.csv"))
sys_col <- pal_sys_lab
pa <- ggplot(cl, aes(ratio, fill = system)) + geom_histogram(binwidth = 0.05, boundary = 1, position = "identity", alpha = a_mean) +
  geom_vline(xintercept = 1, colour = col_ref, linetype = "dashed") + coord_cartesian(xlim = c(0.8, 2.5)) +
  scale_fill_manual(values = sys_col, name = NULL) + facet_wrap(~ system, ncol = 1, scales = "free_y") +
  geom_text(data = s, aes(x = 2.5, y = Inf, label = sprintf("HM chosen in %.0f%%\nmedian ratio %.2f", hm_pct, ratio_med)),
            hjust = 1, vjust = 1.3, size = txt, inherit.aes = FALSE) +
  labs(x = "Closure flux, goFlux best / linear", y = "Closures") + theme_afm() +
  theme(legend.position = "none", strip.background = element_blank())
fm <- read.csv(file.path(out_dir, "flux_model_matrix.csv")) %>% filter(autochamber != "HF293") %>%
  mutate(fluxbot = factor(fluxbot, levels = c("LM", "HM", "best")), autochamber = factor(autochamber, levels = c("LM", "HM", "best")))
pb <- ggplot(fm, aes(autochamber, fluxbot, fill = offset_pct)) + geom_tile(colour = "white") +
  geom_text(aes(label = sprintf("%+.0f%%\nr %.2f", offset_pct, r)), size = txt, lineheight = 0.9) +
  scale_fill_gradient2(low = unname(pal_div["low"]), mid = "white", high = unname(pal_div["high"]), midpoint = 0, limits = c(-35, 35), name = "Offset (%)") +
  labs(x = "Autochamber flux model", y = "Fluxbot flux model") + coord_equal() + theme_afm() +
  theme(axis.line = element_blank(), axis.ticks = element_blank())
bud <- read.csv(file.path(out_dir, "october_budget.csv")) %>% filter(model %in% c("LM.flux", "best.flux"), dataset == "deployed") %>%
  distinct(model, system, mean_stands_gC, lo, hi) %>%
  mutate(model = recode(model, LM.flux = "Linear (main)", best.flux = "goFlux best"), system = recode(system, fluxbot = "Fluxbot 2.0", autochamber = "Autochamber"))
pc <- ggplot(bud, aes(model, mean_stands_gC, colour = system)) +
  geom_pointrange(aes(ymin = lo, ymax = hi), position = position_dodge(0.4), size = 0.3, linewidth = lw_main) +
  scale_colour_manual(values = sys_col, name = NULL) + expand_limits(y = 0) +
  labs(x = NULL, y = expression("CO"[2]*"-C, 2-31 Oct (g C m"^-2*")")) + theme_afm() + theme(legend.position = "bottom")
pfig <- (pa | pb | pc) + plot_layout(widths = c(1.1, 1, 0.8)) + plot_annotation(tag_levels = "a")
save_afm(pfig, "FigS02_flux_model", 190, 85, tif = FALSE)
