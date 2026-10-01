# SI figure: flux model. Main analysis uses linear fits (LM) for both systems; the goFlux "best"
# selection (LM or Hutchinson-Mosier, HM) is the alternative.
#  (a) closure-level best / LM flux ratio by system (where goFlux chose HM, the ratio > 1)
#  (b) array offset and hourly r for each Fluxbot x autochamber model pairing (as-deployed data)
#  (c) gap-filled October budgets by model, with chamber-bootstrap 95% CIs

source("afm_revision/00_prep.R")
suppressPackageStartupMessages({ library(ggplot2); library(patchwork) })
cl <- bind_rows(apply_qc(load_fluxbot("LM.flux"), "deployed") %>% mutate(system = "Fluxbot"),
                apply_qc(load_autochamber("LM.flux"), "deployed") %>% mutate(system = "Autochamber")) %>%
  filter(LM.flux > 0.3) %>% mutate(ratio = best.flux / LM.flux, hm = model == "HM")
s <- cl %>% group_by(system) %>% summarise(hm_pct = 100 * mean(hm), ratio_med = median(ratio), ratio_mean = mean(ratio), .groups = "drop")
print(s)
for (i in seq_len(nrow(s))) { tg <- tolower(s$system[i])
  record(paste0("fm_hm_selected_pct_", tg), s$hm_pct[i], "flux_model", "goFlux best.flux chose HM")
  record(paste0("fm_best_lm_ratio_median_", tg), s$ratio_med[i], "flux_model", "closure-level best / LM") }
sys_col <- c(Fluxbot = "#4D4D4D", Autochamber = "#1F6E43")
pa <- ggplot(cl, aes(ratio, fill = system)) + geom_histogram(binwidth = 0.05, boundary = 1, position = "identity", alpha = 0.55) +
  geom_vline(xintercept = 1, colour = "grey30") + coord_cartesian(xlim = c(0.8, 2.5)) +
  scale_fill_manual(values = sys_col, name = NULL) + facet_wrap(~ system, ncol = 1, scales = "free_y") +
  geom_text(data = s, aes(x = 2.5, y = Inf, label = sprintf("HM chosen in %.0f%%\nmedian ratio %.2f", hm_pct, ratio_med)),
            hjust = 1, vjust = 1.3, size = 2.3, inherit.aes = FALSE) +
  labs(x = "Closure flux, goFlux best / linear", y = "Closures") + theme_classic(base_size = 8) +
  theme(legend.position = "none", strip.background = element_blank())
fm <- read.csv(file.path(out_dir, "flux_model_matrix.csv")) %>% filter(autochamber != "HF293") %>%
  mutate(fluxbot = factor(fluxbot, levels = c("LM", "HM", "best")), autochamber = factor(autochamber, levels = c("LM", "HM", "best")))
pb <- ggplot(fm, aes(autochamber, fluxbot, fill = offset_pct)) + geom_tile(colour = "white") +
  geom_text(aes(label = sprintf("%+.0f%%\nr %.2f", offset_pct, r)), size = 2.4, lineheight = 0.9) +
  scale_fill_gradient2(low = "#B2182B", mid = "white", high = "#2166AC", midpoint = 0, limits = c(-35, 35), name = "Offset (%)") +
  labs(x = "Autochamber flux model", y = "Fluxbot flux model") + coord_equal() + theme_minimal(base_size = 8) +
  theme(panel.grid = element_blank())
bud <- read.csv(file.path(out_dir, "october_budget.csv")) %>% filter(model %in% c("LM.flux", "best.flux"), dataset == "deployed") %>%
  distinct(model, system, mean_stands_gC, lo, hi) %>%
  mutate(model = recode(model, LM.flux = "Linear (main)", best.flux = "goFlux best"), system = recode(system, fluxbot = "Fluxbot", autochamber = "Autochamber"))
pc <- ggplot(bud, aes(model, mean_stands_gC, colour = system)) +
  geom_pointrange(aes(ymin = lo, ymax = hi), position = position_dodge(0.4), size = 0.3) +
  scale_colour_manual(values = sys_col, name = NULL) + expand_limits(y = 0) +
  labs(x = NULL, y = expression("CO"[2]*"-C, 2-31 Oct (g C m"^-2*")")) + theme_classic(base_size = 8) + theme(legend.position = "bottom")
pfig <- (pa | pb | pc) + plot_layout(widths = c(1.1, 1, 0.8)) + plot_annotation(tag_levels = "a")
ggsave(file.path(out_dir, "figures", "FigS_flux_model.pdf"), pfig, width = 190, height = 85, units = "mm", device = cairo_pdf)
ggsave(file.path(out_dir, "figures", "FigS_flux_model.png"), pfig, width = 190, height = 85, units = "mm", dpi = 300, device = ragg::agg_png)
print(write_numbers("numbers_fig_flux_model.csv") %>% select(key, value), row.names = FALSE)
