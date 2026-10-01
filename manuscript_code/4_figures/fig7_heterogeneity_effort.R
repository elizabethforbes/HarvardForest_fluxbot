# Fig. 7. Spatial heterogeneity (Lorenz curves, Gini) and sampling effort (CI half-width vs number of chambers).
source("R/setup.R")
source("R/fig_style.R")
suppressPackageStartupMessages({ library(ggplot2); library(patchwork); library(ineq) })
pal <- pal_sys
save_fig <- function(p, name, width_mm, height_mm) save_afm(p, name, width_mm, height_mm)
d <- load_chamber_hours("deployed") %>%
  mutate(stand_label = factor(if_else(stand == "healthy", "Stand 1", "Stand 2")),
         method_label = factor(lab_sys[as.character(method)], levels = lab_sys))
num <- get_number
set.seed(20260930)

# ---- Fig 7: spatial heterogeneity and sampling effort ---------------------------------------
lz <- d %>% group_by(hour_of_obs) %>%
  filter(sum(method == "autochamber") >= 12 & sum(method == "fluxbot") >= 12) %>% ungroup() %>%
  group_by(method, id) %>% summarise(mf = mean(fluxL_umolm2sec), .groups = "drop") %>%
  group_by(method) %>% group_modify(~ { lc <- Lc(.x$mf); data.frame(p = lc$p, L = lc$L, gini = Gini(.x$mf)) }) %>%
  ungroup()
glab <- lz %>% distinct(method, gini) %>%
  mutate(lab = sprintf("%s: Gini = %.2f", lab_sys[as.character(method)], gini), y = c(0.95, 0.87))
p9a <- ggplot(lz, aes(p, L, colour = method)) +
  geom_abline(linetype = "dashed", colour = col_ref) + geom_line(linewidth = lw_main) + geom_point(size = pt_mean) +
  geom_text(data = glab, aes(x = 0.02, y = y, label = lab), hjust = 0, size = 2.5, show.legend = FALSE) +
  scale_colour_manual(values = pal, guide = "none") + coord_equal() +
  labs(x = "Cumulative share of chambers", y = "Cumulative share of flux")
eff <- read.csv(file.path(out_dir, "sampling_effort.csv")) %>%
  mutate(stand_label = if_else(stand == "healthy", "Stand 1", "Stand 2"))
p9b <- ggplot(eff %>% filter(n >= 3), aes(n, 100 * analytic, colour = method, linetype = stand_label)) +
  geom_hline(yintercept = c(10, 20), colour = "grey70", linewidth = 0.3) +
  geom_line(linewidth = 0.5) +
  geom_point(data = eff %>% filter(n == n_chambers), size = pt_big - 0.6) +
  scale_colour_manual(values = pal, labels = lab_sys, name = NULL) +
  scale_linetype_manual(values = c("solid", "22"), name = NULL) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Number of chambers per stand", y = "Uncertainty of stand mean (±%)") +
  theme(legend.position = c(0.72, 0.78), legend.spacing.y = unit(0, "mm"))
save_fig(p9a + p9b + tags_afm(), "Fig7_heterogeneity_effort", 190, 85)
