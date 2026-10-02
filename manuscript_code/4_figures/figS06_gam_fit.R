# Fig. S6. Observed vs fitted fluxes from the GAM (2_analysis/03_main_analyses.R), as deployed.
source("R/setup.R")
source("R/fig_style.R")
suppressPackageStartupMessages({ library(ggplot2); library(patchwork); library(mgcv) })
pal <- pal_sys
save_fig <- function(p, name, width_mm, height_mm) save_afm(p, name, width_mm, height_mm)
d <- load_chamber_hours("deployed") %>%
  mutate(stand_label = factor(if_else(stand == "healthy", "Stand 1", "Stand 2")),
         method_label = factor(lab_sys[as.character(method)], levels = lab_sys))
num <- get_number
set.seed(20260930)

# ---- SI: GAM observed vs fitted -----------------------------------------------------
g <- readRDS(file.path(out_dir, "gam_re.rds"))
d$fitted <- fitted(g)
fit_of <- lm(fluxL_umolm2sec ~ fitted, data = d)
p4 <- ggplot(d, aes(fitted, fluxL_umolm2sec)) +
  geom_point(aes(colour = method), size = pt_dense, alpha = a_dense, stroke = 0) +
  geom_abline(linetype = "dashed") +
  scale_colour_manual(values = pal, labels = lab_sys, name = NULL) +
  guides(colour = guide_legend(override.aes = list(size = 1.5, alpha = 1))) +
  annotate("text", x = -Inf, y = Inf, hjust = -0.05, vjust = 1.3, size = txt,
           label = sprintf("Adj. R\u00b2 = %.2f", num("gam_r2adj"))) +
  labs(x = expression(GAM ~ fitted ~ flux ~ (mu * mol ~ m^-2 ~ s^-1)), y = expression(Observed ~ flux ~ (mu * mol ~ m^-2 ~ s^-1))) +
  coord_equal() + theme(legend.position = c(0.8, 0.12))
save_fig(p4, "FigS06_gam_fit", 90, 90)
