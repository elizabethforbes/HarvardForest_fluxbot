# Fig. S7. Distributions of individual fluxes in compared hours, as deployed.
source("R/setup.R")
source("R/fig_style.R")
suppressPackageStartupMessages({ library(ggplot2); library(patchwork) })
pal <- pal_sys
save_fig <- function(p, name, width_mm, height_mm) save_afm(p, name, width_mm, height_mm)
d <- load_chamber_hours("deployed") %>%
  mutate(stand_label = factor(if_else(stand == "healthy", "Stand 1", "Stand 2")),
         method_label = factor(lab_sys[as.character(method)], levels = lab_sys))
num <- get_number
set.seed(20260930)

# ---- SI: distributions in compared hours ----------------------------------------------
hrs <- matched_hours(d, 3)
d5 <- d %>% filter(hour_of_obs %in% hrs)
m5 <- d5 %>% group_by(method) %>% summarise(m = mean(fluxL_umolm2sec))
p5 <- ggplot(d5, aes(fluxL_umolm2sec, fill = method, colour = method)) +
  geom_density(alpha = 0.4, linewidth = lw_thin) +
  geom_vline(data = m5, aes(xintercept = m, colour = method), linetype = "dashed", linewidth = 0.4) +
  scale_fill_manual(values = pal, labels = lab_sys, name = NULL) +
  scale_colour_manual(values = pal, labels = lab_sys, name = NULL) +
  labs(x = flux_lab, y = "Density") + theme(legend.position = c(0.8, 0.8))
save_fig(p5, "FigS07_flux_distributions", 90, 70)
