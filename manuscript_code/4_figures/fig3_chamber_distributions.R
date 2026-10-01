# Fig. 3. Flux distributions of each chamber, grouped by array (system x stand), as deployed.
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

# ---- Fig 3: chamber-level distributions ----------------------------------------------
# four arrays (system x stand); chambers ordered by mean flux within each array
d3 <- d %>% mutate(array = factor(paste0(lab_sys[as.character(method)], ", ", stand_label),
                                  levels = c("Autochamber, Stand 1", "Autochamber, Stand 2", "Fluxbot 2.0, Stand 1", "Fluxbot 2.0, Stand 2")),
                   unit = sub("^(autochamber|fluxbot|fluxes_bot)", "", as.character(id)))
ord <- d3 %>% group_by(array, unit) %>% summarise(m = mean(fluxL_umolm2sec), .groups = "drop") %>% arrange(array, m) %>% mutate(key = paste(array, unit))
d3 <- d3 %>% mutate(key = factor(paste(array, unit), levels = ord$key))
p3 <- ggplot(d3, aes(fluxL_umolm2sec, key)) +
  geom_jitter(aes(colour = method), height = 0.2, size = pt_dense, alpha = a_dense, stroke = 0) +
  geom_boxplot(outliers = FALSE, fill = NA, linewidth = 0.3, width = 0.6) +
  facet_wrap(~ array, scales = "free_y", ncol = 2) +
  scale_y_discrete(labels = function(k) sub(".* ", "", k)) +
  scale_colour_manual(values = pal, guide = "none") + coord_cartesian(xlim = c(0, 7.5)) +
  labs(x = flux_lab, y = "Chamber or unit")
save_fig(p3, "Fig3_chamber_distributions", 140, 130)
