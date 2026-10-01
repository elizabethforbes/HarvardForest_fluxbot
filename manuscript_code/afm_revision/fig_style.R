# Shared figure style for all manuscript and SI figures (sourced by 05, 11, 17 and the SI figure
# scripts). Elsevier widths: single column 90 mm, 1.5 column 140 mm, double column 190 mm.
suppressPackageStartupMessages({ library(ggplot2); library(patchwork) })

# colours
pal_sys <- c(autochamber = "#3B8F63", fluxbot = "#7A7A7A")          # system colours, used everywhere
pal_sys_lab <- c("Autochamber" = "#3B8F63", "Fluxbot 2.0" = "#7A7A7A")
col_wet <- "#4575B4"                                                 # wet sensors / rain
lab_sys <- c(autochamber = "Autochamber", fluxbot = "Fluxbot 2.0")

theme_afm <- function(base_size = 8) {
  theme_classic(base_size = base_size) +
    theme(axis.text = element_text(colour = "black", size = base_size - 1),
          axis.title = element_text(size = base_size),
          strip.background = element_blank(),
          strip.text = element_text(face = "bold", size = base_size),
          legend.key.size = unit(3, "mm"), legend.text = element_text(size = base_size - 1),
          legend.title = element_text(size = base_size - 1),
          plot.title = element_text(face = "bold", size = base_size),
          plot.tag = element_text(face = "bold", size = base_size + 2))
}
theme_set(theme_afm())
tags_afm <- function() plot_annotation(tag_levels = "a")   # tag style comes from theme_afm()

save_afm <- function(p, name, width_mm, height_mm, dir = file.path(out_dir, "figures"), tif = TRUE) {
  ggsave(file.path(dir, paste0(name, ".pdf")), p, width = width_mm, height = height_mm, units = "mm", device = cairo_pdf)
  ggsave(file.path(dir, paste0(name, ".png")), p, width = width_mm, height = height_mm, units = "mm", dpi = 300, device = ragg::agg_png)
  if (tif) ggsave(file.path(dir, paste0(name, ".tif")), p, width = width_mm, height = height_mm, units = "mm", dpi = 600,
                  device = ragg::agg_tiff, compression = "lzw")
}
