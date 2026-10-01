# Shared figure style for all manuscript and SI figures (sourced by 05, 11, 17 and the SI figure
# scripts). Elsevier widths: single column 90 mm, 1.5 column 140 mm, double column 190 mm.
suppressPackageStartupMessages({ library(ggplot2); library(patchwork) })

# ---- colours (one meaning per colour, used in every figure) ----
pal_sys <- c(autochamber = "#3B8F63", fluxbot = "#7A7A7A")          # the two systems
pal_sys_lab <- c("Autochamber" = "#3B8F63", "Fluxbot 2.0" = "#7A7A7A")
lab_sys <- c(autochamber = "Autochamber", fluxbot = "Fluxbot 2.0")
pal_stand <- c("Autochamber Stand 1" = "#1F6E43", "Autochamber Stand 2" = "#7FBF96",   # dark = stand 1,
               "Fluxbot 2.0 Stand 1" = "#4D4D4D", "Fluxbot 2.0 Stand 2" = "#A6A6A6")   # light = stand 2
col_wet <- "#4575B4"          # wet sensors, rain, water vapour
col_accent <- "#F4A582"       # daily means and other highlighted points
col_fit <- "black"            # model fits (SMA, exponential, GAM)
col_ref <- "grey40"           # 1:1 and zero reference lines (dashed)
col_cross <- "#2F5D9E"        # cross-system comparisons (benchmarks)
pal_lab <- c(covered = "#2C7BB6", uncovered = "#E08214", reference = "black")         # lab tests only
pal_window <- c(main = "black", alternative = "#D6604D")                              # fit windows
pal_div <- c(low = "#B2182B", mid = "white", high = "#2166AC")                         # signed offsets
pal_state <- c(">= 3 units" = "#2C7BB6", "1-2 units" = "#ABD9E9", "measured, removed by QC" = "#FDAE61", "no data (down)" = "#D7191C")

# ---- transparency and sizes ----
a_dense <- 0.4     # many individual fluxes
a_mean <- 0.65     # hourly or daily means, unit-level points
a_band <- 0.15     # confidence bands, shading
pt_dense <- 0.55; pt_mean <- 0.9; pt_big <- 2.1       # point sizes
lw_main <- 0.6; lw_thin <- 0.35                        # line widths
txt <- 2.5                                             # annotation text (~7 pt)

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
