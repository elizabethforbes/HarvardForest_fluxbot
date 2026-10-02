# Fig. S1. Site map: Fluxbot GPS positions (17 Nov 2023) in the two stands (points; boxes = inset extents), with the Fisher meteorological
# station (HF001) and the EMS and HEM eddy-covariance towers (Harvard Forest Data Archive coordinates:
# HF001 42.53311 N 72.18968 W; EMS tower, HF004, 42.537755 N 72.171478 W; HEM tower, HF103, 42.539 N 72.180 W,
# given to 0.001 degrees, about +/- 100 m). Autochamber positions were not surveyed; the autochambers are in
# the same stands as the Fluxbots. Insets: each unit's GPS position with its reported horizontal accuracy
# (4-8 m) as a circle. Basemaps: Esri World Topographic Map and World Imagery (downloaded at run time).
source("R/setup.R")
source("R/fig_style.R")
suppressPackageStartupMessages({ library(sf); library(maptiles); library(tidyterra); library(ggplot2); library(patchwork); library(ggspatial); library(ggrepel) })
u <- read.csv(file.path(pkg, "metadata", "fluxbot_units.csv"), colClasses = c(unit = "character")) %>%
  mutate(stand_lab = if_else(stand == "stand 1", "Stand 1 (Bigelow Brook)", "Stand 2 (Hemlock tower)"))
fb <- st_as_sf(u, coords = c("longitude", "latitude"), crs = 4326)
utm <- 32618
sites <- st_as_sf(data.frame(name = c("HF001 met station", "EMS tower", "HEM tower (approx.)"),
                             lon = c(-72.18968, -72.171478, -72.180), lat = c(42.53311, 42.537755, 42.539)),
                  coords = c("lon", "lat"), crs = 4326)
cent <- fb %>% group_by(stand_lab) %>% summarise(geometry = st_centroid(st_union(geometry)), .groups = "drop")
record("map_stand_separation_m", as.numeric(st_distance(cent)[1, 2]), "site", "distance between Fluxbot stand centroids")
for (i in 1:2) record(paste0("map_hf001_to_stand", i, "_m"), as.numeric(st_distance(cent[i, ], sites[1, ])), "site", "HF001 to Fluxbot stand centroid")
record("map_gps_accuracy_min_m", min(u$gps_horizontal_accuracy_m), "site", "reported GPS horizontal accuracy")
record("map_gps_accuracy_max_m", max(u$gps_horizontal_accuracy_m), "site", "reported GPS horizontal accuracy")
stand_pal <- c("Stand 1 (Bigelow Brook)" = unname(pal_stand["Autochamber Stand 1"]), "Stand 2 (Hemlock tower)" = unname(pal_stand["Autochamber Stand 2"]))
box <- function(g, m) st_bbox(st_buffer(st_union(g) |> st_transform(utm), m)) |> st_as_sfc() |> st_transform(4326)
# stand boxes (extent of each inset)
inset_m <- 14
boxes <- bind_rows(lapply(names(stand_pal), function(st) st_sf(stand_lab = st, geometry = box(st_geometry(fb %>% filter(stand_lab == st)), inset_m))))
boxes$tag <- c("b", "c")
ov_ext <- box(c(st_geometry(fb), st_geometry(sites)), 180)
ov <- ggplot() + geom_spatraster_rgb(data = get_tiles(ov_ext, provider = "Esri.WorldTopoMap", zoom = 16, crop = TRUE)) +
  geom_sf(data = fb, aes(fill = stand_lab), shape = 21, colour = "white", stroke = 0.2, size = 1.1, show.legend = FALSE) +
  geom_sf(data = boxes, aes(colour = stand_lab), fill = NA, linewidth = 0.7) +
  geom_sf_text(data = boxes, aes(label = tag), nudge_y = 0.0004, fontface = "bold", size = txt + 0.5) +
  geom_sf(data = sites, aes(shape = name), size = 2.4, fill = "white", stroke = 0.6) +
  geom_text_repel(data = sites, aes(label = name, geometry = geometry), stat = "sf_coordinates", size = txt - 0.2,
                  min.segment.length = 0, box.padding = 0.5, seed = 1) +
  scale_colour_manual(values = stand_pal, name = NULL) + scale_fill_manual(values = stand_pal, guide = "none") + scale_shape_manual(values = c(24, 21, 22), guide = "none") +
  annotation_scale(location = "bl", height = unit(1.5, "mm"), text_cex = 0.6) +
  coord_sf(expand = FALSE) + theme_void(base_size = 8) +
  theme(legend.position = "bottom", plot.background = element_rect(fill = "white", colour = NA))
zoom <- function(st) {
  x <- fb %>% filter(stand_lab == st)
  acc <- st_buffer(st_transform(x, utm), x$gps_horizontal_accuracy_m) |> st_transform(4326)
  ggplot() + geom_spatraster_rgb(data = get_tiles(box(st_geometry(x), inset_m), provider = "Esri.WorldImagery", zoom = 20, crop = TRUE)) +
    geom_sf(data = acc, fill = stand_pal[st], colour = "white", alpha = 0.18, linewidth = 0.25) +
    geom_sf(data = x, colour = "white", fill = stand_pal[st], shape = 21, size = 1.8) +
    geom_text_repel(data = x, aes(label = unit, geometry = geometry), stat = "sf_coordinates", size = txt - 0.3, colour = "white",
                    bg.colour = "black", bg.r = 0.12, min.segment.length = 0, segment.colour = "white", box.padding = 0.3, seed = 1) +
    annotation_scale(location = "br", height = unit(1.2, "mm"), text_cex = 0.55, text_col = "white", bar_cols = c("white", "grey40")) +
    coord_sf(expand = FALSE) + labs(title = st) + theme_void(base_size = 8) +
    theme(plot.title = element_text(size = 7, face = "bold"), plot.background = element_rect(fill = "white", colour = NA))
}
fig <- (ov | (zoom("Stand 1 (Bigelow Brook)") / zoom("Stand 2 (Hemlock tower)"))) + plot_layout(widths = c(1.35, 1)) +
  plot_annotation(tag_levels = "a") & theme(plot.tag = element_text(face = "bold", size = 10))
save_afm(fig, "FigS01_site_map", 190, 130, tif = FALSE)
print(write_numbers() %>% select(key, value), row.names = FALSE)
