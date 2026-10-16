# Fig. S8. The data record of every chamber and array-level resilience.
#  (a) every chamber, every hour, coloured by what happened to its data (main dataset; removed as wet sensor;
#      removed by QC; no data), in the colours of main Fig. 6a. Autochamber hours count as in the main
#      dataset if either of their two closures is.
#  (b) coverage by number of units reporting, observed and expected under independent failures
#      (curves from 2_analysis/13_resilience.R)
source("R/setup.R")
source("R/fig_style.R")
suppressPackageStartupMessages({ library(ggplot2); library(patchwork) })
pal <- pal_sys
stl <- c(healthy = "Stand 1", unhealthy = "Stand 2")
p0 <- as.POSIXct("2023-10-02", tz = "America/New_York"); p1 <- as.POSIXct("2023-11-01", tz = "America/New_York")

# ---- (a) chamber x hour record ---------------------------------------------------------------------
rc <- function(f) read_csv(file.path(clean_dir, f), show_col_types = FALSE, col_types = cols(id = "c", .default = col_guess()))
cl <- bind_rows(rc("closures_fluxbot.csv") %>% mutate(method = "fluxbot"), rc("closures_autochamber.csv") %>% mutate(method = "autochamber")) %>%
  mutate(hr = floor_date(with_tz(as.POSIXct(start_utc, tz = "UTC"), "America/New_York"), "hour")) %>% filter(hr >= p0, hr < p1) %>%
  mutate(rank = case_when(in_screened ~ 4L, in_deployed ~ 3L, TRUE ~ 2L)) %>%
  group_by(method, id, hr) %>% summarise(rank = max(rank), .groups = "drop")
roster <- bind_rows(read_csv(file.path(pkg, "metadata", "fluxbot_units.csv"), show_col_types = FALSE, col_types = cols(unit = "c")) %>%
                      transmute(method = "fluxbot", stand_code, id = unit),
                    read_csv(file.path(pkg, "metadata", "autochamber_chambers.csv"), show_col_types = FALSE) %>%
                      transmute(method = "autochamber", stand_code, id = as.character(chamber)))
fate_lab <- c("No data", "Removed by QC", "Removed: wet sensor", "Main dataset")
grid <- tidyr::crossing(roster, hr = seq(p0, p1 - 3600, by = "hour")) %>% left_join(cl, by = c("method", "id", "hr")) %>%
  mutate(fate = factor(coalesce(rank, 1L), levels = 1:4, labels = fate_lab),
         arr = factor(paste0(lab_sys[method], ", ", stl[stand_code]), levels = c("Autochamber, Stand 1", "Autochamber, Stand 2", "Fluxbot 2.0, Stand 1", "Fluxbot 2.0, Stand 2")),
         row = reorder(paste(method, id), as.numeric(id)))
fcol <- c("No data" = scales::alpha(unname(pal_state["no data (down)"]), 0.6), "Removed by QC" = scales::alpha(unname(pal_state["measured, removed by QC"]), 0.6),
          "Removed: wet sensor" = scales::alpha(col_wet, 0.6), "Main dataset" = "grey88")   # muted, as in main Fig. 6a
pa <- ggplot(grid, aes(hr, row, fill = fate)) + geom_raster() +
  facet_grid(arr ~ ., scales = "free_y", space = "free_y") +
  scale_fill_manual(values = fcol, name = NULL, breaks = rev(fate_lab)) +
  scale_x_datetime(date_labels = "%d %b", expand = c(0, 0)) + scale_y_discrete(labels = function(x) sub(".* ", "", x)) +
  labs(x = NULL, y = "Chamber or unit") + theme_afm() +
  theme(legend.position = "top", strip.text.y = element_text(angle = 0, hjust = 0, face = "bold", size = 6.5), axis.text.y = element_text(size = 5))

# ---- (b) coverage by number of units, observed vs independent --------------------------------------------
curves <- read.csv(file.path(out_dir, "resilience_curves.csv"))
pb <- ggplot(curves %>% mutate(stand = stl[stand]), aes(k, 100 * observed, colour = method)) +
  geom_line(aes(y = 100 * independent), linetype = "22", linewidth = 0.4) + geom_line(linewidth = 0.6) + geom_point(size = 1) +
  facet_wrap(~stand) + scale_colour_manual(values = pal, labels = lab_sys, name = NULL) +
  scale_x_continuous(breaks = 1:8) + labs(x = "Units reporting (at least k)", y = "Stand-hours (%)") +
  theme_afm() + theme(legend.position = "bottom", strip.background = element_blank())
save_afm(pa / pb + plot_layout(heights = c(1.6, 1)) + plot_annotation(tag_levels = "a"), "FigS08_resilience", 190, 200, tif = FALSE)
