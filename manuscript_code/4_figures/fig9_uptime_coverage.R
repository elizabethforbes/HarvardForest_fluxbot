# Fig. 9. Measurement success per unit and system, daily share of units reporting, and the hourly state of
# each stand array. Tables from 2_analysis/14_resilience.R.
source("R/setup.R")
source("R/fig_style.R")
suppressPackageStartupMessages({ library(ggplot2); library(patchwork) })
set.seed(20260930)
pal <- pal_sys
stl <- c(healthy = "Stand 1", unhealthy = "Stand 2")
p0 <- as.POSIXct("2023-10-02", tz = "America/New_York"); p1 <- as.POSIXct("2023-11-01", tz = "America/New_York")
unit_rate <- read.csv(file.path(out_dir, "unit_success.csv"))
st_h <- read.csv(file.path(out_dir, "stand_hour_states.csv")) %>%
  mutate(hour_of_obs = with_tz(as.POSIXct(hour_of_obs, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), "America/New_York"),
         state = factor(state, levels = c(">= 3 units", "1-2 units", "measured, removed by QC", "no data (down)")),
         series = factor(series, levels = c("Autochamber Stand 1", "Autochamber Stand 2", "Fluxbot 2.0 Stand 1", "Fluxbot 2.0 Stand 2")))

# colours follow the system palette used in all figures (autochamber green, Fluxbot grey):
# dark shade + solid = stand 1, light shade + dashed = stand 2
ser_pal <- pal_stand
ser_lty <- c("Autochamber Stand 1" = "solid", "Autochamber Stand 2" = "22", "Fluxbot 2.0 Stand 1" = "solid", "Fluxbot 2.0 Stand 2" = "22")
ur <- unit_rate %>% mutate(unit = reorder(sub("^(autochamber|fluxbot|fluxes_bot)", "", id), success))
pa <- ggplot(ur, aes(unit, 100 * success, fill = method)) + geom_col(width = 0.8) +
  facet_grid(~ lab_sys[method], scales = "free_x", space = "free_x") + scale_fill_manual(values = pal, guide = "none") +
  labs(x = "Chamber or unit", y = "Measurement success\n(% of intended closures)") + scale_y_continuous(limits = c(0, 100)) + theme_afm() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6))
pb <- ggplot(ur, aes(c(autochamber = "Auto-\nchamber", fluxbot = "Fluxbot\n2.0")[method], 100 * success, fill = method)) + geom_boxplot(outliers = FALSE, width = 0.6, alpha = a_mean) +
  geom_jitter(width = 0.1, size = pt_mean, alpha = a_mean) + scale_fill_manual(values = pal, guide = "none") +
  scale_y_continuous(limits = c(0, 100)) + labs(x = NULL, y = NULL) + theme_afm()
daily <- st_h %>% mutate(day = as.Date(hour_of_obs, tz = "America/New_York")) %>% group_by(series, day) %>%
  summarise(share = 100 * mean(n_ok / n_units), .groups = "drop")
pc <- ggplot(daily, aes(day, share, colour = series, linetype = series)) + geom_line(linewidth = 0.7) +
  scale_colour_manual(values = ser_pal, name = NULL) + scale_linetype_manual(values = ser_lty, name = NULL) +
  scale_y_continuous(limits = c(0, 100)) + scale_x_date(date_labels = "%d %b", expand = c(0, 0)) +
  labs(x = NULL, y = "Units reporting\n(% of stand's units, daily)") +
  theme_afm() + theme(legend.position = "top", legend.key.width = unit(8, "mm"))
# timeline: the same state colours for both systems, from good (blue, >= 3 units) to bad
# (red, no data)
st_h <- st_h %>% mutate(fill_key = as.character(state))
# good-to-bad diverging scale (ColorBrewer RdYlBu, colourblind-safe; avoids the system greens/greys)
fill_pal <- pal_state
rows <- tibble(series = factor(levels(st_h$series), levels = levels(st_h$series)))
pd <- ggplot(st_h, aes(hour_of_obs, forcats::fct_rev(series))) +
  geom_tile(aes(fill = fill_key), height = 0.8) +
  geom_tile(data = rows, aes(x = p0 + (p1 - p0) / 2, y = forcats::fct_rev(series)), width = as.numeric(difftime(p1, p0, units = "secs")),
            height = 0.8, fill = NA, colour = "grey60", linewidth = 0.3, inherit.aes = FALSE) +
  scale_fill_manual(values = fill_pal, breaks = names(fill_pal), name = NULL) +
  guides(fill = guide_legend(nrow = 1, override.aes = list(colour = "grey60", linewidth = 0.3))) +
  scale_x_datetime(date_labels = "%d %b", expand = c(0, 0)) + labs(x = NULL, y = NULL) +
  theme_afm() + theme(legend.position = "bottom", axis.line.y = element_blank(), axis.ticks.y = element_blank())
fig <- ((pa + pb + plot_layout(widths = c(4, 1))) / pc / pd) + plot_layout(heights = c(1, 0.9, 0.55)) + tags_afm()
save_afm(fig, "Fig9_uptime_coverage", 190, 175)
