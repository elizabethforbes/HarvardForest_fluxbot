# Fig. S2. The Fluxbot fit window. The lid closes at 55:00; CO2 starts to rise only after the gas
# has diffused through the PTFE envelope into the K30 (breakpoint delay t0, 2_analysis/11_q10_moisture.R).
# Main window 57:00-60:00; sensitivity 56:00-60:00 (as submitted).
#  (a) an example closure, with linear fits over both windows
#  (b) distribution of t0
#  (c) closure-level flux ratio, 56:00 vs 57:00 window, by t0
#  (d) agreement with the autochambers under each window (as-deployed dataset)
# Closure-level ratios and numbers: 2_analysis/12_window_sensitivity.R.

source("R/setup.R")
source("R/fig_style.R")
suppressPackageStartupMessages({ library(readr); library(ggplot2); library(patchwork) })
set.seed(20260930)
seg <- readRDS(file.path(out_dir, "fluxbot_sensor_delay.rds"))
both <- read.csv(file.path(out_dir, "fit_window_closures.csv"), colClasses = c(unit = "character")) %>%
  mutate(hour_of_obs = with_tz(as.POSIXct(hour_of_obs, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), "America/New_York"))

# (a) examples
raw <- read_csv(file.path(pkg, "raw", "fluxbot_sensor_records_2023.csv.gz"), col_types = cols(unit = col_character())) %>%
  filter(!co2_ppm %in% c(65535, 65533), co2_ppm > 0, co2_ppm < 10000) %>%
  mutate(time = as.POSIXct(unix_time, origin = "1970-01-01", tz = "America/New_York"), mm = minute(time) + second(time) / 60,
         hour_of_obs = floor_date(time, "hour") + 3600) %>% filter(mm >= 54)
cand <- both %>% filter(rh < 90, rise > 60, rise < 250)
ex <- cand %>% filter(t0 == 96) %>% slice_sample(n = 1) %>%
  mutate(lab = sprintf("Unit %s, %s", unit, format(hour_of_obs - 3600, "%d %b %H:55")))
exd <- raw %>% inner_join(ex %>% select(unit, hour_of_obs, lab, t0), by = c("unit", "hour_of_obs"))
fits <- bind_rows(lapply(c(56, 57), function(w) { m <- lm(co2_ppm ~ mm, data = exd %>% filter(mm >= w, mm < 60))
  tibble(mm = c(w, 60), co2 = predict(m, newdata = tibble(mm = c(w, 60))), window = paste0(w, ":00-60:00")) }))
wcol <- c("56:00-60:00" = unname(pal_window["alternative"]), "57:00-60:00" = unname(pal_window["main"]))
yr <- range(exd$co2_ppm)
# (a) one closure whose CO2 rise reaches the sensor 96 s after the lid closes
pa <- ggplot(exd, aes(mm, co2_ppm)) +
  annotate("rect", xmin = 57, xmax = 60, ymin = -Inf, ymax = Inf, fill = "grey90") +
  geom_vline(xintercept = c(55, 55 + ex$t0 / 60), linetype = c("22", "13"), colour = "grey30") +
  annotate("text", x = c(55, 55 + ex$t0 / 60), y = yr[2], label = c("lid closes", "rise reaches\nsensor"), hjust = 1.05, vjust = 1, size = txt, lineheight = 0.9) +
  annotate("text", x = 58.5, y = yr[1], label = "main window\n57:00-60:00", vjust = 0, size = txt, lineheight = 0.9) +
  geom_point(size = 0.7) + geom_line(data = fits, aes(mm, co2, colour = window), linewidth = 0.7) +
  scale_colour_manual(values = wcol, name = "Linear fit") +
  scale_x_continuous(breaks = 54:60, labels = paste0(54:60, ":00")) +
  labs(x = "Minute of hour", y = expression(CO[2] ~ (ppm)), title = ex$lab) +
  theme_afm() + theme(legend.position = "bottom", legend.margin = margin(0, 0, 0, 0), plot.title = element_text(size = 7, face = "plain"))
# (b) delay from lid closure to the start of the CO2 rise, all closures
pb <- ggplot(seg, aes(t0)) + geom_histogram(binwidth = 6, boundary = 0, fill = "grey55", colour = "white") +
  geom_vline(xintercept = c(60, 120), colour = wcol, linewidth = 0.7) +
  annotate("text", x = c(60, 120), y = Inf, label = c("56:00 start", "57:00 start"), vjust = 1.5, hjust = -0.08, size = txt, colour = wcol) +
  labs(x = "Delay from lid closure to CO2 rise (s)", y = "Closures") + theme_afm()
# (c) closure-level bias of the earlier window, by delay
pc <- ggplot(both, aes(factor(cut(t0, c(-1, 30, 60, 120, 180), labels = c("0-30", "31-60", "61-120", "121-180"))), ratio)) +
  geom_hline(yintercept = 1, colour = "grey50", linetype = "22") + geom_boxplot(outliers = FALSE, fill = "grey90", width = 0.6) +
  labs(x = "Delay (s)", y = "Flux ratio, 56:00 / 57:00 window") + theme_afm()
# (d) array-level comparison with the autochambers under each window (as deployed)
wsr <- read.csv(file.path(out_dir, "window_sensitivity.csv"))
tab <- data.frame(row.names = c("Offset vs autochamber", "r, hourly means", "r, daily means", "Diel amplitude (autochamber 21%)"),
                  `56:00-60:00` = c(sprintf("%+.1f%%", wsr$offset_pct[1]), sprintf("%.2f", wsr$r_hourly[1]), sprintf("%.2f", wsr$r_daily[1]), sprintf("%.0f%%", wsr$diel_amp_fb[1])),
                  `57:00-60:00 (main)` = c(sprintf("%+.1f%%", wsr$offset_pct[2]), sprintf("%.2f", wsr$r_hourly[2]), sprintf("%.2f", wsr$r_daily[2]), sprintf("%.0f%%", wsr$diel_amp_fb[2])),
                  check.names = FALSE)
stopifnot(grepl("56", wsr$window[1]), grepl("57", wsr$window[2]), round(wsr$diel_amp_ac[1]) == 21)
tg <- gridExtra::tableGrob(tab, theme = gridExtra::ttheme_minimal(base_size = 7, padding = unit(c(3, 2.5), "mm"),
        core = list(fg_params = list(hjust = 1, x = 0.9)), rowhead = list(fg_params = list(hjust = 0, x = 0.02, fontface = "plain")),
        colhead = list(fg_params = list(fontface = "bold", col = c(wcol[[1]], wcol[[2]])))))
tg$vp <- grid::viewport(y = 0.95, just = "top", height = sum(tg$heights))
pd <- wrap_elements(full = tg)
pfig <- (pa | pb) / (pc | pd) + plot_annotation(tag_levels = "a")
save_afm(pfig, "FigS02_fit_window", 190, 130, tif = FALSE)
