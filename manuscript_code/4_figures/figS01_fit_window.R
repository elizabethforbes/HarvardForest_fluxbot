# Fig. S1. The Fluxbot fit window. The lid closes at 55:00; CO2 starts to rise only after the gas
# has diffused through the PTFE envelope into the K30 (breakpoint delay t0, 2_analysis/11_q10_moisture.R).
# Main window 57:00-60:00; sensitivity 56:00-60:00 (as submitted).
#  (a) example closures across the range of delays, with linear fits over both windows
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
ex <- bind_rows(lapply(c(24, 54, 96), function(t) cand %>% filter(t0 == t) %>% slice_sample(n = 1))) %>%
  mutate(lab = sprintf("Unit %s, %s\nrise starts %d s after lid closure", unit, format(hour_of_obs - 3600, "%d %b %H:55"), t0)) %>%
  arrange(t0) %>% mutate(lab = factor(lab, levels = lab))
exd <- raw %>% inner_join(ex %>% select(unit, hour_of_obs, lab, t0), by = c("unit", "hour_of_obs"))
fits <- bind_rows(lapply(c(56, 57), function(w) exd %>% filter(mm >= w, mm < 60) %>% group_by(lab) %>%
  group_modify(~ { m <- lm(co2_ppm ~ mm, data = .x); tibble(mm = c(w, 60), co2 = predict(m, newdata = tibble(mm = c(w, 60)))) }) %>%
  mutate(window = paste0(w, ":00-60:00"))))
wcol <- c("56:00-60:00" = unname(pal_window["alternative"]), "57:00-60:00" = unname(pal_window["main"]))
pa <- ggplot(exd, aes(mm, co2_ppm)) +
  annotate("rect", xmin = 57, xmax = 60, ymin = -Inf, ymax = Inf, fill = unname(pal_window["main"]), alpha = 0.10) +
  geom_vline(xintercept = 55, linetype = "22", colour = "grey40") +
  geom_vline(data = ex, aes(xintercept = 55 + t0 / 60), linetype = "13", colour = "grey20") +
  geom_point(size = 0.6) + geom_line(data = fits, aes(mm, co2, colour = window), linewidth = 0.6) +
  facet_wrap(~ lab, scales = "free_y") + scale_colour_manual(values = wcol, name = "Linear fit") +
  scale_x_continuous(breaks = 54:60, labels = paste0(54:60, ":00")) +
  labs(x = "Minute of hour", y = expression(CO[2] ~ (ppm))) + theme_afm() + theme(legend.position = "bottom", strip.background = element_blank())
pb <- ggplot(seg, aes(t0)) + geom_histogram(binwidth = 6, boundary = 0, fill = "grey55", colour = "white") +
  geom_vline(xintercept = c(60, 120), colour = wcol, linewidth = 0.7) +
  annotate("text", x = c(60, 120), y = Inf, label = c("56:00", "57:00"), vjust = 1.5, hjust = -0.1, size = 2.5, colour = wcol) +
  labs(x = "Delay from lid closure to start of CO2 rise (s)", y = "Closures") + theme_afm()
pc <- ggplot(both, aes(factor(cut(t0, c(-1, 30, 60, 120, 180), labels = c("0-30", "31-60", "61-120", "121-180"))), ratio)) +
  geom_hline(yintercept = 1, colour = "grey50") + geom_boxplot(outlier.size = 0.3, fill = "grey90", width = 0.6) +
  coord_cartesian(ylim = c(0.5, 1.8)) + labs(x = "Delay (s)", y = "Flux, 56:00 / 57:00 window") + theme_afm()
ws <- read.csv(file.path(out_dir, "window_sensitivity.csv")) %>%
  transmute(window = sub(" .*", "", window), `Offset (%)` = offset_pct, `r hourly` = r_hourly, `r daily` = r_daily,
            `Fluxbot diel amplitude (%)` = diel_amp_fb) %>%
  tidyr::pivot_longer(-window) %>% mutate(name = factor(name, levels = unique(name)))
ac_amp <- read.csv(file.path(out_dir, "window_sensitivity.csv"))$diel_amp_ac[1]
pd <- ggplot(ws, aes(window, value, fill = window)) + geom_col(width = 0.6) + facet_wrap(~ name, scales = "free_y", nrow = 1) +
  geom_hline(data = tibble(name = factor("Fluxbot diel amplitude (%)", levels = levels(ws$name)), y = ac_amp), aes(yintercept = y), linetype = "22") +
  geom_text(data = tibble(name = factor("Fluxbot diel amplitude (%)", levels = levels(ws$name)), y = ac_amp), aes(x = 0.5, y = y, label = "autochamber"),
            inherit.aes = FALSE, hjust = 0, vjust = -0.4, size = 2) +
  geom_text(aes(label = ifelse(abs(value) < 1.5, sprintf("%.2f", value), sprintf("%.1f", value)), vjust = if_else(value < 0, 1.3, -0.3)), size = txt) +
  scale_fill_manual(values = c("56:00-60:00" = unname(pal_window["alternative"]), "57:00-60:00" = unname(pal_window["main"])), guide = "none") +
  scale_y_continuous(expand = expansion(mult = 0.2)) +
  labs(x = NULL, y = NULL) + theme_afm(7) + theme(strip.background = element_blank(), axis.text.x = element_text(angle = 20, hjust = 1))
pfig <- pa / (pb | pc) / pd + plot_layout(heights = c(1.2, 1, 0.9)) + plot_annotation(tag_levels = "a")
save_afm(pfig, "FigS01_fit_window", 190, 190, tif = FALSE)
