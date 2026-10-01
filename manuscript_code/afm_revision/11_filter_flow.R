# Measurement accounting and filtering flow, with agreement between systems at each stage.
#
# Definitions (per closure = one scheduled chamber measurement), 2-31 October 2023, when
# both systems were deployed:
#   scheduled  : closures the system was programmed to make (Fluxbot: 1 per unit-hour;
#                autochamber: 2 per chamber-hour)
#   recorded   : scheduled closures with any raw CO2 data in the closure window
#   valid      : recorded closures with enough data to fit a flux (Fluxbot >= 75% of the 180-s
#                window; autochamber >= 120 s of the 220-s window) that are not chamber
#                failures (no statistically significant CO2 decline; no stuck lid, i.e. open-lid CO2
#                > 500 ppm above the other units through a saturated episode)
#   dry sensor : valid closures without a wet-sensor flag (Fluxbot in-chamber RH >= 99% in the
#                open-lid minute; condensation on the K30 optics, Pan et al. 2024)
#   retained   : dry-sensor closures that pass the per-chamber spike screen (median +/- 5 MAD);
#                these are analysed
# Derived rates: uptime = recorded / scheduled; downtime = 1 - uptime;
#   measurement success = retained / scheduled; QC retention = retained / computable;
#   measurement density = retained closures per unit per day; replicated coverage = share of
#   hours with >= 3 retained chambers of a system in a stand.

source("afm_revision/00_prep.R")
suppressPackageStartupMessages({ library(readr); library(ggplot2) })

p0 <- as.POSIXct("2023-10-02", tz = "America/New_York"); p1 <- as.POSIXct("2023-11-01", tz = "America/New_York")
n_days <- as.numeric(difftime(p1, p0, units = "days"))
units <- read_csv(file.path(pkg, "metadata", "fluxbot_units.csv"), col_types = cols(unit = col_character()))
chambers <- read_csv(file.path(pkg, "metadata", "autochamber_chambers.csv"), show_col_types = FALSE)

# ---- recorded closures from the raw records ------------------------------------------------------
fbr <- read_csv(file.path(pkg, "raw", "fluxbot_sensor_records_2023.csv.gz"), col_types = cols(unit = col_character())) %>%
  mutate(time = as.POSIXct(unix_time, origin = "1970-01-01", tz = "America/New_York")) %>%
  filter(!is.na(co2_ppm), minute(time) >= 56) %>%
  mutate(hr = floor_date(time, "hour") + 3600) %>% filter(hr >= p0, hr < p1) %>% distinct(unit, hr)
acr <- read_csv(file.path(pkg, "raw", "autochamber_co2_1hz_oct2023.csv.gz"),
                col_types = cols(datetime_est = col_character(), chamber = col_integer(), co2_ppm = col_double())) %>%
  filter(chamber %in% 1:12, !is.na(co2_ppm)) %>%
  left_join(chambers %>% select(chamber, slot_minute), by = "chamber") %>%
  mutate(time = as.POSIXct(datetime_est, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT+5"),
         sec = ((minute(time) %% 30) - slot_minute) * 60 + second(time)) %>%
  filter(sec >= 75, sec < 295) %>%
  mutate(slot = floor_date(time, "30 mins")) %>% filter(slot >= p0, slot < p1) %>% distinct(chamber, slot)

acct <- function(system) {
  flx <- read.csv(file.path(flux_dir, paste0(system, "_fluxes.csv")), colClasses = c(id = "character")) %>%
    mutate(t = round_hour(start_local)) %>% filter(t >= p0, t < p1) %>%
    mutate(lid_fail = if ("lid_fail" %in% names(.)) coalesce(as.logical(lid_fail), FALSE) else FALSE,
           decline = (LM.flux < 0 & !grepl("p-value", quality.check)) | lid_fail, wet = coalesce(as.logical(wet), FALSE))
  # "recorded" windows that yielded no flux are counted as lost at the "valid" step
  n_units <- if (system == "fluxbot") nrow(units) else nrow(chambers)
  sched <- n_units * n_days * ifelse(system == "fluxbot", 24, 48)
  rec <- if (system == "fluxbot") nrow(fbr) else nrow(acr)
  comp <- nrow(flx)
  valid <- flx %>% filter(!decline)
  dry <- valid %>% filter(!wet)
  ret <- dry %>% group_by(id) %>% filter(abs(LM.flux - median(LM.flux)) <= 5 * mad(LM.flux)) %>% ungroup()
  tibble(system, stage = c("scheduled", "recorded", "valid", "dry sensor", "retained"),
         n = c(sched, rec, nrow(valid), nrow(dry), nrow(ret))) %>%
    mutate(pct_of_scheduled = 100 * n / sched, lost = lag(n) - n, pct_lost_step = 100 * lost / lag(n))
}
acc <- bind_rows(acct("fluxbot"), acct("autochamber"))
write.csv(acc, file.path(out_dir, "measurement_accounting.csv"), row.names = FALSE)
print(acc, n = 20)
for (i in seq_len(nrow(acc))) record(paste0("acct_", acc$system[i], "_", acc$stage[i]), acc$n[i], "accounting",
                                      sprintf("%.1f%% of scheduled", acc$pct_of_scheduled[i]))
for (s in c("fluxbot", "autochamber")) {
  a <- acc %>% filter(system == s); g <- function(st) a$n[a$stage == st]
  record(paste0("uptime_pct_", s), 100 * g("recorded") / g("scheduled"), "accounting")
  record(paste0("success_pct_", s), 100 * g("retained") / g("scheduled"), "accounting")
  record(paste0("qc_retention_pct_", s), 100 * g("retained") / g("recorded"), "accounting")
  record(paste0("density_per_unit_day_", s), g("retained") / (ifelse(s == "fluxbot", nrow(units), nrow(chambers)) * n_days), "accounting")
}

# ---- agreement at each stage ------------------------------------------------------------------------
stage_agree <- function(qc) {
  d <- build_dataset(qc = qc) %>% filter(hour_of_obs < p1)
  hrs <- matched_hours(d, 3)
  s <- d %>% filter(hour_of_obs %in% hrs) %>% group_by(hour_of_obs, stand, method) %>%
    summarise(f = mean(fluxL_umolm2sec), .groups = "drop") %>% group_by(hour_of_obs, method) %>%
    summarise(f = mean(f), .groups = "drop") %>% pivot_wider(names_from = method, values_from = f)
  dd <- s %>% mutate(day = as.Date(hour_of_obs, tz = "America/New_York")) %>% group_by(day) %>%
    filter(n() >= 12) %>% summarise(a = mean(autochamber), f = mean(fluxbot))
  tibble(stage = qc, n_hours = nrow(s), offset = mean(s$fluxbot - s$autochamber),
         offset_pct = 100 * (mean(s$fluxbot) / mean(s$autochamber) - 1),
         r_hourly = cor(s$autochamber, s$fluxbot), r_daily = cor(dd$a, dd$f), n_days = nrow(dd))
}
ag <- bind_rows(lapply(c("valid", "dry", "fit"), stage_agree)) %>%
  mutate(stage = c("valid", "dry sensor", "retained"))
write.csv(ag, file.path(out_dir, "agreement_by_stage.csv"), row.names = FALSE)
print(ag)
for (i in seq_len(nrow(ag))) for (k in c("offset_pct", "r_hourly", "r_daily"))
  record(paste0("stage_", ag$stage[i], "_", k), ag[[k]][i], "accounting", "array means, hours with >= 3 chambers per system x stand")

# ---- flow diagram ---------------------------------------------------------------------------------
lab_step <- c(recorded = "no data transmitted / logger or power down",
              valid = "too few records, or chamber failure (significant CO2 decline)",
              `dry sensor` = "wet sensor (in-chamber RH >= 99% before closure)",
              retained = "spike (> 5 MAD from chamber median)")
fd <- acc %>% mutate(stage = factor(stage, levels = c("scheduled", "recorded", "valid", "dry sensor", "retained")),
                     y = 5 - as.integer(stage), x = if_else(system == "fluxbot", 1, 3.2),
                     label = sprintf("%s\n%s (%.1f%%)", tools::toTitleCase(as.character(stage)), format(n, big.mark = ","), pct_of_scheduled))
fl <- fd %>% filter(!is.na(lost)) %>% mutate(dl = sprintf("-%s: %s", format(lost, big.mark = ","), lab_step[as.character(stage)]))
agl <- ag %>% mutate(y = 5 - match(stage, levels(fd$stage)),
                     label = sprintf("offset %+.0f%%\nr hourly %.2f\nr daily %.2f", offset_pct, r_hourly, r_daily))
pflow <- ggplot() +
  geom_segment(data = fd %>% filter(stage != "retained"), aes(x = x, xend = x, y = y - 0.18, yend = y - 0.82),
               arrow = arrow(length = unit(1.5, "mm")), linewidth = 0.3) +
  geom_label(data = fd, aes(x, y, label = label), size = 2.3, label.size = 0.25, lineheight = 0.9) +
  geom_text(data = fl, aes(x = x + 0.08, y = y + 0.5, label = dl), hjust = 0, size = 1.9, colour = "grey30") +
  geom_label(data = agl, aes(x = 5.3, y = y, label = label), size = 2.1, fill = "#EEF3FA", label.size = 0.2, lineheight = 0.9) +
  annotate("text", x = c(1, 3.2, 5.3), y = 4.65, label = c("Fluxbot 2.0 (16 units)", "Autochamber (12 chambers)", "Agreement (array means)"),
           fontface = "bold", size = 2.6) +
  scale_x_continuous(limits = c(0.4, 6.1)) + scale_y_continuous(limits = c(-0.4, 4.8)) + theme_void()
ggsave(file.path(out_dir, "figures", "FigS_filter_flow.pdf"), pflow, width = 190, height = 120, units = "mm", device = cairo_pdf)
ggsave(file.path(out_dir, "figures", "FigS_filter_flow.png"), pflow, width = 190, height = 120, units = "mm", dpi = 300, device = ragg::agg_png)
write_numbers("numbers_filter_flow.csv")
