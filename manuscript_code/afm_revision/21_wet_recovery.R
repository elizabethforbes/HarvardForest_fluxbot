# Wet K30 sensors in the field: how long they stay wet, whether and how fast they recover, and how
# much data loss and bias water causes.
#  - wet hour: in-chamber RH >= 99% in the open-lid minute (54:00-55:00), the main QC flag.
#  - baseline anomaly: the unit's open-lid CO2 minus the median of the other units in its stand-hour
#    (16_vent_wet.R); ~0 for a healthy sensor.
#  - flux ratio: closure flux / stand-hour autochamber mean (log), for closures passing every QC
#    step except the wet flag.
# Episodes are runs of wet hours per unit (gaps of up to 3 h without a record are bridged).
# Recovery is tracked from the last wet hour of each episode (t = 0).

source("afm_revision/00_prep.R")
suppressPackageStartupMessages({ library(readr); library(ggplot2); library(patchwork); library(lme4) })
p0 <- as.POSIXct("2023-10-02", tz = "America/New_York"); p1 <- as.POSIXct("2023-11-01", tz = "America/New_York")
units <- read_csv(file.path(pkg, "metadata", "fluxbot_units.csv"), col_types = cols(unit = col_character()))

# ---- unit-hour table ----------------------------------------------------------------------------
raw <- read_csv(file.path(pkg, "raw", "fluxbot_sensor_records_2023.csv.gz"), col_types = cols(unit = col_character())) %>%
  mutate(time = as.POSIXct(unix_time, origin = "1970-01-01", tz = "America/New_York"), mm = minute(time) + second(time) / 60) %>%
  filter(mm >= 54) %>% mutate(hour_of_obs = floor_date(time, "hour") + 3600) %>% filter(hour_of_obs >= p0, hour_of_obs < p1)
uh_raw <- raw %>% group_by(unit, hour_of_obs) %>%
  summarise(err_pct = 100 * mean(co2_ppm %in% c(65535, 65533)),
            rh = mean(rh_pct[mm < 55 & rh_pct >= 0 & rh_pct <= 100.5], na.rm = TRUE), .groups = "drop") %>%
  mutate(rh = if_else(is.nan(rh), NA_real_, rh))
grid <- tidyr::expand_grid(unit = units$unit, hour_of_obs = seq(p0, p1 - 3600, by = 3600)) %>%
  left_join(units %>% select(unit, stand = stand_code), by = "unit") %>%
  left_join(uh_raw, by = c("unit", "hour_of_obs")) %>% mutate(recorded = !is.na(err_pct))
cl <- readRDS(file.path(out_dir, "fluxbot_baseline_anomaly.rds")) %>% select(unit, hour_of_obs, anom)
d_all <- build_dataset(qc = "fit_nowet")
ach <- d_all %>% filter(method == "autochamber") %>% group_by(stand, hour_of_obs) %>% filter(n() >= 3) %>%
  summarise(ac = mean(fluxL_umolm2sec), .groups = "drop") %>% mutate(stand = as.character(stand))
fbq <- d_all %>% filter(method == "fluxbot") %>% transmute(unit = gsub("[^0-9]", "", as.character(id)), hour_of_obs, flux = fluxL_umolm2sec)
uh <- grid %>% left_join(cl, by = c("unit", "hour_of_obs")) %>% left_join(fbq, by = c("unit", "hour_of_obs")) %>%
  left_join(ach, by = c("stand", "hour_of_obs")) %>%
  mutate(wet = !is.na(rh) & rh >= 99, lr = if_else(flux > 0.1 & ac > 0.1, log(flux / ac), NA_real_)) %>% arrange(unit, hour_of_obs)
stopifnot(nrow(uh) == nrow(grid))

# ---- wet episodes -------------------------------------------------------------------------------
episodes <- uh %>% group_by(unit) %>% group_modify(function(g, key) {
  w <- which(g$wet); if (!length(w)) return(tibble())
  brk <- c(TRUE, diff(w) > 4)                                   # bridge gaps of up to 3 h
  tibble(start_i = w[brk], end_i = w[c(brk[-1], TRUE)]) %>%
    mutate(start = g$hour_of_obs[start_i], end = g$hour_of_obs[end_i], length_h = end_i - start_i + 1,
           wet_h = mapply(function(a, b) sum(g$wet[a:b]), start_i, end_i),
           anom_end = mapply(function(a, b) median(g$anom[max(a, b - 2):b], na.rm = TRUE), start_i, end_i),
           anom_ep = mapply(function(a, b) median(g$anom[a:b], na.rm = TRUE), start_i, end_i))
}) %>% ungroup() %>%
  # open-lid CO2 hundreds to thousands of ppm above the other units for the whole episode = headspace not
  # venting (lid stuck shut; field log: unit 114 stuck shut 7 Oct), not a wet sensor
  mutate(type = if_else(!is.na(anom_ep) & anom_ep > 500, "lid failure", "wet sensor"))
print(episodes %>% count(type, unit) %>% filter(type == "lid failure"))
for (ty in c("lid failure", "wet sensor")) { e <- episodes %>% filter(type == ty); tg <- gsub(" ", "_", ty)
  record(paste0("ep_", tg, "_n"), nrow(e), "wet_recovery"); record(paste0("ep_", tg, "_wet_h"), sum(e$wet_h), "wet_recovery")
  record(paste0("ep_", tg, "_length_median_h"), median(e$length_h), "wet_recovery"); record(paste0("ep_", tg, "_units"), n_distinct(e$unit), "wet_recovery") }
record("wet_hours_total", sum(uh$wet), "wet_recovery"); record("wet_pct_recorded_hours", 100 * mean(uh$wet[uh$recorded]), "wet_recovery")
record("wet_episodes_n", nrow(episodes), "wet_recovery"); record("wet_episode_length_median_h", median(episodes$length_h), "wet_recovery")
record("wet_episode_length_p90_h", quantile(episodes$length_h, .9), "wet_recovery")
record("wet_hours_in_episodes_ge24h_pct", 100 * sum(episodes$wet_h[episodes$length_h >= 24]) / sum(episodes$wet_h), "wet_recovery")
record("wet_units_with_any", n_distinct(episodes$unit), "wet_recovery")

# ---- event-time composites and per-episode recovery ---------------------------------------------
ev <- bind_rows(lapply(seq_len(nrow(episodes)), function(i) {
  e <- episodes[i, ]; g <- uh %>% filter(unit == e$unit)
  nxt <- episodes %>% filter(unit == e$unit, start > e$end) %>% summarise(m = suppressWarnings(min(start))) %>% pull(m)
  g %>% filter(hour_of_obs >= e$start - 24 * 3600, hour_of_obs <= e$end + 72 * 3600) %>%
    mutate(ep = i, t_end = as.numeric(difftime(hour_of_obs, e$end, units = "hours")),
           t_start = as.numeric(difftime(hour_of_obs, e$start, units = "hours")),
           in_ep = hour_of_obs >= e$start & hour_of_obs <= e$end,
           censored = is.finite(nxt) & hour_of_obs >= nxt, length_h = e$length_h, type = e$type)
})) %>% filter(!censored)
# reference: healthy behaviour = dry closures more than 72 h from any wet hour
near_wet <- uh %>% group_by(unit) %>% mutate(last_wet = { lw <- if_else(wet, hour_of_obs, as.POSIXct(NA)); zoo::na.locf(lw, na.rm = FALSE) },
                                             next_wet = { nw <- if_else(wet, hour_of_obs, as.POSIXct(NA)); zoo::na.locf(nw, na.rm = FALSE, fromLast = TRUE) }) %>%
  ungroup() %>% mutate(h_since_wet = as.numeric(difftime(hour_of_obs, last_wet, units = "hours")),
                       h_to_wet = as.numeric(difftime(next_wet, hour_of_obs, units = "hours")))
ref <- near_wet %>% filter(!wet, (is.na(h_since_wet) | h_since_wet > 72), (is.na(h_to_wet) | h_to_wet > 24))
record("ref_anom_median", median(ref$anom, na.rm = TRUE), "wet_recovery", "dry, >72 h after wet")
record("ref_anom_p90", quantile(ref$anom, .9, na.rm = TRUE), "wet_recovery")
record("ref_ratio_median", exp(median(ref$lr, na.rm = TRUE)), "wet_recovery")
thr <- 50   # ppm; ~p90 of healthy sensors is reported above
rec <- ev %>% filter(t_end >= 1, !wet) %>% group_by(ep) %>% arrange(t_end) %>%
  summarise(rec_h = { ok <- !is.na(anom) & anom <= thr; if (any(ok)) t_end[which(ok)[1]] else NA_real_ },
            followed_h = max(t_end), .groups = "drop") %>% left_join(episodes %>% mutate(ep = row_number()), by = "ep")
write.csv(rec, file.path(out_dir, "wet_episode_recovery.csv"), row.names = FALSE)
hi <- rec %>% filter(anom_end > 100, type == "wet sensor")
record("rec_episodes_anom_end_gt100_n", nrow(hi), "wet_recovery", "episodes whose last wet hours read >100 ppm high")
record("rec_h_median_anom_end_gt100", median(hi$rec_h, na.rm = TRUE), "wet_recovery", paste("hours after last wet hour until anomaly <=", thr, "ppm"))
record("rec_h_p75_anom_end_gt100", quantile(hi$rec_h, .75, na.rm = TRUE), "wet_recovery")
record("rec_pct_within_3h", 100 * mean(hi$rec_h <= 3, na.rm = TRUE), "wet_recovery")
record("rec_pct_within_12h", 100 * mean(hi$rec_h <= 12, na.rm = TRUE), "wet_recovery")
record("rec_pct_not_recovered", 100 * mean(is.na(hi$rec_h)), "wet_recovery", "not recovered before data end or next episode")
ct <- suppressWarnings(cor.test(hi$length_h, hi$rec_h, method = "spearman", exact = FALSE))
record("rec_vs_length_rho", ct$estimate, "wet_recovery"); record("rec_vs_length_p", ct$p.value, "wet_recovery")
print(summary(hi$rec_h)); print(table(cut(hi$rec_h, c(0, 1, 3, 6, 12, 24, 72)), useNA = "ifany"))

ev_all <- ev; ev <- ev %>% filter(type == "wet sensor")   # composites, post-wet and build-up: wet-sensor episodes
comp_end <- ev %>% filter(t_end >= -12, t_end <= 48) %>% group_by(t_end) %>%
  summarise(anom_lo = quantile(anom, .25, na.rm = TRUE), anom_hi = quantile(anom, .75, na.rm = TRUE), anom = median(anom, na.rm = TRUE),
            ratio = exp(median(lr, na.rm = TRUE)), n_ratio = sum(!is.na(lr)), wet_share = mean(wet), n = n(), .groups = "drop")
# anomaly and ratio in the closures right after a wet episode (dry flag, so retained by the main QC)
for (w in list(c(1, 3), c(4, 12), c(13, 48))) {
  z <- ev %>% filter(!wet, t_end >= w[1], t_end <= w[2])
  record(sprintf("post_%d_%dh_anom_median", w[1], w[2]), median(z$anom, na.rm = TRUE), "wet_recovery")
  record(sprintf("post_%d_%dh_ratio_median", w[1], w[2]), exp(median(z$lr, na.rm = TRUE)), "wet_recovery")
}
comp_start <- ev %>% filter(in_ep, t_start <= 72) %>% mutate(tb = cut(t_start, c(-1, 2, 6, 12, 24, 48, 72), labels = c("0-2", "3-6", "7-12", "13-24", "25-48", "49-72"))) %>%
  group_by(tb) %>% summarise(pct_gt100 = 100 * mean(anom > 100, na.rm = TRUE), anom = median(anom, na.rm = TRUE),
                             ratio = exp(median(lr, na.rm = TRUE)), n = n(), .groups = "drop")
print(comp_start)
write.csv(comp_start, file.path(out_dir, "wet_buildup.csv"), row.names = FALSE)
for (i in seq_len(nrow(comp_start))) { record(paste0("buildup_pct_gt100_", gsub("-", "_", comp_start$tb[i]), "h"), comp_start$pct_gt100[i], "wet_recovery", "% of wet hours with anomaly > 100 ppm, by hours since episode start")
  record(paste0("buildup_ratio_", gsub("-", "_", comp_start$tb[i]), "h"), comp_start$ratio[i], "wet_recovery") }

# episodes that lasted >= 48 h (all are lid failures; kept for the record)
comp_long <- ev_all %>% filter(in_ep, t_start <= 72, length_h >= 48) %>% mutate(tb = cut(t_start, c(-1, 2, 6, 12, 24, 48, 72), labels = c("0-2", "3-6", "7-12", "13-24", "25-48", "49-72"))) %>%
  group_by(tb) %>% summarise(pct_gt100 = 100 * mean(anom > 100, na.rm = TRUE), ratio = exp(median(lr, na.rm = TRUE)), n = n(), n_ep = n_distinct(ep), .groups = "drop")
print(comp_long)
write.csv(comp_long, file.path(out_dir, "wet_buildup_long_episodes.csv"), row.names = FALSE)
for (i in seq_len(nrow(comp_long))) record(paste0("buildup_long_pct_gt100_", gsub("-", "_", comp_long$tb[i]), "h"), comp_long$pct_gt100[i], "wet_recovery", paste("episodes >= 48 h; n episodes =", comp_long$n_ep[i]))
byu <- uh %>% filter(recorded) %>% group_by(unit, stand) %>% summarise(wet_pct = 100 * mean(wet), wet_h = sum(wet), .groups = "drop") %>% arrange(-wet_h)
print(byu, n = 20); write.csv(byu, file.path(out_dir, "wet_by_unit.csv"), row.names = FALSE)
record("wet_top4_units_share_pct", 100 * sum(head(byu$wet_h, 4)) / sum(byu$wet_h), "wet_recovery", "share of wet hours in the 4 wettest units")
record("wet_units_gt20pct", sum(byu$wet_pct > 20), "wet_recovery")

# ---- lasting change? dry-closure ratio vs cumulative wet hours to date ---------------------------
lt <- near_wet %>% group_by(unit) %>% mutate(cum_wet = cumsum(wet)) %>% ungroup() %>%
  filter(!wet, is.na(h_since_wet) | h_since_wet > 24, !is.na(lr))
lt <- lt %>% mutate(day = as.numeric(difftime(hour_of_obs, p0, units = "days")))
mlt <- lmer(lr ~ I(cum_wet / 100) + day + (1 | unit), data = lt)   # day: cumulative wet hours rise with date
record("lasting_ratio_pct_per_100_wet_h", 100 * (exp(fixef(mlt)[2]) - 1), "wet_recovery", "dry closures >24 h after wet; unit random intercept; adjusted for date")
record("lasting_t", summary(mlt)$coefficients[2, 3], "wet_recovery")

# ---- data loss -----------------------------------------------------------------------------------
# missed or failed recordings after wet hours (water reaching the electronics?)
ml <- near_wet %>% mutate(prev_wet6 = !is.na(h_since_wet) & h_since_wet >= 1 & h_since_wet <= 6) %>% filter(hour_of_obs >= p0 + 6 * 3600)
pm <- ml %>% group_by(prev_wet6) %>% summarise(not_recorded_pct = 100 * mean(!recorded), err_pct = mean(err_pct, na.rm = TRUE), n = n())
print(pm)
gm <- glmer(!recorded ~ prev_wet6 + (1 | unit), family = binomial, data = ml)
record("loss_notrec_pct_after_wet", pm$not_recorded_pct[pm$prev_wet6], "wet_recovery", "unit-hours with a wet hour in the previous 6 h")
record("loss_notrec_pct_otherwise", pm$not_recorded_pct[!pm$prev_wet6], "wet_recovery")
record("loss_notrec_OR_after_wet", exp(fixef(gm)[2]), "wet_recovery", "unit random intercept"); record("loss_notrec_p", summary(gm)$coefficients[2, 4], "wet_recovery")
record("loss_err_pct_wet_hours", mean(uh$err_pct[uh$wet], na.rm = TRUE), "wet_recovery"); record("loss_err_pct_dry_hours", mean(uh$err_pct[uh$recorded & !uh$wet], na.rm = TRUE), "wet_recovery")
acc <- read.csv(file.path(out_dir, "measurement_accounting.csv"))
print(acc)

# ---- bias: agreement with wet closures kept, removed, and with a post-wet buffer -----------------
fb0 <- load_fluxbot() %>% mutate(unit = gsub("[^0-9]", "", as.character(id))) %>%
  left_join(near_wet %>% select(unit, hour_of_obs, h_since_wet), by = c("unit", "hour_of_obs")) %>% mutate(wet_orig = wet)
lid_h <- bind_rows(lapply(which(episodes$type == "lid failure"), function(i) tibble(unit = episodes$unit[i],
  hour_of_obs = seq(episodes$start[i], episodes$end[i], by = 3600))))
fb0 <- fb0 %>% left_join(lid_h %>% mutate(lid = TRUE), by = c("unit", "hour_of_obs")) %>% mutate(lid = coalesce(lid, FALSE))
qc_in <- apply_qc(fb0 %>% mutate(wet = FALSE), "valid")
record("removed_wet_closures_total", sum(qc_in$wet_orig), "wet_recovery", "valid closures with the wet flag")
record("removed_wet_closures_lid", sum(qc_in$lid & qc_in$wet_orig), "wet_recovery", "of which inside lid-failure episodes")
ac0 <- apply_qc(load_autochamber(), "fit"); met <- load_met()
agree <- function(fb) {
  d <- assemble_dataset(fb, ac0, met) %>% filter(hour_of_obs >= p0, hour_of_obs < p1)
  hrs <- matched_hours(d, 3)
  s <- d %>% filter(hour_of_obs %in% hrs) %>% group_by(hour_of_obs, stand, method) %>% summarise(f = mean(fluxL_umolm2sec), .groups = "drop") %>%
    group_by(hour_of_obs, method) %>% summarise(f = mean(f), .groups = "drop") %>% tidyr::pivot_wider(names_from = method, values_from = f)
  dd <- s %>% mutate(day = as.Date(hour_of_obs, tz = "America/New_York")) %>% group_by(day) %>% filter(n() >= 12) %>% summarise(a = mean(autochamber), f = mean(fluxbot))
  tibble(fb_closures = sum(d$method == "fluxbot"), n_hours = nrow(s), offset_pct = 100 * (mean(s$fluxbot) / mean(s$autochamber) - 1),
         r_hourly = cor(s$autochamber, s$fluxbot), r_daily = cor(dd$a, dd$f), n_days = nrow(dd))
}
sc <- bind_rows(
  agree(apply_qc(fb0, "fit_nowet")) %>% mutate(scenario = "wet closures kept"),
  agree(apply_qc(fb0 %>% mutate(wet = lid), "fit")) %>% mutate(scenario = "lid-failure episodes removed, wet-sensor closures kept"),
  agree(apply_qc(fb0, "fit")) %>% mutate(scenario = "wet closures removed (main)"),
  bind_rows(lapply(c(1, 3, 6, 12, 24), function(k) agree(apply_qc(fb0 %>% mutate(wet = wet | (!is.na(h_since_wet) & h_since_wet >= 1 & h_since_wet <= k)), "fit")) %>%
    mutate(scenario = paste0("main + ", k, " h after wet removed")))))
print(sc, width = 200)
write.csv(sc, file.path(out_dir, "wet_bias_scenarios.csv"), row.names = FALSE)
for (i in seq_len(nrow(sc))) { tg <- gsub("[^a-z0-9]+", "_", tolower(sc$scenario[i]))
  for (v in c("fb_closures", "n_hours", "offset_pct", "r_hourly", "r_daily")) record(paste0("wetbias_", tg, "_", v), sc[[v]][i], "wet_recovery") }

# ---- figure ---------------------------------------------------------------------------------------
th <- theme_classic(base_size = 8)
ep_len <- ggplot(episodes, aes(length_h, fill = type)) + geom_histogram(binwidth = 6, boundary = 0, colour = "white") +
  scale_fill_manual(values = c("wet sensor" = "#4575B4", "lid failure" = "#D73027"), name = NULL) +
  labs(x = "Episode length (h, RH >= 99%)", y = "Episodes") + th + theme(legend.position = c(0.7, 0.8))
pa <- ggplot(comp_end, aes(t_end, anom)) + annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf, fill = "#4575B4", alpha = 0.12) +
  geom_hline(yintercept = c(0, thr), linetype = c("solid", "22"), colour = "grey50") +
  geom_ribbon(aes(ymin = anom_lo, ymax = anom_hi), fill = "grey80") + geom_line() + geom_point(size = 0.6) +
  labs(x = "Hours since last wet hour (wet-sensor episodes)", y = "Open-lid CO2 anomaly (ppm)\nmedian, IQR") + th
pb <- ggplot(comp_end %>% filter(n_ratio >= 10), aes(t_end, ratio)) + annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf, fill = "#4575B4", alpha = 0.12) +
  geom_hline(yintercept = c(1, exp(median(ref$lr, na.rm = TRUE))), linetype = c("solid", "22"), colour = "grey50") + geom_line() + geom_point(size = 0.6) +
  labs(x = "Hours since last wet hour (wet-sensor episodes)", y = "Fluxbot / autochamber\n(median; dashed = dry reference)") + th
pc <- ggplot(hi %>% filter(!is.na(rec_h)), aes(factor(rec_h, levels = 1:6))) + geom_bar(fill = "#4575B4") + scale_x_discrete(drop = FALSE) +
  labs(x = paste0("Hours after drying until open-lid\nanomaly <= ", thr, " ppm (episodes ending > 100 ppm)"), y = "Wet-sensor episodes") + th
pfig <- (ep_len | pc) / pa / pb + plot_annotation(tag_levels = "a")
ggsave(file.path(out_dir, "figures", "FigS_wet_recovery.pdf"), pfig, width = 160, height = 170, units = "mm", device = cairo_pdf)
ggsave(file.path(out_dir, "figures", "FigS_wet_recovery.png"), pfig, width = 160, height = 170, units = "mm", dpi = 300, device = ragg::agg_png)
print(write_numbers("numbers_wet_recovery.csv") %>% select(key, value), row.names = FALSE)
