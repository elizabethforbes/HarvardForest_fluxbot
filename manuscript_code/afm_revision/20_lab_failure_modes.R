# What the two laboratory tests say about three candidate failure modes of wet K30 sensors:
#  (1) liquid water on the PTFE membrane/envelope as a diffusion barrier -> slower, damped
#      response; in a closure this lowers the fitted slope (flux underestimate);
#  (2) water vapour inside the sensor (through the membrane) interfering with the IR reading ->
#      a reading that tracks humidity at constant CO2 (offset; apparent gain error when CO2 and
#      H2O change together);
#  (3) water reaching the electronics -> error codes, dropouts, spikes, sensor stopping.
# Tests: 22 Sep 2023 (18_ptfe_lab_test.R; one covered, one uncovered K30) and 13 Dec 2023
# (19_cover_test_dec.R; t1-t2 covered, c1-c3 uncovered). Each sensor is calibrated against the
# LGR on its own dry period; wet periods are then compared with that calibration.

source("afm_revision/00_prep.R")
source("afm_revision/fig_style.R")
suppressPackageStartupMessages({ library(readr); library(purrr); library(ggplot2); library(patchwork) })
tz <- "America/New_York"

# ---- data -------------------------------------------------------------------------------------
read_k30_txt <- function(path, day, s, last_session = FALSE) {
  x <- readLines(path); sess <- cumsum(grepl("BEGIN", x)); keep <- grepl("^ *[0-9]{1,2}:[0-9]{2}:[0-9]{2}, *[0-9]+", x)
  d <- tibble(time = as.POSIXct(paste(day, sub(",.*", "", trimws(x[keep]))), tz = tz), co2 = as.numeric(sub(".*, *", "", x[keep])),
              sensor = s, session = sess[keep])
  if (last_session) d <- d %>% filter(session == max(session))
  d %>% select(-session)
}
read_lgr <- function(path, shift_s) read_csv(path, col_types = cols(.default = "c")) %>%
  transmute(time = as.POSIXct(substr(lgr_time, 1, 19), format = "%m/%d/%Y %H:%M:%S", tz = tz) + shift_s,
            co2 = as.numeric(co2_ppm), h2o = as.numeric(h2o_ppm)) %>%
  filter(!is.na(time)) %>% group_by(time) %>% summarise(co2 = mean(co2), h2o = mean(h2o), .groups = "drop") %>%
  { left_join(tibble(time = seq(min(.$time), max(.$time), by = 1)), ., by = "time") } %>%
  mutate(co2 = zoo::na.approx(co2, na.rm = FALSE), h2o = zoo::na.approx(h2o, na.rm = FALSE))

d1 <- file.path(pkg, "raw", "lab_ptfe_test_2023-09-22"); day1 <- "2023-09-22"
sep <- list(
  k = bind_rows(read_k30_txt(file.path(d1, "co2_coveredPTFE_k30_22Sept2023.txt"), day1, "covered"),
                read_k30_txt(file.path(d1, "co2_uncoveredk30_22Sept2023.txt"), day1, "uncovered")) %>%
    mutate(group = sensor),
  lgr = read_lgr(file.path(d1, "lgr_2023-09-22.csv.gz"), -116),
  phases = tibble(phase = c("Dry", "Covered sensor wetted"),
                  t0 = as.POSIXct(paste(day1, c("13:20:00", "17:33:00")), tz = tz),
                  t1 = as.POSIXct(paste(day1, c("17:32:00", "17:49:00")), tz = tz)))
d2 <- file.path(pkg, "raw", "lab_cover_test_2023-12-13"); day2 <- "2023-12-13"
dec <- list(
  k = bind_rows(lapply(c(c1 = "k30_control_c1.txt", c2 = "k30_control_c2.txt", c3 = "k30_control_c3.txt",
                         t1 = "k30_test_t1.txt", t2 = "k30_test_t2.txt") %>% imap(~ read_k30_txt(file.path(d2, .x), day2, .y, TRUE)), identity)) %>%
    mutate(group = if_else(substr(sensor, 1, 1) == "t", "covered", "uncovered")),
  lgr = read_lgr(file.path(d2, "lgr_2023-12-13.csv.gz"), -130),
  phases = tibble(phase = c("Dry", "Wet PTFE", "Dry bracket", "Wet bracket", "Bare K30s sprayed"),
                  t0 = as.POSIXct(paste(day2, c("15:40:00", "16:40:00", "17:15:00", "17:43:00", "18:06:00")), tz = tz),
                  t1 = as.POSIXct(paste(day2, c("16:40:00", "17:15:00", "17:43:00", "18:06:00", "18:30:00")), tz = tz)))
tests <- list(`22 Sep` = sep, `13 Dec` = dec)

tag <- function(x) gsub("[^a-z0-9]", "", tolower(x))
fo <- function(x, tau) if (tau == 0) x else { a <- 1 - exp(-1 / tau); as.numeric(stats::filter(a * x, 1 - a, method = "recursive", init = x[1])) }
taus <- c(0, 5, 10, 15, 20, 30, 40, 50, 60, 75, 90)
at_ref <- function(lgr, var, tau, L, t) approx(as.numeric(lgr$time) + L, fo(lgr[[var]], tau), xout = as.numeric(t))$y

# ---- (a) dry calibration per sensor: K30 = a + b*CO2_f (tau and lag by grid search). Water vapour is
#      tested separately in (d): in the dry periods it varies too little to be fitted alongside CO2. ----
calib <- function(tst) {
  ph <- tst$phases[1, ]
  bind_rows(lapply(split(tst$k %>% filter(co2 < 65533, time >= ph$t0, time < ph$t1), ~sensor), function(y) {
    best <- NULL
    for (tau in taus) for (L in seq(-30, 30, 3)) {
      xc <- at_ref(tst$lgr, "co2", tau, L, y$time)
      m <- lm(y$co2 ~ xc); r <- sigma(m)
      if (is.null(best) || r < best$rmse) best <- list(tau = tau, L = L, a = coef(m)[[1]], b = coef(m)[[2]], rmse = r, n = nrow(y))
    }
    as_tibble(best) %>% mutate(sensor = y$sensor[1], group = y$group[1])
  }))
}
cal <- imap_dfr(tests, ~ calib(.x) %>% mutate(test = .y))
print(cal)

# ---- (b) per phase: refit the same model (how tau, gain and H2O sensitivity change), and apply
#      the dry calibration (bias and scatter relative to the sensor's own dry behaviour) ----------
per_phase <- function(tst, tname) {
  cl <- cal %>% filter(test == tname)
  bind_rows(lapply(seq_len(nrow(tst$phases)), function(p) {
    ph <- tst$phases[p, ]
    bind_rows(lapply(unique(tst$k$sensor), function(s) {
      all <- tst$k %>% filter(sensor == s, time >= ph$t0, time < ph$t1)
      y <- all %>% filter(co2 < 65533); c0 <- cl %>% filter(sensor == s)
      # longest gap between valid readings
      gaps <- if (nrow(y) > 1) max(diff(as.numeric(y$time))) else NA
      base <- tibble(test = tname, phase = ph$phase, sensor = s, group = tst$k$group[tst$k$sensor == s][1],
                     n_all = nrow(all), err_pct = 100 * mean(all$co2 >= 65533), max_gap_s = gaps, n = nrow(y))
      if (nrow(y) < 30) return(base)
      pred <- c0$a + c0$b * at_ref(tst$lgr, "co2", c0$tau, c0$L, y$time)
      res <- y$co2 - pred
      best <- NULL
      for (tau in taus) for (L in seq(-30, 30, 3)) {
        xc <- at_ref(tst$lgr, "co2", tau, L, y$time)
        m <- lm(y$co2 ~ xc); r <- sigma(m)
        if (is.null(best) || r < best$rmse) best <- list(tau = tau, b = coef(m)[[2]], rmse = r)
      }
      bind_cols(base, as_tibble(best) %>% rename_with(~ paste0("refit_", .x)),
                tibble(bias_vs_dry = median(res, na.rm = TRUE), scatter_vs_dry = mad(res, na.rm = TRUE),
                       spike_pct = 100 * mean(abs(res - median(res, na.rm = TRUE)) > 5 * c0$rmse, na.rm = TRUE),
                       h2o_range_ppt = diff(range(tst$lgr$h2o[tst$lgr$time >= ph$t0 & tst$lgr$time < ph$t1], na.rm = TRUE)) / 1000,
                       r_co2_h2o = cor(tst$lgr$co2[tst$lgr$time >= ph$t0 & tst$lgr$time < ph$t1], tst$lgr$h2o[tst$lgr$time >= ph$t0 & tst$lgr$time < ph$t1], use = "complete")))
    }))
  }))
}
pp <- imap_dfr(tests, per_phase)
write.csv(pp, file.path(out_dir, "lab_failure_modes_by_phase.csv"), row.names = FALSE)
print(pp %>% select(test, phase, sensor, err_pct, max_gap_s, refit_tau, refit_b, bias_vs_dry, scatter_vs_dry, spike_pct, r_co2_h2o), n = 60, width = 220)

# ---- (c) closure-like ramps: slope of each K30 vs the LGR slope over 90-s windows -------------
# windows where the LGR changes steadily (|slope| > 1 ppm/s, R2 > 0.9); non-overlapping. Lab CO2 swings
# are shorter than a 3-min closure, so 90 s is used (~15 K30 readings).
ramps <- function(tst, tname) {
  l <- tst$lgr; W <- 90; st <- seq(min(l$time), max(l$time) - W, by = 10); keep <- list(); last_end <- min(l$time) - 1
  for (i in seq_along(st)) { w <- l %>% filter(time >= st[i], time < st[i] + W, !is.na(co2)); if (nrow(w) < 0.8 * W || st[i] < last_end) next
    m <- lm(co2 ~ I(as.numeric(time) - as.numeric(st[i])), data = w); sl <- coef(m)[2]  # centred: raw epoch seconds are collinear with the intercept in lm()
    if (abs(sl) > 1 && summary(m)$r.squared > 0.9) { keep[[length(keep) + 1]] <- tibble(t0 = st[i], lgr_slope = sl); last_end <- st[i] + W } }
  rw <- bind_rows(keep)
  bind_rows(lapply(seq_len(nrow(rw)), function(i) {
    ph <- tst$phases %>% filter(rw$t0[i] >= t0, rw$t0[i] + W <= t1); if (nrow(ph) == 0) return(NULL)
    tst$k %>% filter(co2 < 65533, time >= rw$t0[i], time < rw$t0[i] + W) %>% group_by(sensor, group) %>% filter(n() >= 10) %>%
      summarise(k30_slope = coef(lm(co2 ~ I(as.numeric(time) - as.numeric(rw$t0[i]))))[2], .groups = "drop") %>%
      mutate(test = tname, phase = ph$phase[1], t0 = rw$t0[i], lgr_slope = rw$lgr_slope[i], ratio = k30_slope / lgr_slope)
  }))
}
rp <- imap_dfr(tests, ramps)
write.csv(rp, file.path(out_dir, "lab_ramp_slopes.csv"), row.names = FALSE)
# paired: each sensor's slope relative to the uncovered reference on the same ramp (c2, noisy
# throughout the December test, is not used as reference)
refs <- rp %>% filter(group == "uncovered", sensor != "c2") %>% group_by(test, t0) %>% summarise(ref_slope = mean(k30_slope), .groups = "drop")
rp <- rp %>% left_join(refs, by = c("test", "t0")) %>% mutate(rel_unc = k30_slope / ref_slope)
rs <- rp %>% group_by(test, phase, group) %>%
  summarise(n_ramps = n_distinct(t0), median_ratio = median(ratio), q25 = quantile(ratio, .25), q75 = quantile(ratio, .75),
            covered_rel_uncovered = median(rel_unc[group == "covered"]), .groups = "drop")
print(rs, n = 40)
write.csv(rs, file.path(out_dir, "lab_ramp_slopes_summary.csv"), row.names = FALSE)


# ---- (d) water-vapour sensitivity: residual from the dry calibration vs LGR H2O, 1-min means.
#      The uncovered sensors see the same vapour but no liquid water (before the final spray), so
#      their slope isolates vapour interference; covered sensors' slopes mix vapour and liquid. --
vapour <- function(tst, tname) {
  cl <- cal %>% filter(test == tname); end <- if (tname == "13 Dec") tst$phases$t0[5] else max(tst$lgr$time)
  bind_rows(lapply(unique(tst$k$sensor), function(s) {
    c0 <- cl %>% filter(sensor == s)
    y <- tst$k %>% filter(sensor == s, co2 < 65533, time >= tst$phases$t0[1], time < end) %>%
      mutate(res = co2 - (c0$a + c0$b * at_ref(tst$lgr, "co2", c0$tau, c0$L, time)),
             h2o = at_ref(tst$lgr, "h2o", c0$tau, c0$L, time) / 1000,
             wet = time >= tst$phases$t0[2], m = floor_date(time, "minute")) %>%
      group_by(m, wet) %>% summarise(res = median(res), h2o = mean(h2o), .groups = "drop") %>% filter(!is.na(res), !is.na(h2o))
    fit <- function(z) if (nrow(z) > 10 && diff(range(z$h2o)) > 0.5) { m <- lm(res ~ h2o, z); ci <- confint(m)[2, ]
      tibble(slope = coef(m)[2], lo = ci[1], hi = ci[2], n_min = nrow(z), h2o_lo = min(z$h2o), h2o_hi = max(z$h2o)) } else tibble(slope = NA_real_)
    bind_rows(fit(y) %>% mutate(subset = "all"), fit(y %>% filter(!wet)) %>% mutate(subset = "dry only")) %>%
      mutate(test = tname, sensor = s, group = tst$k$group[tst$k$sensor == s][1])
  }))
}
vp <- imap_dfr(tests, vapour)
print(vp, n = 30)
write.csv(vp, file.path(out_dir, "lab_vapour_sensitivity.csv"), row.names = FALSE)
for (i in which(!is.na(vp$slope))) record(paste0("lab_vapour_", tag(vp$test[i]), "_", vp$sensor[i], "_", tag(vp$subset[i])), vp$slope[i], "lab_failure",
  sprintf("ppm CO2 per ppt H2O (95%% CI %.1f to %.1f; H2O %.1f-%.1f ppt)", vp$lo[i], vp$hi[i], vp$h2o_lo[i], vp$h2o_hi[i]))

# ---- numbers ----------------------------------------------------------------------------------
for (i in seq_len(nrow(cal))) for (v in c("tau", "b", "rmse"))
  record(paste0("lab_cal_", tag(cal$test[i]), "_", cal$sensor[i], "_", v), cal[[v]][i], "lab_failure", "dry calibration against LGR")
grp <- pp %>% group_by(test, phase, group) %>% summarise(across(c(err_pct, max_gap_s, refit_tau, refit_b, bias_vs_dry, scatter_vs_dry, spike_pct), ~ median(.x, na.rm = TRUE)), .groups = "drop")
for (i in seq_len(nrow(grp))) for (v in setdiff(names(grp), c("test", "phase", "group")))
  record(paste0("lab_", tag(grp$test[i]), "_", tag(grp$phase[i]), "_", grp$group[i], "_", v), grp[[v]][i], "lab_failure", "median over sensors")
for (i in seq_len(nrow(rs))) { record(paste0("lab_ramp_", tag(rs$test[i]), "_", tag(rs$phase[i]), "_", rs$group[i]), rs$median_ratio[i], "lab_failure", paste("K30/LGR slope, n ramps =", rs$n_ramps[i]))
  record(paste0("lab_ramp_relunc_", tag(rs$test[i]), "_", tag(rs$phase[i]), "_", rs$group[i]), rs$covered_rel_uncovered[i], "lab_failure", "covered/uncovered slope, same ramps") }

# ---- figure: per sensor and phase, (a) ramp slope ratio, (b) bias vs dry calibration, (c) scatter,
#      (d) error-code share --------------------------------------------------------------------------
lvl <- c("Sep: Dry", "Sep: Covered sensor wetted", "Dec: Dry", "Dec: Wet PTFE", "Dec: Dry bracket", "Dec: Wet bracket", "Dec: Bare K30s sprayed")
lab_ph <- function(t, p) factor(paste0(substr(t, 4, 6), ": ", p), levels = lvl)
gcol <- c(covered = unname(pal_lab["covered"]), uncovered = unname(pal_lab["uncovered"]))
ppl <- pp %>% mutate(ph = lab_ph(test, phase), slab = if_else(test == "13 Dec", sensor, ""), noisy = sensor == "c2")
p_a <- ggplot(rp %>% filter(group == "covered") %>% mutate(ph = lab_ph(test, phase)), aes(ph, rel_unc, colour = group)) + geom_hline(yintercept = 1, colour = "grey50") +
  geom_point(position = position_jitter(width = 0.12, height = 0), size = 0.8, alpha = 0.6, show.legend = FALSE) +
  stat_summary(fun = median, geom = "point", shape = 95, size = 8, show.legend = FALSE) +
  scale_colour_manual(values = gcol, name = NULL) + coord_cartesian(ylim = c(0.4, 1.6)) +
  labs(x = NULL, y = "Covered / uncovered\nslope (90-s ramps)", title = "Response to CO2 ramps (diffusion barrier would lower this)")
p_b <- ggplot(ppl, aes(ph, bias_vs_dry, colour = group, label = slab, shape = noisy)) + geom_hline(yintercept = 0, colour = "grey50") +
  geom_point(position = position_dodge(0.6), size = 1.8) + geom_text(position = position_dodge(0.6), size = 1.9, hjust = -0.5, show.legend = FALSE) +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 1), labels = "c2 (noisy all day)", breaks = "TRUE", name = NULL) + scale_colour_manual(values = gcol, name = NULL) +
  labs(x = NULL, y = "Bias vs own dry\ncalibration (ppm)", title = "Offset when wet")
p_c <- ggplot(ppl, aes(ph, scatter_vs_dry, colour = group, label = slab, shape = noisy)) + geom_point(position = position_dodge(0.6), size = 1.8) + geom_text(position = position_dodge(0.6), size = 1.9, hjust = -0.5, show.legend = FALSE) +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 1), labels = "c2 (noisy all day)", breaks = "TRUE", name = NULL) +
  scale_colour_manual(values = gcol, name = NULL) + labs(x = NULL, y = "Scatter vs own dry\ncalibration (MAD, ppm)", title = "Erratic readings (scatter)")
p_d <- ggplot(ppl, aes(ph, err_pct, colour = group, label = slab, shape = noisy)) + geom_point(position = position_dodge(0.6), size = 1.8) + geom_text(position = position_dodge(0.6), size = 1.9, hjust = -0.5, show.legend = FALSE) +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 1), labels = "c2 (noisy all day)", breaks = "TRUE", name = NULL) +
  scale_colour_manual(values = gcol, name = NULL) + labs(x = NULL, y = "Error codes (%)", title = "Error codes (electronics)")
pfig <- (p_a / p_b / p_c / p_d) + plot_layout(guides = "collect") + plot_annotation(tag_levels = "a") &
  theme_afm() & theme(legend.position = "bottom", plot.title = element_text(size = 8), axis.text.x = element_text(angle = 25, hjust = 1))
ggsave(file.path(out_dir, "figures", "FigS_lab_failure_modes.pdf"), pfig, width = 140, height = 230, units = "mm", device = cairo_pdf)
ggsave(file.path(out_dir, "figures", "FigS_lab_failure_modes.png"), pfig, width = 140, height = 230, units = "mm", dpi = 300, device = ragg::agg_png)
write_numbers("numbers_lab_failure.csv")
