# Laboratory test of K30 covers, 13 December 2023 (Raymond lab CO2 rig). Three uncovered
# K30s (c1-c3, controls) and two covered K30s (t1, t2) logged every ~6 s in one chamber next
# to an LGR analyzer (1 Hz); CO2 was varied by breath pulses and door openings.
# Sequence (J. Gewirtzman, messages of 13 Dec 2023): dry PTFE envelope; wet PTFE envelope;
# dry 3D-printed bracket; wet 3D-printed bracket; finally bare K30s sprayed with water, which
# became erratic and stopped recording. Phase boundaries are not in the notes; they are set
# from the LGR water vapour record (wetting raises chamber H2O sharply at 16:40 and 17:43)
# and from where the covered sensors stop recording (18:06).
# Clocks: each K30 file holds several logging sessions; only the last (started 11:12:11 on
# all loggers) is from this run. The LGR clock is 130 s ahead of the K30 loggers (constant
# over the day; cross-correlation in 20-min windows).
#
# Same response model as 18_ptfe_lab_test.R: K30(t) = a + b * [LGR first-order filtered with tau](t - L).
# The covered - uncovered difference needs no reference and is reported directly.

source("afm_revision/00_prep.R")
suppressPackageStartupMessages({ library(readr); library(ggplot2); library(patchwork) })
dd <- file.path(pkg, "raw", "lab_cover_test_2023-12-13")
tz <- "America/New_York"; at <- function(x) as.POSIXct(paste("2023-12-13", x), tz = tz)
rd_k30 <- function(f, s) {
  x <- readLines(file.path(dd, f)); sess <- cumsum(grepl("BEGIN", x)); keep <- grepl("^ *[0-9]{1,2}:[0-9]{2}:[0-9]{2}, *[0-9]+", x)
  tibble(time = at(sub(",.*", "", trimws(x[keep]))), co2 = as.numeric(sub(".*, *", "", x[keep])), sensor = s, session = sess[keep]) %>%
    filter(session == max(session)) %>% select(-session)
}
kall <- bind_rows(rd_k30("k30_control_c1.txt", "c1"), rd_k30("k30_control_c2.txt", "c2"), rd_k30("k30_control_c3.txt", "c3"),
                  rd_k30("k30_test_t1.txt", "t1"), rd_k30("k30_test_t2.txt", "t2")) %>%
  mutate(group = if_else(substr(sensor, 1, 1) == "t", "Covered", "Uncovered"))
k <- kall %>% filter(co2 < 65533)
lgr <- read_csv(file.path(dd, "lgr_2023-12-13.csv.gz"), col_types = "cddd") %>%
  mutate(time = as.POSIXct(substr(lgr_time, 1, 19), format = "%m/%d/%Y %H:%M:%S", tz = tz) - 130) %>%
  group_by(time) %>% summarise(co2 = mean(co2_ppm), h2o = mean(h2o_ppm), .groups = "drop")
grid <- tibble(time = seq(min(lgr$time), max(lgr$time), by = 1)) %>% left_join(lgr, by = "time") %>%
  mutate(co2 = zoo::na.approx(co2, na.rm = FALSE), h2o = zoo::na.approx(h2o, na.rm = FALSE))

phases <- tibble(phase = c("Dry, steady", "Dry PTFE", "Wet PTFE", "Dry bracket", "Wet bracket", "Bare K30s sprayed"),
                 t0 = at(c("13:50:00", "15:40:00", "16:40:00", "17:15:00", "17:43:00", "18:06:00")),
                 t1 = at(c("15:40:00", "16:40:00", "17:15:00", "17:43:00", "18:06:00", "18:30:00"))) %>%
  mutate(phase = factor(phase, levels = phase))
record("cover_lgr_h2o_median_dry_ppm", median(grid$h2o[grid$time < at("16:40:00") & grid$time > at("13:50:00")], na.rm = TRUE), "cover_dec")
record("cover_lgr_h2o_median_wet_ppm", median(grid$h2o[grid$time >= at("16:40:00") & grid$time < at("18:06:00")], na.rm = TRUE), "cover_dec")
err <- kall %>% group_by(sensor) %>% summarise(pct = 100 * mean(co2 >= 65533))
for (i in seq_len(nrow(err))) record(paste0("cover_errors_pct_", err$sensor[i]), err$pct[i], "cover_dec", "% readings = error code, whole run")

# response fits per sensor and phase (the steady phase has too little variation for tau; in the
# bare-spray phase only the uncovered sensors still record)
fo_filter <- function(x, tau) if (tau == 0) x else { a <- 1 - exp(-1 / tau); as.numeric(stats::filter(a * x, 1 - a, method = "recursive", init = x[1])) }
fgrid <- lapply(setNames(nm = c(0, 5, 10, 15, 20, 30, 40, 50, 60, 75, 90, 120, 150)), function(tau) fo_filter(grid$co2, as.numeric(tau)))
fit_resp <- function(s, p) {
  y <- k %>% filter(sensor == s, time >= phases$t0[p], time < phases$t1[p])
  if (nrow(y) < 60) return(NULL)
  best <- NULL
  for (tau in names(fgrid)) for (L in seq(-60, 60, 3)) {
    x <- approx(as.numeric(grid$time) + L, fgrid[[tau]], xout = as.numeric(y$time))$y; ok <- !is.na(x)
    m <- lm(y$co2[ok] ~ x[ok]); r <- sqrt(mean(residuals(m)^2))
    if (is.null(best) || r < best$rmse) best <- list(tau = as.numeric(tau), L = L, a = unname(coef(m)[1]), b = unname(coef(m)[2]), rmse = r, r2 = summary(m)$r.squared, n = sum(ok))
  }
  as_tibble(best) %>% mutate(sensor = s, phase = phases$phase[p])
}
fits <- bind_rows(lapply(unique(k$sensor), function(s) bind_rows(lapply(2:6, function(p) fit_resp(s, p))))) %>%
  mutate(group = if_else(substr(sensor, 1, 1) == "t", "Covered", "Uncovered"))
write.csv(fits, file.path(out_dir, "cover_test_dec_response_fits.csv"), row.names = FALSE)
print(fits, n = 50)
fsum <- fits %>% group_by(phase, group) %>% summarise(across(c(tau, b, rmse, r2), median), .groups = "drop")
print(fsum)
for (i in seq_len(nrow(fsum))) { tag <- paste0(tolower(fsum$group[i]), "_", gsub("[^a-z]", "", tolower(fsum$phase[i])))
  for (v in c("tau", "b", "rmse", "r2")) record(paste0("cover_", tag, "_", v), fsum[[v]][i], "cover_dec", "median over sensors") }

# covered - uncovered difference, and each group - LGR, at quiescent times (|dCO2/dt| small)
q <- grid %>% mutate(slope = abs(zoo::rollapply(co2, 61, function(z) diff(range(z)), fill = NA))) %>% select(time, slope, ref = co2)
kd <- k %>% mutate(t6 = round_date(time, "6 sec")) %>% group_by(t6, group) %>% summarise(co2 = mean(co2), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = group, values_from = co2) %>% mutate(diff = Covered - Uncovered) %>%
  left_join(q, by = c("t6" = "time")) %>% left_join(grid %>% select(time, h2o), by = c("t6" = "time"))
off <- bind_rows(lapply(seq_len(nrow(phases)), function(p) kd %>% filter(t6 >= phases$t0[p], t6 < phases$t1[p], slope < 15) %>%
  summarise(phase = phases$phase[p], n = sum(!is.na(diff)), diff_median = median(diff, na.rm = TRUE),
            cov_minus_lgr = median(Covered - ref, na.rm = TRUE), unc_minus_lgr = median(Uncovered - ref, na.rm = TRUE))))
print(off)
write.csv(off, file.path(out_dir, "cover_test_dec_offsets.csv"), row.names = FALSE)
for (i in seq_len(nrow(off))) { tag <- gsub("[^a-z]", "", tolower(off$phase[i]))
  for (v in c("diff_median", "cov_minus_lgr", "unc_minus_lgr", "n")) record(paste0("cover_offset_", tag, "_", v), off[[v]][i], "cover_dec", "quiescent periods (LGR range < 15 ppm over 61 s)") }

# figure: a, whole run (overview); b, treatment phases, each sensor; c, covered - uncovered (group means);
# d, LGR water vapour. Phase names are printed on the shading.
ph_fill <- c("Dry, steady" = "grey92", "Dry PTFE" = "#FEE090", "Wet PTFE" = "#4575B4", "Dry bracket" = "#FDAE61",
             "Wet bracket" = "#74ADD1", "Bare K30s sprayed" = "#D73027")
shade <- function(ph = phases) geom_rect(data = ph, aes(xmin = t0, xmax = t1, ymin = -Inf, ymax = Inf, fill = phase), inherit.aes = FALSE, alpha = 0.22, show.legend = FALSE)
ph_lab <- function(ph = phases[-1, ], size = 2.3) geom_text(data = ph, aes(x = t0 + (t1 - t0) / 2, y = Inf, label = phase), inherit.aes = FALSE, vjust = 1.4, size = size, colour = "grey20")
fill_ph <- scale_fill_manual(values = ph_fill, guide = "none")
sens_pal <- c(c1 = "#B35806", c2 = "#F1A340", c3 = "#FDB863", t1 = "#2166AC", t2 = "#67A9CF")
sens_lab <- c(c1 = "c1 uncovered", c2 = "c2 uncovered", c3 = "c3 uncovered", t1 = "t1 covered", t2 = "t2 covered")
win <- at(c("15:35:00", "18:32:00"))
xs <- scale_x_datetime(date_breaks = "15 min", date_labels = "%H:%M", limits = win, expand = c(0, 0))
th <- theme_classic(base_size = 8)
ref <- geom_line(data = grid %>% filter(!is.na(co2)), aes(time, co2), inherit.aes = FALSE, linewidth = 0.3, colour = "black")
pa <- ggplot(k, aes(time, co2, colour = sensor)) + shade() + fill_ph + ref +
  annotate("rect", xmin = win[1], xmax = win[2], ymin = -Inf, ymax = Inf, fill = NA, colour = "grey40", linetype = "22") +
  geom_point(size = 0.15, alpha = 0.6) + scale_colour_manual(values = sens_pal, labels = sens_lab, name = NULL) +
  scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M", expand = c(0.01, 0)) + coord_cartesian(ylim = c(420, 1650)) +
  labs(x = NULL, y = expression(CO[2] ~ (ppm)), title = "Whole run; dashed box = panels b-d") + th + theme(plot.title = element_text(size = 7))
pb <- ggplot(k, aes(time, co2, colour = sensor)) + shade() + fill_ph + ph_lab() + ref +
  geom_point(size = 0.35, alpha = 0.75) + scale_colour_manual(values = sens_pal, labels = sens_lab, name = NULL) +
  coord_cartesian(ylim = c(420, 1750)) + xs + labs(x = NULL, y = expression(CO[2] ~ (ppm))) + th
pc <- ggplot(kd, aes(t6, diff)) + shade() + fill_ph + geom_hline(yintercept = 0, colour = "grey40") + geom_point(size = 0.3) +
  coord_cartesian(ylim = c(-130, 130)) + xs + labs(x = NULL, y = "Covered - uncovered\n(ppm, group means)") + th
pd2 <- ggplot(grid %>% filter(!is.na(h2o)), aes(time, h2o / 1000)) + shade() + fill_ph + geom_line(linewidth = 0.3) + xs +
  labs(x = "Time (13 Dec 2023, K30 logger clock)", y = expression(LGR ~ H[2]*O ~ (ppt))) + th
pfig <- pa / pb / pc / pd2 + plot_layout(heights = c(1.4, 3, 1.3, 1), guides = "collect") + plot_annotation(tag_levels = "a") &
  theme(legend.position = "bottom") & guides(colour = guide_legend(override.aes = list(size = 2, alpha = 1), nrow = 1))
ggsave(file.path(out_dir, "figures", "FigS_cover_test_dec.pdf"), pfig, width = 190, height = 210, units = "mm", device = cairo_pdf)
ggsave(file.path(out_dir, "figures", "FigS_cover_test_dec.png"), pfig, width = 190, height = 210, units = "mm", dpi = 300, device = ragg::agg_png)

# per-sensor status in the last phase (which sensors were affected by spraying)
last <- kall %>% filter(time >= phases$t0[6] - 300) %>% group_by(sensor) %>%
  summarise(last_valid = format(max(time[co2 < 65533]), "%H:%M:%S"), pct_err_after_1801 = 100 * mean(co2 >= 65533), .groups = "drop")
print(last)
jump <- k %>% filter(time >= phases$t0[6], time < phases$t1[6]) %>% mutate(x = approx(as.numeric(grid$time), grid$co2, xout = as.numeric(time))$y) %>%
  group_by(sensor) %>% summarise(n = n(), rmse_vs_lgr = sqrt(mean((co2 - x - median(co2 - x, na.rm = TRUE))^2, na.rm = TRUE)))
print(jump)
for (i in seq_len(nrow(jump))) record(paste0("cover_spray_rmse_", jump$sensor[i]), jump$rmse_vs_lgr[i], "cover_dec", "bare-spray phase, offset-removed RMSE vs LGR")
print(write_numbers("numbers_cover_dec.csv") %>% select(key, value), row.names = FALSE)
