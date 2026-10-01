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

# response fits per sensor and phase (the steady phase has too little variation for tau)
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
fits <- bind_rows(lapply(unique(k$sensor), function(s) bind_rows(lapply(2:5, function(p) fit_resp(s, p))))) %>%
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

# figure: a, full record with phases; b, covered - uncovered; c, LGR water vapour
shade <- geom_rect(data = phases[-1, ], aes(xmin = t0, xmax = t1, ymin = -Inf, ymax = Inf, fill = phase), inherit.aes = FALSE, alpha = 0.25)
fill_ph <- scale_fill_manual(values = c("Dry, steady" = "grey85", "Dry PTFE" = "#FEE090", "Wet PTFE" = "#4575B4", "Dry bracket" = "#FDAE61", "Wet bracket" = "#74ADD1", "Bare K30s sprayed" = "#D73027"), breaks = levels(phases$phase)[-1], name = NULL)
xs <- scale_x_datetime(date_breaks = "15 min", date_labels = "%H:%M", limits = at(c("15:35:00", "18:32:00")), expand = c(0, 0))
pal <- c("LGR (reference)" = "black", "Uncovered K30" = "#E08214", "Covered K30" = "#2C7BB6")
pd <- bind_rows(k %>% transmute(time, co2, sensor, series = paste(group, "K30")),
                grid %>% filter(!is.na(co2)) %>% transmute(time, co2, sensor = "LGR", series = "LGR (reference)"))
pa <- ggplot(pd, aes(time, co2, colour = series)) + shade + fill_ph +
  geom_line(data = ~ filter(.x, series == "LGR (reference)"), linewidth = 0.35) +
  geom_point(data = ~ filter(.x, series != "LGR (reference)"), size = 0.35, alpha = 0.7) +
  scale_colour_manual(values = pal, name = NULL) + coord_cartesian(ylim = c(420, 1650)) + xs +
  labs(x = NULL, y = expression(CO[2] ~ (ppm))) + theme_classic(base_size = 8)
pb <- ggplot(kd, aes(t6, diff)) + shade + fill_ph + geom_hline(yintercept = 0, colour = "grey40") + geom_point(size = 0.3) +
  coord_cartesian(ylim = c(-130, 130)) + xs + labs(x = NULL, y = "Covered − uncovered (ppm)") + theme_classic(base_size = 8)
pc <- ggplot(grid %>% filter(!is.na(h2o)), aes(time, h2o / 1000)) + shade + fill_ph + geom_line(linewidth = 0.3) + xs +
  labs(x = "Time (13 Dec 2023)", y = expression(H[2]*O ~ (ppt))) + theme_classic(base_size = 8)
pfig <- pa / pb / pc + plot_layout(heights = c(3, 1.4, 1), guides = "collect") + plot_annotation(tag_levels = "a") & theme(legend.position = "bottom")
ggsave(file.path(out_dir, "figures", "FigS_cover_test_dec.pdf"), pfig, width = 190, height = 170, units = "mm", device = cairo_pdf)
ggsave(file.path(out_dir, "figures", "FigS_cover_test_dec.png"), pfig, width = 190, height = 170, units = "mm", dpi = 300, device = ragg::agg_png)
print(write_numbers("numbers_cover_dec.csv") %>% select(key, value), row.names = FALSE)
