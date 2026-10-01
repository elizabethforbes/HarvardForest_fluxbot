# Laboratory test of the K30 PTFE envelope, 22 September 2023 (growth chamber, ~90% RH,
# CO2 setpoint 900 ppm). A PTFE-covered and an uncovered K30 logged every ~6 s next to an
# LGR analyzer (1 Hz). Lab notes: 13:36 door closed (CO2 injection off); 13:45 CO2 on;
# 14:00 door opened 1 min; 17:32 door opened and the covered sensor sprayed with water (water
# beaded on the PTFE), door left open to plateau, then closed ~3 min; 17:43 opened (to ~670 ppm),
# breath spike to ~900 ppm, closed to plateau; 17:49 SD cards collected.
# LGR clock correction from the notes: LGR 17:54:53 = real 17:52:57.
#
# Each K30 series is modelled as a lagged first-order response to the LGR:
#   K30(t) = a + b * [LGR filtered with time constant tau](t - L)
# fitted separately before and after wetting. L absorbs logger clock offsets; tau measures how
# slowly the sensor (and envelope) follows concentration changes; b is the gain (span);
# a is the offset.

source("afm_revision/00_prep.R")
suppressPackageStartupMessages({ library(readr); library(ggplot2) })
dd <- file.path(pkg, "raw", "lab_ptfe_test_2023-09-22")
day <- "2023-09-22"
rd_k30 <- function(f, name) read_csv(file.path(dd, f), skip = 1, col_names = c("t", "co2"), col_types = "cd") %>%
  mutate(time = as.POSIXct(paste(day, trimws(t)), tz = "America/New_York"), co2 = if_else(co2 >= 65533, NA_real_, co2), sensor = name) %>%
  filter(!is.na(time))
k30 <- bind_rows(rd_k30("co2_coveredPTFE_k30_22Sept2023.txt", "PTFE-covered K30"),
                 rd_k30("co2_uncoveredk30_22Sept2023.txt", "Uncovered K30"))
lgr <- read_csv(file.path(dd, "lgr_2023-09-22.csv.gz"), col_types = "cddd") %>%
  mutate(time = as.POSIXct(substr(lgr_time, 1, 19), format = "%m/%d/%Y %H:%M:%S", tz = "America/New_York") +
           as.numeric(difftime(as.POSIXct("2023-09-22 17:52:57", tz = "America/New_York"),
                               as.POSIXct("2023-09-22 17:54:53", tz = "America/New_York"), units = "secs"))) %>%
  filter(!is.na(time)) %>% group_by(time) %>% summarise(co2 = mean(co2_ppm), h2o = mean(h2o_ppm), .groups = "drop")
grid <- tibble(time = seq(min(lgr$time), max(lgr$time), by = 1)) %>% left_join(lgr, by = "time") %>%
  mutate(co2 = zoo::na.approx(co2, na.rm = FALSE))
record("ptfe_lgr_h2o_median_ppm", median(lgr$h2o, na.rm = TRUE), "ptfe_lab", "LGR water vapour")

wet_t <- as.POSIXct("2023-09-22 17:32:00", tz = "America/New_York")
periods <- list(before = c(as.POSIXct("2023-09-22 13:20:00", tz = "America/New_York"), wet_t),
                after = c(wet_t + 60, as.POSIXct("2023-09-22 17:49:00", tz = "America/New_York")))
fo_filter <- function(x, tau) if (tau == 0) x else { a <- 1 - exp(-1 / tau); as.numeric(stats::filter(a * x, 1 - a, method = "recursive", init = x[1])) }
fit_resp <- function(s, p) {
  y <- k30 %>% filter(sensor == s, time >= p[1], time < p[2], !is.na(co2))
  best <- NULL
  for (tau in c(0, 5, 10, 15, 20, 30, 40, 50, 60, 75, 90, 120, 150, 180, 240)) {
    f <- fo_filter(grid$co2, tau)
    for (L in seq(-300, 300, 3)) {
      x <- approx(as.numeric(grid$time) + L, f, xout = as.numeric(y$time))$y
      ok <- !is.na(x); if (sum(ok) < 30) next
      m <- lm(y$co2[ok] ~ x[ok]); r <- sqrt(mean(residuals(m)^2))
      if (is.null(best) || r < best$rmse) best <- list(tau = tau, L = L, a = coef(m)[1], b = coef(m)[2], rmse = r,
                                                         r2 = summary(m)$r.squared, n = sum(ok))
    }
  }
  as_tibble(best) %>% mutate(sensor = s, period = names(periods)[sapply(periods, identical, p)])
}
fits <- bind_rows(lapply(unique(k30$sensor), function(s) bind_rows(lapply(periods, function(p) fit_resp(s, p)))))
write.csv(fits, file.path(out_dir, "ptfe_lab_response_fits.csv"), row.names = FALSE)
print(fits)
for (i in seq_len(nrow(fits))) { tag <- paste0(ifelse(grepl("PTFE", fits$sensor[i]), "covered", "uncovered"), "_", fits$period[i])
  for (k in c("tau", "L", "a", "b", "rmse", "r2")) record(paste0("ptfe_", tag, "_", k), fits[[k]][i], "ptfe_lab") }
# offsets from the LGR at plateaus, before vs after wetting (mean K30 - LGR over 10-min windows)
plateau <- function(s, t0, t1) { y <- k30 %>% filter(sensor == s, time >= t0, time < t1, !is.na(co2))
  x <- approx(as.numeric(grid$time), grid$co2, xout = as.numeric(y$time))$y; mean(y$co2 - x, na.rm = TRUE) }
for (s in unique(k30$sensor)) {
  tag <- ifelse(grepl("PTFE", s), "covered", "uncovered")
  record(paste0("ptfe_offset_", tag, "_before"), plateau(s, wet_t - 1800, wet_t), "ptfe_lab", "K30 - LGR, 30 min before wetting")
  record(paste0("ptfe_offset_", tag, "_after"), plateau(s, wet_t + 120, as.POSIXct("2023-09-22 17:49:00", tz = "America/New_York")), "ptfe_lab", "K30 - LGR after wetting")
  record(paste0("ptfe_errors_pct_", tag), 100 * mean(is.na(k30$co2[k30$sensor == s])), "ptfe_lab", "% readings = error code")
}

# figure
ev <- tibble(time = as.POSIXct(paste(day, c("13:36:00", "13:45:00", "14:00:00", "17:32:00", "17:43:00")), tz = "America/New_York"),
             lab = c("door closed", "CO2 on", "door 1 min", "covered sensor wetted", "door opened"))
pd <- bind_rows(k30 %>% select(time, co2, sensor), grid %>% filter(!is.na(co2)) %>% transmute(time, co2, sensor = "LGR (reference)"))
pal3 <- c("LGR (reference)" = "black", "Uncovered K30" = "#E08214", "PTFE-covered K30" = "#2C7BB6")
mk <- function(t0, t1) ggplot(pd %>% filter(time >= t0, time <= t1), aes(time, co2, colour = sensor)) +
  geom_line(data = ~ filter(.x, sensor == "LGR (reference)"), linewidth = 0.4) +
  geom_point(data = ~ filter(.x, sensor != "LGR (reference)"), size = 0.5, alpha = 0.7) +
  geom_vline(data = ev %>% filter(time >= t0, time <= t1), aes(xintercept = time), linetype = "22", colour = "grey50") +
  geom_text(data = ev %>% filter(time >= t0, time <= t1), aes(x = time, y = Inf, label = lab), inherit.aes = FALSE,
            angle = 90, hjust = 1.1, vjust = -0.4, size = 2.2, colour = "grey30") +
  scale_colour_manual(values = pal3, name = NULL) + labs(x = NULL, y = expression(CO[2] ~ (ppm))) +
  theme_classic(base_size = 8) + theme(legend.position = "bottom")
library(patchwork)
t_all <- range(k30$time)
pfig <- mk(t_all[1], t_all[2]) / mk(wet_t - 900, as.POSIXct("2023-09-22 17:50:00", tz = "America/New_York")) +
  plot_layout(guides = "collect") + plot_annotation(tag_levels = "a") & theme(legend.position = "bottom")
ggsave(file.path(out_dir, "figures", "FigS_ptfe_lab_test.pdf"), pfig, width = 190, height = 140, units = "mm", device = cairo_pdf)
ggsave(file.path(out_dir, "figures", "FigS_ptfe_lab_test.png"), pfig, width = 190, height = 140, units = "mm", dpi = 300, device = ragg::agg_png)
print(write_numbers("numbers_ptfe_lab.csv") %>% select(key, value), row.names = FALSE)
