# Figures for the AFM submission (task A10). Each figure is written as a vector
# PDF and a 600 dpi LZW TIFF, sized to Elsevier column widths
# (single = 90 mm, 1.5 = 140 mm, double = 190 mm).
# Main figures (Fig. 1 is the field photo, not generated here):
#   Fig 2 time series and cumulative budgets; Fig 3 agreement (scatter + Bland-Altman);
#   Fig 4 diel; Fig 5 temperature response; Fig 6 heterogeneity and sampling effort;
#   Fig 7 measurement flow (11_filter_flow.R); Fig 8 uptime and coverage (17_resilience.R).
# SI figures written here: chamber distributions, GAM fit, flux distributions.
# Datasets: "as deployed" (main; dataset_main.rds) and the "RH-screened" subset.

source("afm_revision/00_prep.R")
suppressPackageStartupMessages({
  library(ggplot2); library(patchwork); library(mgcv); library(zoo); library(ineq); library(scales)
})

fig_dir <- file.path(out_dir, "figures"); dir.create(fig_dir, showWarnings = FALSE)
pal <- c(autochamber = "#3B8F63", fluxbot = "#8C8C8C")
lab_sys <- c(autochamber = "Autochamber", fluxbot = "Fluxbot 2.0")
flux_lab <- expression(CO[2] ~ flux ~ (mu * mol ~ m^-2 ~ s^-1))
theme_set(theme_classic(base_size = 8) +
            theme(strip.background = element_blank(), strip.text = element_text(face = "bold"),
                  legend.key.size = unit(3, "mm")))
save_fig <- function(p, name, width_mm, height_mm) {
  ggsave(file.path(fig_dir, paste0(name, ".pdf")), p, width = width_mm, height = height_mm,
         units = "mm", device = cairo_pdf)
  ggsave(file.path(fig_dir, paste0(name, ".tif")), p, width = width_mm, height = height_mm,
         units = "mm", dpi = 600, device = ragg::agg_tiff, compression = "lzw")
}

d <- readRDS(file.path(out_dir, "dataset_main.rds")) %>%
  mutate(stand_label = factor(if_else(stand == "healthy", "Stand 1", "Stand 2")),
         method_label = factor(lab_sys[as.character(method)], levels = lab_sys))
met <- load_met()
nums <- read.csv(file.path(out_dir, "numbers_for_text.csv"))
num <- function(k) nums$value[nums$key == k]

# ---- Fig 2: time series and cumulative budgets ---------------------------------------------
# (a) hourly stand means (points) and 24-h centred rolling means (lines) of each array, as
#     deployed; ticks mark hours when at least half of the stand's recording Fluxbots were wet
# (b) soil temperature (HF001, 10 cm) and hourly precipitation
# (c) cumulative CO2-C from 2 Oct, gap-filled per stand (s10t + time-of-day GAM, as in
#     12_scales_budget.R), mean of the two stands; bands = chamber-bootstrap 95% intervals
p0 <- as.POSIXct("2023-10-02", tz = "America/New_York"); p1 <- as.POSIXct("2023-11-01", tz = "America/New_York")
sh <- d %>% group_by(stand_label, method, hour_of_obs) %>% summarise(f = mean(fluxL_umolm2sec), n = n(), .groups = "drop") %>% filter(n >= 3)
roll <- sh %>% group_by(stand_label, method) %>%
  complete(hour_of_obs = seq(p0, p1 - 3600, by = "hour")) %>% arrange(hour_of_obs, .by_group = TRUE) %>%
  mutate(r = rollapply(f, 24, function(x) if (sum(!is.na(x)) >= 12) mean(x, na.rm = TRUE) else NA, fill = NA, align = "center")) %>% ungroup()
wet_h <- load_fluxbot() %>% filter(!lid_fail) %>% group_by(stand, hour_of_obs) %>% summarise(w = mean(wet), .groups = "drop") %>%
  filter(w >= 0.5) %>% mutate(stand_label = if_else(stand == "healthy", "Stand 1", "Stand 2"))
p2a <- ggplot(sh, aes(hour_of_obs, f, colour = method)) +
  geom_rug(data = wet_h, aes(x = hour_of_obs), inherit.aes = FALSE, sides = "b", colour = "#4575B4", alpha = 0.6, length = unit(1.5, "mm")) +
  geom_point(size = 0.4, alpha = 0.35, stroke = 0) + geom_line(data = roll, aes(y = r), linewidth = 0.6, na.rm = TRUE) +
  facet_wrap(~ stand_label, ncol = 1) + scale_colour_manual(values = pal, labels = lab_sys, name = NULL) +
  scale_x_datetime(limits = c(p0, p1), date_labels = "%d %b") + labs(x = NULL, y = flux_lab) +
  theme(legend.position = "top")
metp <- met %>% mutate(hr = floor_date(with_tz(Time, "America/New_York"), "hour")) %>% group_by(hr) %>%
  summarise(s10t = mean(s10t), prec = sum(prec), .groups = "drop") %>% filter(hr >= p0, hr < p1)
sc <- max(metp$prec, na.rm = TRUE) / 20
p2b <- ggplot(metp, aes(hr)) + geom_col(aes(y = prec / sc), fill = "#4575B4", alpha = 0.6, width = 3600) +
  geom_line(aes(y = s10t), linewidth = 0.4) +
  scale_y_continuous(name = "Soil temp. 10 cm (\u00b0C)", sec.axis = sec_axis(~ . * sc, name = "Precip. (mm h-1)")) +
  scale_x_datetime(limits = c(p0, p1), date_labels = "%d %b") + labs(x = NULL)
gf_series <- function(x) {   # x: chamber-level rows of one system; hourly gap-filled flux, mean of stands
  s <- x %>% group_by(stand, hour_of_obs, id) %>% summarise(f = mean(fluxL_umolm2sec), .groups = "drop") %>%
    group_by(stand, hour_of_obs) %>% filter(n() >= 3) %>% summarise(f = mean(f), .groups = "drop") %>%
    left_join(metp %>% rename(hour_of_obs = hr), by = "hour_of_obs") %>% mutate(h = hour(hour_of_obs))
  bind_rows(lapply(unique(s$stand), function(st) {
    z <- s %>% filter(stand == st); g <- gam(f ~ s(s10t, k = 6) + s(h, bs = "cc", k = 8), data = z, knots = list(h = c(-0.5, 23.5)))
    tibble(hour_of_obs = seq(p0, p1 - 3600, by = "hour")) %>% left_join(metp %>% rename(hour_of_obs = hr), by = "hour_of_obs") %>%
      mutate(h = hour(hour_of_obs)) %>% left_join(z %>% select(hour_of_obs, f), by = "hour_of_obs") %>%
      mutate(ff = coalesce(f, predict(g, newdata = .)), stand = st)
  })) %>% group_by(hour_of_obs) %>% summarise(ff = mean(ff), .groups = "drop") %>% arrange(hour_of_obs) %>%
    mutate(cum = cumsum(ff) * 3600 * 12.011e-6)
}
boot_cum <- function(x, B = 100) {
  ids <- x %>% distinct(stand, id)
  sims <- replicate(B, { bi <- ids %>% group_by(stand) %>% slice_sample(prop = 1, replace = TRUE) %>% mutate(bid = paste0(id, "_", row_number())) %>% ungroup()
    gf_series(bi %>% inner_join(x, by = c("stand", "id"), relationship = "many-to-many") %>% mutate(id = bid))$cum })
  tibble(lo = apply(sims, 1, quantile, 0.025), hi = apply(sims, 1, quantile, 0.975))
}
set.seed(20260930)
d_scr <- build_dataset(qc = "screened")
cum <- bind_rows(
  bind_cols(gf_series(d %>% filter(method == "autochamber")), boot_cum(d %>% filter(method == "autochamber"))) %>% mutate(series = "Autochamber"),
  bind_cols(gf_series(d %>% filter(method == "fluxbot")), boot_cum(d %>% filter(method == "fluxbot"))) %>% mutate(series = "Fluxbot 2.0, as deployed"),
  gf_series(d_scr %>% filter(method == "fluxbot")) %>% mutate(lo = NA_real_, hi = NA_real_, series = "Fluxbot 2.0, RH-screened"))
cpal <- c("Autochamber" = unname(pal["autochamber"]), "Fluxbot 2.0, as deployed" = "#4D4D4D", "Fluxbot 2.0, RH-screened" = "#4D4D4D")
p2c <- ggplot(cum, aes(hour_of_obs, cum, colour = series, fill = series, linetype = series)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15, colour = NA, na.rm = TRUE) + geom_line(linewidth = 0.6) +
  scale_colour_manual(values = cpal, name = NULL) + scale_fill_manual(values = cpal, name = NULL) +
  scale_linetype_manual(values = c("Autochamber" = "solid", "Fluxbot 2.0, as deployed" = "solid", "Fluxbot 2.0, RH-screened" = "22"), name = NULL) +
  scale_x_datetime(limits = c(p0, p1), date_labels = "%d %b") +
  labs(x = NULL, y = expression("Cumulative CO"[2]*"-C (g C m"^-2*")")) + theme(legend.position = c(0.25, 0.8))
save_fig(p2a / p2b / p2c + plot_layout(heights = c(2.4, 0.9, 1.3)) + plot_annotation(tag_levels = "a"),
         "Fig2_timeseries", 190, 200)

# ---- SI: chamber-level distributions ----------------------------------------------
ord <- d %>% group_by(id) %>% summarise(m = mean(fluxL_umolm2sec)) %>% arrange(m)
p3 <- ggplot(d %>% mutate(id = factor(id, levels = ord$id)),
             aes(fluxL_umolm2sec, id)) +
  geom_jitter(aes(colour = method), height = 0.2, size = 0.2, alpha = 0.2, stroke = 0) +
  geom_boxplot(outliers = FALSE, fill = NA, linewidth = 0.3, width = 0.6) +
  facet_wrap(~stand_label, scales = "free_y") +
  scale_colour_manual(values = pal, labels = lab_sys, name = NULL) +
  guides(colour = guide_legend(override.aes = list(size = 1.5, alpha = 1))) +
  labs(x = flux_lab, y = NULL) + theme(legend.position = "bottom")
save_fig(p3, "FigS_chamber_distributions", 140, 120)

# ---- SI: GAM observed vs fitted -----------------------------------------------------
g <- readRDS(file.path(out_dir, "gam_re.rds"))
d$fitted <- fitted(g)
fit_of <- lm(fluxL_umolm2sec ~ fitted, data = d)
p4 <- ggplot(d, aes(fitted, fluxL_umolm2sec)) +
  geom_point(aes(colour = method), size = 0.3, alpha = 0.2, stroke = 0) +
  geom_abline(linetype = "dashed") +
  scale_colour_manual(values = pal, labels = lab_sys, name = NULL) +
  guides(colour = guide_legend(override.aes = list(size = 1.5, alpha = 1))) +
  annotate("text", x = -Inf, y = Inf, hjust = -0.05, vjust = 1.3, size = 2.6,
           label = sprintf("Adj. R\u00b2 = %.2f", num("gam_r2adj"))) +
  labs(x = expression(GAM ~ fitted ~ flux ~ (mu * mol ~ m^-2 ~ s^-1)), y = expression(Observed ~ flux ~ (mu * mol ~ m^-2 ~ s^-1))) +
  coord_equal() + theme(legend.position = c(0.8, 0.12))
save_fig(p4, "FigS_gam_fit", 90, 90)

# ---- SI: distributions in compared hours ----------------------------------------------
hrs <- matched_hours(d, 3)
d5 <- d %>% filter(hour_of_obs %in% hrs)
m5 <- d5 %>% group_by(method) %>% summarise(m = mean(fluxL_umolm2sec))
p5 <- ggplot(d5, aes(fluxL_umolm2sec, fill = method, colour = method)) +
  geom_density(alpha = 0.5, linewidth = 0.3) +
  geom_vline(data = m5, aes(xintercept = m, colour = method), linetype = "dashed", linewidth = 0.4) +
  scale_fill_manual(values = pal, labels = lab_sys, name = NULL) +
  scale_colour_manual(values = pal, labels = lab_sys, name = NULL) +
  labs(x = flux_lab, y = "Density") + theme(legend.position = c(0.8, 0.8))
save_fig(p5, "FigS_flux_distributions", 90, 70)

# ---- Fig 3: array-level agreement --------------------------------------------------------
# hourly array means (mean of the two stand means) in compared hours (>= 3 units of each system
# per stand), as deployed; filled points = hours also in the RH-screened comparison, open = hours
# present only as deployed (wet sensors). Large points: daily means (days with >= 12 compared hours).
# (b) Bland-Altman: difference vs mean, with mean difference and 95% limits of agreement.
arr <- function(dd) { hrs <- matched_hours(dd, 3)
  dd %>% filter(hour_of_obs %in% hrs) %>% group_by(hour_of_obs, stand, method) %>% summarise(f = mean(fluxL_umolm2sec), .groups = "drop") %>%
    group_by(hour_of_obs, method) %>% summarise(f = mean(f), .groups = "drop") %>% pivot_wider(names_from = method, values_from = f) }
hd <- arr(d); hs <- arr(d_scr)
hd <- hd %>% mutate(subset = if_else(hour_of_obs %in% hs$hour_of_obs, "also RH-screened", "as deployed only"))
dy <- hd %>% mutate(day = as.Date(hour_of_obs, tz = "America/New_York")) %>% group_by(day) %>% filter(n() >= 12) %>%
  summarise(autochamber = mean(autochamber), fluxbot = mean(fluxbot))
lim <- range(c(hd$autochamber, hd$fluxbot)); lim <- c(floor(lim[1] * 2) / 2, ceiling(lim[2] * 2) / 2)
sma_b <- sign(cor(hd$autochamber, hd$fluxbot)) * sd(hd$fluxbot) / sd(hd$autochamber); sma_a <- mean(hd$fluxbot) - sma_b * mean(hd$autochamber)
st <- function(x) sprintf("%s: r = %.2f, offset = %+.0f%%, CCC = %.2f", x$lab, cor(x$a, x$f), 100 * (mean(x$f) / mean(x$a) - 1), epiR::epi.ccc(x$a, x$f)$rho.c$est)
lab3 <- paste(st(list(lab = "Hourly, as deployed", a = hd$autochamber, f = hd$fluxbot)),
              st(list(lab = "Hourly, RH-screened", a = hs$autochamber, f = hs$fluxbot)),
              st(list(lab = "Daily, as deployed", a = dy$autochamber, f = dy$fluxbot)), sep = "\n")
p3a <- ggplot(hd, aes(autochamber, fluxbot)) + geom_abline(linetype = "dashed") +
  geom_point(aes(shape = subset), size = 0.9, alpha = 0.6) + scale_shape_manual(values = c("also RH-screened" = 16, "as deployed only" = 1), name = "Hourly means") +
  geom_point(data = dy, size = 2.2, colour = "#B2182B") +
  geom_abline(intercept = sma_a, slope = sma_b, colour = "#2F5D9E", linewidth = 0.6) +
  annotate("text", x = lim[1], y = lim[2], hjust = 0, vjust = 1, size = 2.1, label = lab3, lineheight = 0.95) +
  coord_equal(xlim = lim, ylim = lim) + theme(legend.position = "bottom") +
  labs(x = expression(Autochamber ~ array ~ mean ~ (mu * mol ~ m^-2 ~ s^-1)), y = expression(Fluxbot ~ array ~ mean ~ (mu * mol ~ m^-2 ~ s^-1)))
ba <- bind_rows(hd %>% mutate(ds = "As deployed"), hs %>% mutate(ds = "RH-screened")) %>% mutate(m = (autochamber + fluxbot) / 2, df = fluxbot - autochamber)
bal <- ba %>% group_by(ds) %>% summarise(mu = mean(df), lo = mu - 1.96 * sd(df), hi = mu + 1.96 * sd(df), .groups = "drop")
p3b <- ggplot(ba, aes(m, df)) + geom_hline(yintercept = 0, colour = "grey60") +
  geom_point(size = 0.7, alpha = 0.5) + geom_hline(data = bal, aes(yintercept = mu), colour = "#2F5D9E") +
  geom_hline(data = bal, aes(yintercept = lo), linetype = "22", colour = "#2F5D9E") + geom_hline(data = bal, aes(yintercept = hi), linetype = "22", colour = "#2F5D9E") +
  geom_text(data = bal, aes(x = Inf, y = hi, label = sprintf("mean %.2f\n95%% LoA %.2f to %.2f", mu, lo, hi)), hjust = 1.05, vjust = -0.3, size = 2.1, inherit.aes = FALSE) +
  facet_wrap(~ ds, ncol = 1) + labs(x = expression(Mean ~ of ~ systems ~ (mu * mol ~ m^-2 ~ s^-1)), y = expression(Fluxbot - autochamber ~ (mu * mol ~ m^-2 ~ s^-1)))
save_fig(p3a + p3b + plot_layout(widths = c(1.3, 1)) + plot_annotation(tag_levels = "a"), "Fig3_array_agreement", 190, 105)

# ---- Fig 4: diel pattern (common stand-hours) --------------------------------------------
diel <- read.csv(file.path(out_dir, "diel_common_window.csv"))
p7 <- ggplot(diel, aes(hour, mean, colour = method, fill = method)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.2, colour = NA) +
  geom_line(linewidth = 0.5) + geom_point(size = 0.8) +
  scale_colour_manual(values = pal, labels = lab_sys, name = NULL) +
  scale_fill_manual(values = pal, labels = lab_sys, name = NULL) +
  scale_x_continuous(breaks = seq(0, 24, 4)) +
  labs(x = "Hour of day (EDT)", y = flux_lab) + theme(legend.position = c(0.2, 0.88))
save_fig(p7, "Fig4_diel", 90, 70)

# ---- Fig 5: temperature response (common stand-hours) -------------------------------------
common <- d %>% distinct(stand, hour_of_obs, method) %>% count(stand, hour_of_obs) %>% filter(n == 2)
dq <- d %>% semi_join(common, by = c("stand", "hour_of_obs")) %>% filter(!is.na(s10t), fluxL_umolm2sec > 0)
tseq <- seq(min(dq$s10t), max(dq$s10t), length.out = 100)
pred <- bind_rows(lapply(split(dq, dq$method), function(x) {
  m <- nls(fluxL_umolm2sec ~ a * exp(b * s10t), data = x, start = list(a = 1, b = 0.1))
  p <- coef(m); J <- cbind(exp(p["b"] * tseq), p["a"] * tseq * exp(p["b"] * tseq))
  se <- sqrt(rowSums((J %*% vcov(m)) * J))
  data.frame(method = x$method[1], s10t = tseq, f = p["a"] * exp(p["b"] * tseq), se = se)
})) %>% mutate(method_label = factor(lab_sys[as.character(method)], levels = lab_sys))
qlab <- data.frame(method_label = factor(lab_sys, levels = lab_sys),
                   lab = c(sprintf("Q[10] == %.2f ~ (%.2f-%.2f)", num("q10_autochamber_q10"), num("q10_autochamber_lo"), num("q10_autochamber_hi")),
                           sprintf("Q[10] == %.2f ~ (%.2f-%.2f)", num("q10_fluxbot_q10"), num("q10_fluxbot_lo"), num("q10_fluxbot_hi"))))
p8 <- ggplot(dq, aes(s10t, fluxL_umolm2sec)) +
  geom_point(aes(colour = method), size = 0.5, alpha = 0.3, stroke = 0) +
  geom_ribbon(data = pred, aes(y = f, ymin = f - 1.96 * se, ymax = f + 1.96 * se), alpha = 0.3) +
  geom_line(data = pred, aes(y = f), linewidth = 0.6) +
  geom_text(data = qlab, aes(x = -Inf, y = Inf, label = lab), parse = TRUE, hjust = -0.05, vjust = 1.3, size = 2.5) +
  facet_wrap(~method_label) + scale_colour_manual(values = pal, guide = "none") +
  labs(x = "Soil temperature at 10 cm, HF001 (\u00b0C)", y = flux_lab)
save_fig(p8, "Fig5_temperature_response", 140, 70)

# ---- Fig 6: spatial heterogeneity and sampling effort ---------------------------------------
lz <- d %>% group_by(hour_of_obs) %>%
  filter(sum(method == "autochamber") >= 12 & sum(method == "fluxbot") >= 12) %>% ungroup() %>%
  group_by(method, id) %>% summarise(mf = mean(fluxL_umolm2sec), .groups = "drop") %>%
  group_by(method) %>% group_modify(~ { lc <- Lc(.x$mf); data.frame(p = lc$p, L = lc$L, gini = Gini(.x$mf)) }) %>%
  ungroup()
glab <- lz %>% distinct(method, gini) %>%
  mutate(lab = sprintf("%s: Gini = %.2f", lab_sys[as.character(method)], gini), y = c(0.95, 0.87))
p9a <- ggplot(lz, aes(p, L, colour = method)) +
  geom_abline(linetype = "dashed") + geom_line(linewidth = 0.5) + geom_point(size = 0.8) +
  geom_text(data = glab, aes(x = 0.02, y = y, label = lab), hjust = 0, size = 2.5, show.legend = FALSE) +
  scale_colour_manual(values = pal, guide = "none") + coord_equal() +
  labs(x = "Cumulative share of chambers", y = "Cumulative share of flux")
eff <- read.csv(file.path(out_dir, "sampling_effort.csv")) %>%
  mutate(stand_label = if_else(stand == "healthy", "Stand 1", "Stand 2"))
p9b <- ggplot(eff %>% filter(n >= 2), aes(n, 100 * analytic, colour = method, linetype = stand_label)) +
  geom_hline(yintercept = c(10, 20), colour = "grey70", linewidth = 0.3) +
  geom_line(linewidth = 0.5) +
  geom_point(data = eff %>% filter(n == n_chambers), size = 1.2) +
  scale_colour_manual(values = pal, labels = lab_sys, name = NULL) +
  scale_linetype_manual(values = c("solid", "22"), name = NULL) +
  scale_y_continuous(limits = c(0, 60), oob = squish) +
  labs(x = "Number of chambers", y = "95% CI half-width of stand mean (% of mean)") +
  theme(legend.position = c(0.72, 0.78), legend.spacing.y = unit(0, "mm"))
save_fig(p9a + p9b + plot_annotation(tag_levels = "a"), "Fig6_heterogeneity_effort", 190, 85)

# Fig 7 (measurement flow) is drawn by 11_filter_flow.R and Fig 8 (uptime and coverage) by 17_resilience.R

cat("Figures written to", fig_dir, "\n")
