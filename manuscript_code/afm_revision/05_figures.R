# Figures for the AFM submission (task A10). Each figure is written as a vector
# PDF and a 600 dpi LZW TIFF, sized to Elsevier column widths
# (single = 90 mm, 1.5 = 140 mm, double = 190 mm).
# Main figures (Fig. 1 is the field photo, not generated here):
#   Fig 2 time series and cumulative budgets; Fig 3 chamber-level distributions; Fig 4 agreement
#   (scatter + Bland-Altman); Fig 5 diel; Fig 6 temperature response; Fig 7 heterogeneity and
#   sampling effort; Fig 8 measurement flow (11_filter_flow.R); Fig 9 uptime and coverage (17_resilience.R).
# SI figures written here: GAM fit, flux distributions.
# Datasets: "as deployed" (main; dataset_main.rds) and the "RH-screened" subset.

source("afm_revision/00_prep.R")
suppressPackageStartupMessages({
  library(ggplot2); library(patchwork); library(mgcv); library(zoo); library(ineq); library(scales)
})

fig_dir <- file.path(out_dir, "figures"); dir.create(fig_dir, showWarnings = FALSE)
source("afm_revision/fig_style.R")
pal <- pal_sys
flux_lab <- expression(CO[2] ~ flux ~ (mu * mol ~ m^-2 ~ s^-1))
fig_dir <- file.path(out_dir, "figures"); dir.create(fig_dir, showWarnings = FALSE)
save_fig <- function(p, name, width_mm, height_mm) save_afm(p, name, width_mm, height_mm)

d <- readRDS(file.path(out_dir, "dataset_main.rds")) %>%
  mutate(stand_label = factor(if_else(stand == "healthy", "Stand 1", "Stand 2")),
         method_label = factor(lab_sys[as.character(method)], levels = lab_sys))
met <- load_met()
nums <- read.csv(file.path(out_dir, "numbers_for_text.csv"))
num <- function(k) nums$value[nums$key == k]

# ---- Fig 2: time series and cumulative budgets ---------------------------------------------
# blue ticks mark hours when at least half of the stand's recording Fluxbots were wet
# (b) soil temperature (HF001, 10 cm) and hourly precipitation
# (c) cumulative CO2-C from 2 Oct, gap-filled per stand (s10t + time-of-day GAM, as in
#     12_scales_budget.R), mean of the two stands; bands = chamber-bootstrap 95% intervals
p0 <- as.POSIXct("2023-10-02", tz = "America/New_York"); p1 <- as.POSIXct("2023-11-01", tz = "America/New_York")
# (a) individual chamber fluxes (points) by system (rows) and stand (columns); thick line = this
#     system's 24-h centred rolling mean of stand means; thin dashed line = the other system's
dd2 <- d %>% filter(hour_of_obs >= p0, hour_of_obs < p1)
roll <- dd2 %>% group_by(stand_label, method, hour_of_obs) %>% summarise(f = mean(fluxL_umolm2sec), n = n(), .groups = "drop") %>% filter(n >= 3) %>%
  group_by(stand_label, method) %>% complete(hour_of_obs = seq(p0, p1 - 3600, by = "hour")) %>% arrange(hour_of_obs, .by_group = TRUE) %>%
  mutate(r = rollapply(f, 24, function(x) if (sum(!is.na(x)) >= 12) mean(x, na.rm = TRUE) else NA, fill = NA, align = "center")) %>% ungroup()
own <- roll %>% mutate(method_label = factor(lab_sys[as.character(method)], levels = lab_sys))
other <- roll %>% mutate(method_label = factor(lab_sys[if_else(method == "fluxbot", "autochamber", "fluxbot")], levels = lab_sys))
wet_h <- load_fluxbot() %>% filter(!lid_fail) %>% group_by(stand, hour_of_obs) %>% summarise(w = mean(wet), .groups = "drop") %>%
  filter(w >= 0.5, hour_of_obs >= p0, hour_of_obs < p1) %>%
  mutate(stand_label = if_else(stand == "healthy", "Stand 1", "Stand 2"), method_label = factor(lab_sys["fluxbot"], levels = lab_sys))
xs2 <- scale_x_datetime(limits = c(p0, p1), date_labels = "%d %b", date_breaks = "1 week", expand = c(0.01, 0))
p2a <- ggplot(dd2, aes(hour_of_obs, fluxL_umolm2sec)) +
  geom_rug(data = wet_h, aes(x = hour_of_obs), inherit.aes = FALSE, sides = "b", colour = col_wet, alpha = 0.7, length = unit(2, "mm")) +
  geom_point(aes(colour = method), size = 0.55, alpha = 0.45, stroke = 0) +
  geom_line(data = other, aes(y = r), colour = "grey20", linewidth = 0.35, linetype = "22", na.rm = TRUE) +
  geom_line(data = own, aes(y = r), colour = "black", linewidth = 0.8, na.rm = TRUE) +
  facet_grid(method_label ~ stand_label) + scale_colour_manual(values = pal, guide = "none") +
  coord_cartesian(ylim = c(0, 7)) + xs2 + labs(x = NULL, y = flux_lab)
metp <- met %>% mutate(hr = floor_date(with_tz(Time, "America/New_York"), "hour")) %>% group_by(hr) %>%
  summarise(s10t = mean(s10t), prec = sum(prec), .groups = "drop") %>% filter(hr >= p0, hr < p1)
sc <- max(metp$prec, na.rm = TRUE) / 20
p2b <- ggplot(metp, aes(hr)) + geom_col(aes(y = prec / sc), fill = col_wet, alpha = 0.6, width = 3600) +
  geom_line(aes(y = s10t), linewidth = 0.4) +
  scale_y_continuous(name = expression(Soil ~ T ~ (degree * C)), sec.axis = sec_axis(~ . * sc, name = expression(Rain ~ (mm ~ h^-1)))) +
  xs2 + labs(x = NULL)
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
  xs2 + labs(x = NULL, y = expression("Cumulative C (g m"^-2*")")) +
  theme(legend.position = c(0.22, 0.78), legend.key.height = unit(3, "mm"))
save_fig(p2a / p2b / p2c + plot_layout(heights = c(3.4, 0.7, 1.2)) + tags_afm(),
         "Fig2_timeseries", 190, 210)

# ---- Fig 3: chamber-level distributions ----------------------------------------------
# four arrays (system x stand); chambers ordered by mean flux within each array
d3 <- d %>% mutate(array = factor(paste0(lab_sys[as.character(method)], ", ", stand_label),
                                  levels = c("Autochamber, Stand 1", "Autochamber, Stand 2", "Fluxbot 2.0, Stand 1", "Fluxbot 2.0, Stand 2")),
                   unit = sub("^(autochamber|fluxbot|fluxes_bot)", "", as.character(id)))
ord <- d3 %>% group_by(array, unit) %>% summarise(m = mean(fluxL_umolm2sec), .groups = "drop") %>% arrange(array, m) %>% mutate(key = paste(array, unit))
d3 <- d3 %>% mutate(key = factor(paste(array, unit), levels = ord$key))
p3 <- ggplot(d3, aes(fluxL_umolm2sec, key)) +
  geom_jitter(aes(colour = method), height = 0.2, size = 0.25, alpha = 0.25, stroke = 0) +
  geom_boxplot(outliers = FALSE, fill = NA, linewidth = 0.3, width = 0.6) +
  facet_wrap(~ array, scales = "free_y", ncol = 2) +
  scale_y_discrete(labels = function(k) sub(".* ", "", k)) +
  scale_colour_manual(values = pal, guide = "none") + coord_cartesian(xlim = c(0, 7.5)) +
  labs(x = flux_lab, y = "Chamber or unit")
save_fig(p3, "Fig3_chamber_distributions", 140, 130)

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

# ---- Fig 4: array-level agreement --------------------------------------------------------
# hourly array means (mean of the two stand means) in compared hours (>= 3 units of each system per
# stand), as deployed. Grey: hours that are also in the RH-screened comparison (dry sensors); blue:
# hours present only as deployed (wet sensors). Outlined points: daily means (>= 12 compared hours).
# (b) Bland-Altman plot of the same hourly means: mean difference and 95% limits of agreement.
arr <- function(dd) { hrs <- matched_hours(dd, 3)
  dd %>% filter(hour_of_obs %in% hrs) %>% group_by(hour_of_obs, stand, method) %>% summarise(f = mean(fluxL_umolm2sec), .groups = "drop") %>%
    group_by(hour_of_obs, method) %>% summarise(f = mean(f), .groups = "drop") %>% pivot_wider(names_from = method, values_from = f) }
hd <- arr(d); hs <- arr(d_scr)
hd <- hd %>% mutate(subset = if_else(hour_of_obs %in% hs$hour_of_obs, "Hourly, dry sensors", "Hourly, wet sensors"))
dy <- hd %>% mutate(day = as.Date(hour_of_obs, tz = "America/New_York")) %>% group_by(day) %>% filter(n() >= 12) %>%
  summarise(autochamber = mean(autochamber), fluxbot = mean(fluxbot))
stt <- function(a, f) c(sprintf("%.2f", cor(a, f)), sprintf("%+.0f%%", 100 * (mean(f) / mean(a) - 1)), sprintf("%.2f", epiR::epi.ccc(a, f)$rho.c$est))
tabd <- rbind(stt(hd$autochamber, hd$fluxbot), stt(hs$autochamber, hs$fluxbot), stt(dy$autochamber, dy$fluxbot))
dimnames(tabd) <- list(c("Hourly, as deployed", "Hourly, RH-screened", "Daily, as deployed"), c("r", "Offset", "CCC"))
tg <- gridExtra::tableGrob(tabd, theme = gridExtra::ttheme_minimal(base_size = 6.5, padding = unit(c(3, 1.6), "mm"),
        core = list(fg_params = list(hjust = 1, x = 0.9)), rowhead = list(fg_params = list(hjust = 0, x = 0.05, fontface = "plain")),
        colhead = list(fg_params = list(fontface = "bold"))))
cols4 <- c("Hourly, dry sensors" = "grey45", "Hourly, wet sensors" = col_wet)
sma_b <- sign(cor(hd$autochamber, hd$fluxbot)) * sd(hd$fluxbot) / sd(hd$autochamber); sma_a <- mean(hd$fluxbot) - sma_b * mean(hd$autochamber)
lim <- c(0.8, 5)
lgd <- theme(legend.position = c(0.02, 0.98), legend.justification = c(0, 1), legend.title = element_blank(),
             legend.background = element_rect(fill = alpha("white", 0.8), colour = NA), legend.key.size = unit(3, "mm"),
             legend.spacing.y = unit(0, "mm"), legend.margin = margin(1, 2, 1, 2))
p4a <- ggplot(hd, aes(autochamber, fluxbot)) +
  geom_abline(aes(intercept = 0, slope = 1, linetype = "1:1"), colour = "grey30", linewidth = 0.4) +
  geom_abline(aes(intercept = sma_a, slope = sma_b, linetype = "SMA fit"), colour = "black", linewidth = 0.5) +
  geom_point(aes(colour = subset), size = 0.9, alpha = 0.6, stroke = 0) +
  geom_point(data = dy, aes(fill = "Daily means"), shape = 21, size = 2.1, colour = "black", stroke = 0.35) +
  scale_colour_manual(values = cols4) + scale_fill_manual(values = c("Daily means" = "#F4A582")) +
  scale_linetype_manual(values = c("1:1" = "22", "SMA fit" = "solid")) +
  guides(colour = guide_legend(order = 1, override.aes = list(size = 2, alpha = 1)), fill = guide_legend(order = 2), linetype = guide_legend(order = 3)) +
  coord_equal(xlim = lim, ylim = lim, expand = FALSE) + lgd +
  labs(x = expression(Autochamber ~ array ~ mean ~ (mu * mol ~ m^-2 ~ s^-1)), y = expression(Fluxbot ~ array ~ mean ~ (mu * mol ~ m^-2 ~ s^-1)), tag = "a") +
  inset_element(tg, left = 0.5, bottom = 0.02, right = 0.99, top = 0.28, align_to = "panel", ignore_tag = TRUE)
ba <- hd %>% mutate(m = (autochamber + fluxbot) / 2, df = fluxbot - autochamber)
mu <- mean(ba$df); lo <- mu - 1.96 * sd(ba$df); hi <- mu + 1.96 * sd(ba$df)
p4b <- ggplot(ba, aes(m, df)) + geom_hline(yintercept = 0, colour = "grey75") +
  geom_point(aes(colour = subset), size = 0.9, alpha = 0.6, stroke = 0) +
  geom_hline(aes(yintercept = mu, linetype = "Mean difference"), linewidth = 0.5) +
  geom_hline(aes(yintercept = lo, linetype = "95% limits of agreement"), linewidth = 0.4) + geom_hline(aes(yintercept = hi, linetype = "95% limits of agreement"), linewidth = 0.4) +
  annotate("text", x = 4.6, y = c(mu, lo, hi), label = sprintf("%.2f", c(mu, lo, hi)), hjust = 1, vjust = -0.4, size = 2.3) +
  scale_colour_manual(values = cols4) + scale_linetype_manual(values = c("Mean difference" = "solid", "95% limits of agreement" = "22"), breaks = c("Mean difference", "95% limits of agreement")) +
  guides(colour = guide_legend(order = 1, override.aes = list(size = 2, alpha = 1)), linetype = guide_legend(order = 2)) +
  coord_cartesian(xlim = c(1, 4.6), ylim = c(-2.1, 2.3)) + lgd +
  labs(x = expression(Mean ~ of ~ the ~ two ~ systems ~ (mu * mol ~ m^-2 ~ s^-1)), y = expression(Fluxbot - autochamber ~ (mu * mol ~ m^-2 ~ s^-1)), tag = "b")
save_fig((p4a | p4b), "Fig4_array_agreement", 190, 100)

# ---- Fig 5: diel pattern (common stand-hours) --------------------------------------------
diel <- read.csv(file.path(out_dir, "diel_common_window.csv"))
p7 <- ggplot(diel, aes(hour, mean, colour = method, fill = method)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.2, colour = NA) +
  geom_line(linewidth = 0.5) + geom_point(size = 0.8) +
  scale_colour_manual(values = pal, labels = lab_sys, name = NULL) +
  scale_fill_manual(values = pal, labels = lab_sys, name = NULL) +
  scale_x_continuous(breaks = seq(0, 24, 4)) +
  labs(x = "Hour of day (EDT)", y = flux_lab) + theme(legend.position = c(0.2, 0.88))
save_fig(p7, "Fig5_diel", 90, 70)

# ---- Fig 6: temperature response (common stand-hours) -------------------------------------
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
  geom_point(aes(colour = method), size = 0.6, alpha = 0.45, stroke = 0) +
  geom_ribbon(data = pred, aes(y = f, ymin = f - 1.96 * se, ymax = f + 1.96 * se), alpha = 0.3) +
  geom_line(data = pred, aes(y = f), linewidth = 0.6) +
  geom_text(data = qlab, aes(x = -Inf, y = Inf, label = lab), parse = TRUE, hjust = -0.05, vjust = 1.3, size = 2.5) +
  facet_wrap(~method_label) + scale_colour_manual(values = pal, guide = "none") +
  labs(x = "Soil temperature at 10 cm, HF001 (\u00b0C)", y = flux_lab)
save_fig(p8, "Fig6_temperature_response", 140, 70)

# ---- Fig 7: spatial heterogeneity and sampling effort ---------------------------------------
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
p9b <- ggplot(eff %>% filter(n >= 3), aes(n, 100 * analytic, colour = method, linetype = stand_label)) +
  geom_hline(yintercept = c(10, 20), colour = "grey70", linewidth = 0.3) +
  geom_line(linewidth = 0.5) +
  geom_point(data = eff %>% filter(n == n_chambers), size = 1.2) +
  scale_colour_manual(values = pal, labels = lab_sys, name = NULL) +
  scale_linetype_manual(values = c("solid", "22"), name = NULL) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Number of chambers", y = "95% CI half-width of stand mean (% of mean)") +
  theme(legend.position = c(0.72, 0.78), legend.spacing.y = unit(0, "mm"))
save_fig(p9a + p9b + tags_afm(), "Fig7_heterogeneity_effort", 190, 85)

# Fig 8 (measurement flow) is drawn by 11_filter_flow.R and Fig 9 (uptime and coverage) by 17_resilience.R

cat("Figures written to", fig_dir, "\n")
