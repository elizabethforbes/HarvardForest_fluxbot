# Figures for the AFM submission (task A10). Each figure is written as a vector
# PDF and a 600 dpi LZW TIFF, sized to Elsevier column widths
# (single = 90 mm, 1.5 = 140 mm, double = 190 mm).
# Figure numbering follows the revised manuscript: Fig. 1 is the field photo
# (not generated here), so the data figures are Figs. 2-10.

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

d <- readRDS(file.path(out_dir, "dataset_iqr.rds")) %>%
  mutate(stand_label = factor(if_else(stand == "healthy", "Stand 1", "Stand 2")),
         method_label = factor(lab_sys[as.character(method)], levels = lab_sys))
nums <- read.csv(file.path(out_dir, "numbers_for_text.csv"))
num <- function(k) nums$value[nums$key == k]

# ---- Fig 2: time series -------------------------------------------------------------
# points: individual fluxes; line: 24-h centred rolling mean of hourly stand-array
# means on a complete hourly grid (requires >= 12 of 24 hours)
roll <- d %>% group_by(stand_label, method_label, hour_of_obs) %>%
  summarise(f = mean(fluxL_umolm2sec), .groups = "drop") %>%
  group_by(stand_label, method_label) %>%
  complete(hour_of_obs = seq(min(hour_of_obs), max(hour_of_obs), by = "hour")) %>%
  arrange(hour_of_obs, .by_group = TRUE) %>%
  mutate(r = rollapply(f, 24, function(x) if (sum(!is.na(x)) >= 12) mean(x, na.rm = TRUE) else NA,
                       fill = NA, align = "center")) %>% ungroup()
tsoil <- readRDS(file.path(out_dir, "soiltemp_local_vs_hf001.rds")) %>%
  mutate(stand_label = if_else(stand == "healthy", "Stand 1", "Stand 2"))
met <- load_met()
xl <- range(d$hour_of_obs)
p2a <- ggplot(d, aes(hour_of_obs, fluxL_umolm2sec)) +
  geom_point(aes(colour = method), size = 0.5, alpha = 0.35, stroke = 0) +
  geom_line(data = roll, aes(y = r), linewidth = 0.5, na.rm = TRUE) +
  facet_grid(method_label ~ stand_label) + scale_colour_manual(values = pal, guide = "none") +
  scale_x_datetime(limits = xl, date_labels = "%d %b") + labs(x = NULL, y = flux_lab)
p2b <- ggplot() +
  geom_line(data = met, aes(Time, s10t, linetype = "HF001 (10 cm)"), linewidth = 0.4) +
  geom_line(data = tsoil, aes(hour_of_obs, tsoil_local, colour = stand_label), linewidth = 0.4) +
  scale_colour_manual(values = c("Stand 1" = "#C2703D", "Stand 2" = "#6A4C93"), name = "Autochamber\nsoil probes") +
  scale_linetype_manual(values = "solid", name = NULL) +
  scale_x_datetime(limits = xl, date_labels = "%d %b") +
  labs(x = NULL, y = "Soil temp. (\u00b0C)") + theme(legend.position = "right")
save_fig(p2a / p2b + plot_layout(heights = c(3, 1)) + plot_annotation(tag_levels = "a"),
         "Fig2_timeseries", 190, 150)

# ---- Fig 3: chamber-level distributions ----------------------------------------------
ord <- d %>% group_by(id) %>% summarise(m = mean(fluxL_umolm2sec)) %>% arrange(m)
p3 <- ggplot(d %>% mutate(id = factor(id, levels = ord$id)),
             aes(fluxL_umolm2sec, id)) +
  geom_jitter(aes(colour = method), height = 0.2, size = 0.2, alpha = 0.2, stroke = 0) +
  geom_boxplot(outliers = FALSE, fill = NA, linewidth = 0.3, width = 0.6) +
  facet_wrap(~stand_label, scales = "free_y") +
  scale_colour_manual(values = pal, labels = lab_sys, name = NULL) +
  guides(colour = guide_legend(override.aes = list(size = 1.5, alpha = 1))) +
  labs(x = flux_lab, y = NULL) + theme(legend.position = "bottom")
save_fig(p3, "Fig3_chamber_distributions", 140, 120)

# ---- Fig 4: GAM observed vs fitted -----------------------------------------------------
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
save_fig(p4, "Fig4_gam_fit", 90, 90)

# ---- Fig 5: distributions in matched hours ----------------------------------------------
hrs <- matched_hours(d, 5)
d5 <- d %>% filter(hour_of_obs %in% hrs)
m5 <- d5 %>% group_by(method) %>% summarise(m = mean(fluxL_umolm2sec))
p5 <- ggplot(d5, aes(fluxL_umolm2sec, fill = method, colour = method)) +
  geom_density(alpha = 0.5, linewidth = 0.3) +
  geom_vline(data = m5, aes(xintercept = m, colour = method), linetype = "dashed", linewidth = 0.4) +
  scale_fill_manual(values = pal, labels = lab_sys, name = NULL) +
  scale_colour_manual(values = pal, labels = lab_sys, name = NULL) +
  labs(x = flux_lab, y = "Density") + theme(legend.position = c(0.8, 0.8))
save_fig(p5, "Fig5_flux_distributions", 90, 70)

# ---- Fig 6: array-level agreement --------------------------------------------------------
f6 <- readRDS(file.path(out_dir, "fig5_data.rds"))
lim <- range(c(f6$ac3, f6$fb3)); lim <- c(floor(lim[1] * 2) / 2, ceiling(lim[2] * 2) / 2)
sma_b <- sign(cor(f6$ac3, f6$fb3)) * sd(f6$fb3) / sd(f6$ac3); sma_a <- mean(f6$fb3) - sma_b * mean(f6$ac3)
p6 <- ggplot(f6, aes(ac3, fb3)) +
  geom_abline(linetype = "dashed") +
  geom_point(size = 0.8, alpha = 0.5) +
  geom_abline(intercept = sma_a, slope = sma_b, colour = "#2F5D9E", linewidth = 0.6) +
  annotate("text", x = lim[1], y = lim[2], hjust = 0, vjust = 1, size = 2.5,
           label = sprintf("r = %.2f\noffset = %.2f (%.0f%%)\nSMA slope = %.2f\nCCC = %.2f",
                           num("ccc_pearson_r"), num("paired_bias"), num("paired_bias_pct"), sma_b, num("ccc"))) +
  coord_equal(xlim = lim, ylim = lim) +
  labs(x = expression(Autochamber ~ array ~ mean ~ (mu * mol ~ m^-2 ~ s^-1)),
       y = expression(Fluxbot ~ array ~ mean ~ (mu * mol ~ m^-2 ~ s^-1)))
save_fig(p6, "Fig6_array_agreement", 90, 90)

# ---- Fig 7: diel pattern (common stand-hours) --------------------------------------------
diel <- read.csv(file.path(out_dir, "diel_common_window.csv"))
p7 <- ggplot(diel, aes(hour, mean, colour = method, fill = method)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.2, colour = NA) +
  geom_line(linewidth = 0.5) + geom_point(size = 0.8) +
  scale_colour_manual(values = pal, labels = lab_sys, name = NULL) +
  scale_fill_manual(values = pal, labels = lab_sys, name = NULL) +
  scale_x_continuous(breaks = seq(0, 24, 4)) +
  labs(x = "Hour of day (EDT)", y = flux_lab) + theme(legend.position = c(0.2, 0.88))
save_fig(p7, "Fig7_diel", 90, 70)

# ---- Fig 8: temperature response (common stand-hours) -------------------------------------
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
save_fig(p8, "Fig8_temperature_response", 140, 70)

# ---- Fig 9: spatial heterogeneity and sampling effort ---------------------------------------
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
save_fig(p9a + p9b + plot_annotation(tag_levels = "a"), "Fig9_heterogeneity_effort", 190, 85)

# ---- Fig 10: data collection and replicated coverage -----------------------------------------
fb_raw <- load_fluxbot(); ac_raw <- load_autochamber()
per_day <- c(fluxbot = 24, autochamber = 48)
days <- seq(as.Date("2023-10-02"), as.Date("2023-10-31"), by = "day")
coll <- bind_rows(fb_raw, ac_raw) %>% mutate(date = as.Date(hour_of_obs)) %>%
  filter(date %in% days) %>% count(method, stand, id, date) %>%
  complete(nesting(method, stand, id), date = days, fill = list(n = 0)) %>%
  mutate(rate = pmin(1, n / per_day[method]),
         stand_label = if_else(stand == "healthy", "Stand 1", "Stand 2"),
         unit = paste(lab_sys[method], sub("fluxes_bot", "", id)))
p10a <- ggplot(coll, aes(date, unit, fill = 100 * rate)) +
  geom_tile() + facet_grid(stand_label ~ ., scales = "free_y", space = "free_y") +
  scale_fill_viridis_c(name = "Measurements\ncollected (%)", option = "D") +
  scale_x_date(date_labels = "%d %b", expand = c(0, 0)) + labs(x = NULL, y = NULL) +
  theme(axis.text.y = element_text(size = 5))
cov <- d %>% filter(as.Date(hour_of_obs) %in% days) %>% count(method, stand_label, hour_of_obs) %>%
  mutate(date = as.Date(hour_of_obs)) %>% group_by(method, stand_label, date) %>%
  summarise(cov = sum(n >= 3) / 24, .groups = "drop") %>%
  complete(nesting(method, stand_label), date = days, fill = list(cov = 0))
p10b <- ggplot(cov, aes(date, 100 * cov, colour = method)) +
  geom_line(linewidth = 0.5) + facet_wrap(~stand_label) +
  scale_colour_manual(values = pal, labels = lab_sys, name = NULL) +
  scale_x_date(date_labels = "%d %b") +
  labs(x = NULL, y = "Hours with \u22653 chambers\nreporting (% of day)") + theme(legend.position = "bottom")
save_fig(p10a / p10b + plot_layout(heights = c(2.2, 1)) + plot_annotation(tag_levels = "a"),
         "Fig10_uptime_coverage", 190, 170)

cat("Figures written to", fig_dir, "\n")
