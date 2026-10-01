# Fig. 2. Fluxes, budgets and chamber variation, 2-31 October 2023 (as deployed).
#  (a) individual chamber fluxes by system and stand, with the 24-h mean of the gap-filled stand-mean series
#      (dark where mostly observed, light where mostly modelled); dashed = the other system's
#  (b) air temperature and rain (HF001)
#  (c) cumulative gap-filled budgets with chamber-bootstrap 95% intervals
#  (d) the gap-filling model: observed vs fitted hourly stand means, per array
#  (e) flux distribution of every chamber (ridges) with all closures (points), by array
source("R/setup.R")
source("R/fig_style.R")
suppressPackageStartupMessages({ library(ggplot2); library(patchwork); library(mgcv); library(zoo); library(ggridges) })
pal <- pal_sys
save_fig <- function(p, name, width_mm, height_mm) save_afm(p, name, width_mm, height_mm)
d <- load_chamber_hours("deployed") %>%
  mutate(stand_label = factor(if_else(stand == "healthy", "Stand 1", "Stand 2")),
         method_label = factor(lab_sys[as.character(method)], levels = lab_sys))
num <- get_number
set.seed(20260930)
met <- load_met()

# ---- Fig 2: time series and cumulative budgets ---------------------------------------------
# (b) air temperature (HF001) and hourly precipitation
# (c) cumulative CO2-C from 2 Oct, gap-filled per stand (s10t + time-of-day GAM, as in
#     2_analysis/09_scales_budget.R), mean of the two stands; bands = chamber-bootstrap 95% intervals
p0 <- as.POSIXct("2023-10-02", tz = "America/New_York"); p1 <- as.POSIXct("2023-11-01", tz = "America/New_York")
dd2 <- d %>% filter(hour_of_obs >= p0, hour_of_obs < p1)
xs2 <- scale_x_datetime(limits = c(p0, p1), date_labels = "%d %b", date_breaks = "1 week", expand = c(0.01, 0))
metp <- met %>% mutate(hr = floor_date(with_tz(Time, "America/New_York"), "hour")) %>% group_by(hr) %>%
  summarise(s10t = mean(s10t), airt = mean(airt), prec = sum(prec), .groups = "drop") %>% filter(hr >= p0, hr < p1)
sc <- max(metp$prec, na.rm = TRUE) / max(metp$airt, na.rm = TRUE)
p2b <- ggplot(metp, aes(hr)) + geom_col(aes(y = prec / sc), fill = col_wet, alpha = a_mean, width = 3600) +
  geom_line(aes(y = airt), linewidth = 0.4) +
  scale_y_continuous(name = expression(Air ~ T ~ (degree * C)), sec.axis = sec_axis(~ . * sc, name = expression(Rain ~ (mm ~ h^-1)))) +
  xs2 + labs(x = NULL)
# gap-filling model, per stand and system: hourly stand means (>= 3 chambers) ~ s(10-cm soil T) + cyclic s(hour)
# (the same model fills the budgets in c)
gf_stand <- function(z) {
  z <- z %>% left_join(metp %>% rename(hour_of_obs = hr), by = "hour_of_obs") %>% mutate(h = hour(hour_of_obs))
  g <- gam(f ~ s(s10t, k = 6) + s(h, bs = "cc", k = 8), data = z, knots = list(h = c(-0.5, 23.5)))
  full <- tibble(hour_of_obs = seq(p0, p1 - 3600, by = "hour")) %>% left_join(metp %>% rename(hour_of_obs = hr), by = "hour_of_obs") %>%
    mutate(h = hour(hour_of_obs)) %>% left_join(z %>% select(hour_of_obs, f), by = "hour_of_obs") %>%
    mutate(fit = as.numeric(predict(g, newdata = .)), filled = is.na(f), ff = coalesce(f, fit))
  full %>% mutate(r = rollapply(ff, 24, mean, fill = NA, align = "center"), fillshare = rollapply(filled, 24, mean, fill = NA, align = "center"))
}
sm <- dd2 %>% group_by(stand, stand_label, method, hour_of_obs) %>% summarise(f = mean(fluxL_umolm2sec), n = n(), .groups = "drop") %>% filter(n >= 3)
gfa <- bind_rows(lapply(split(sm, list(sm$stand, sm$method), drop = TRUE), function(z)
  gf_stand(z) %>% mutate(stand = z$stand[1], stand_label = z$stand_label[1], method = z$method[1])))
own <- gfa %>% mutate(method_label = factor(lab_sys[as.character(method)], levels = lab_sys))
other <- gfa %>% mutate(method_label = factor(lab_sys[if_else(method == "fluxbot", "autochamber", "fluxbot")], levels = lab_sys))
p2a <- ggplot(dd2, aes(hour_of_obs, fluxL_umolm2sec)) +
  geom_point(aes(colour = method), size = pt_dense, alpha = a_dense, stroke = 0) +
  geom_line(data = other, aes(y = r), colour = "grey20", linewidth = lw_thin, linetype = "22", na.rm = TRUE) +
  geom_line(data = own, aes(y = r), colour = "grey60", linewidth = 0.8, na.rm = TRUE) +
  geom_line(data = own %>% mutate(r = if_else(fillshare > 0.5, NA_real_, r)), aes(y = r), colour = col_fit, linewidth = 0.8, na.rm = TRUE) +
  facet_grid(method_label ~ stand_label) + scale_colour_manual(values = pal, guide = "none") +
  coord_cartesian(ylim = c(0, 7)) + xs2 + labs(x = NULL, y = flux_lab)
# (d) observed vs fitted hourly stand means
fitd <- gfa %>% filter(!filled) %>% mutate(array = factor(paste(lab_sys[as.character(method)], stand_label), levels = names(pal_stand)))
rlab <- fitd %>% group_by(array) %>% summarise(r = cor(f, fit), .groups = "drop") %>%
  mutate(lab = sprintf("%s: r = %.2f", array, r), y = 6.6 - 0.45 * (as.integer(array) - 1))
p2d <- ggplot(fitd, aes(fit, f, colour = array)) + geom_abline(linetype = "22", colour = col_ref) +
  geom_point(size = pt_dense, alpha = a_dense, stroke = 0) +
  geom_point(data = rlab, aes(x = 0.25, y = y), size = 1.6, show.legend = FALSE) +
  geom_text(data = rlab, aes(x = 0.55, y = y, label = lab), hjust = 0, size = txt - 0.3, colour = "black", show.legend = FALSE) +
  scale_colour_manual(values = pal_stand, guide = "none") + coord_equal(xlim = c(0, 7), ylim = c(0, 7)) +
  labs(x = expression(Gap*"-"*filling ~ model ~ (mu * mol ~ m^-2 ~ s^-1)), y = expression(Observed ~ stand ~ mean ~ (mu * mol ~ m^-2 ~ s^-1)))
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
d_scr <- build_dataset(qc = "screened")
cum <- bind_rows(
  bind_cols(gf_series(d %>% filter(method == "autochamber")), boot_cum(d %>% filter(method == "autochamber"))) %>% mutate(series = "Autochamber"),
  bind_cols(gf_series(d %>% filter(method == "fluxbot")), boot_cum(d %>% filter(method == "fluxbot"))) %>% mutate(series = "Fluxbot 2.0, as deployed"),
  gf_series(d_scr %>% filter(method == "fluxbot")) %>% mutate(lo = NA_real_, hi = NA_real_, series = "Fluxbot 2.0, RH-screened"))
cpal <- c("Autochamber" = unname(pal["autochamber"]), "Fluxbot 2.0, as deployed" = unname(pal["fluxbot"]), "Fluxbot 2.0, RH-screened" = unname(pal["fluxbot"]))
p2c <- ggplot(cum, aes(hour_of_obs, cum, colour = series, fill = series, linetype = series)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = a_band, colour = NA, na.rm = TRUE) + geom_line(linewidth = lw_main) +
  scale_colour_manual(values = cpal, name = NULL) + scale_fill_manual(values = cpal, name = NULL) +
  scale_linetype_manual(values = c("Autochamber" = "solid", "Fluxbot 2.0, as deployed" = "solid", "Fluxbot 2.0, RH-screened" = "22"), name = NULL) +
  xs2 + labs(x = NULL, y = expression("Cumulative C (g m"^-2*")")) +
  theme(legend.position = c(0.3, 0.8), legend.key.height = unit(3, "mm"))
# (e) flux distributions of every chamber, ordered by mean within each array
d3 <- dd2 %>% mutate(array = factor(paste0(lab_sys[as.character(method)], ", ", stand_label),
                                    levels = c("Autochamber, Stand 1", "Autochamber, Stand 2", "Fluxbot 2.0, Stand 1", "Fluxbot 2.0, Stand 2")),
                     unit = sub("^(autochamber|fluxbot)", "", as.character(id)))
ord <- d3 %>% group_by(array, unit) %>% summarise(m = mean(fluxL_umolm2sec), .groups = "drop") %>% arrange(array, m) %>% mutate(key = paste(array, unit))
d3 <- d3 %>% mutate(key = factor(paste(array, unit), levels = ord$key))
p2e <- ggplot(d3, aes(fluxL_umolm2sec, key, fill = method, colour = method)) +
  geom_density_ridges(jittered_points = TRUE, position = position_points_jitter(height = 0.25, yoffset = -0.18), point_size = 0.25, point_alpha = 0.35,
                      alpha = 0.5, scale = 1.1, rel_min_height = 0.005, linewidth = 0.25, bandwidth = 0.18) +
  facet_wrap(~ array, scales = "free_y", nrow = 1) + scale_y_discrete(labels = function(k) sub(".* ", "", k)) +
  scale_fill_manual(values = pal, guide = "none") + scale_colour_manual(values = pal, guide = "none") +
  coord_cartesian(xlim = c(0, 7)) + labs(x = flux_lab, y = "Chamber or unit")
row3 <- (p2c | p2d) + plot_layout(widths = c(1.9, 1))
save_fig(p2a / p2b / row3 / p2e + plot_layout(heights = c(3.2, 0.6, 1.5, 1.6)) + tags_afm(),
         "Fig2_fluxes_budgets", 190, 255)
