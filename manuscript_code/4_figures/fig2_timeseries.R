# Fig. 2. Time series of individual chamber fluxes by system and stand, soil temperature and rain, and
# gap-filled cumulative budgets (as deployed, with the RH-screened Fluxbot series).
source("R/setup.R")
source("R/fig_style.R")
suppressPackageStartupMessages({ library(ggplot2); library(patchwork); library(mgcv); library(zoo) })
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
# (a) individual chamber fluxes (points) by system (rows) and stand (columns); thick line = this
#     system's 24-h centred rolling mean of stand means; thin dashed line = the other system's. Rolling
#     means use only stand-hours in which both systems had >= 3 units, so the two lines compare like
#     with like (using each system's own hours changes their correlation by < 0.01).
dd2 <- d %>% filter(hour_of_obs >= p0, hour_of_obs < p1)
roll <- dd2 %>% group_by(stand_label, method, hour_of_obs) %>% summarise(f = mean(fluxL_umolm2sec), n = n(), .groups = "drop") %>% filter(n >= 3) %>%
  group_by(stand_label, hour_of_obs) %>% filter(n() == 2) %>%
  group_by(stand_label, method) %>% complete(hour_of_obs = seq(p0, p1 - 3600, by = "hour")) %>% arrange(hour_of_obs, .by_group = TRUE) %>%
  mutate(r = rollapply(f, 24, function(x) if (sum(!is.na(x)) >= 12) mean(x, na.rm = TRUE) else NA, fill = NA, align = "center")) %>% ungroup()
own <- roll %>% mutate(method_label = factor(lab_sys[as.character(method)], levels = lab_sys))
other <- roll %>% mutate(method_label = factor(lab_sys[if_else(method == "fluxbot", "autochamber", "fluxbot")], levels = lab_sys))
xs2 <- scale_x_datetime(limits = c(p0, p1), date_labels = "%d %b", date_breaks = "1 week", expand = c(0.01, 0))
p2a <- ggplot(dd2, aes(hour_of_obs, fluxL_umolm2sec)) +
  geom_point(aes(colour = method), size = pt_dense, alpha = a_dense, stroke = 0) +
  geom_line(data = other, aes(y = r), colour = "grey20", linewidth = lw_thin, linetype = "22", na.rm = TRUE) +
  geom_line(data = own, aes(y = r), colour = col_fit, linewidth = 0.8, na.rm = TRUE) +
  facet_grid(method_label ~ stand_label) + scale_colour_manual(values = pal, guide = "none") +
  coord_cartesian(ylim = c(0, 7)) + xs2 + labs(x = NULL, y = flux_lab)
metp <- met %>% mutate(hr = floor_date(with_tz(Time, "America/New_York"), "hour")) %>% group_by(hr) %>%
  summarise(s10t = mean(s10t), airt = mean(airt), prec = sum(prec), .groups = "drop") %>% filter(hr >= p0, hr < p1)
sc <- max(metp$prec, na.rm = TRUE) / max(metp$airt, na.rm = TRUE)
p2b <- ggplot(metp, aes(hr)) + geom_col(aes(y = prec / sc), fill = col_wet, alpha = a_mean, width = 3600) +
  geom_line(aes(y = airt), linewidth = 0.4) +
  scale_y_continuous(name = expression(Air ~ T ~ (degree * C)), sec.axis = sec_axis(~ . * sc, name = expression(Rain ~ (mm ~ h^-1)))) +
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
  theme(legend.position = c(0.22, 0.78), legend.key.height = unit(3, "mm"))
save_fig(p2a / p2b / p2c + plot_layout(heights = c(3.4, 0.7, 1.2)) + tags_afm(),
         "Fig2_timeseries", 190, 210)
