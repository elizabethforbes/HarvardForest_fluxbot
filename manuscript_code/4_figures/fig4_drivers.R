# Fig. 4. Responses to meteorological drivers over stand-hours in which both systems reported (as deployed).
#  (a) diel cycle of each system: hourly means over days with 95% confidence intervals across days
#  (b) temperature response of each system: exponential fits; bands = 95% intervals from resampling
#      chambers within stands; dashed line = the other system's fit
source("R/setup.R")
source("R/fig_style.R")
suppressPackageStartupMessages({ library(ggplot2); library(patchwork); library(mgcv) })
pal <- pal_sys
d <- load_chamber_hours("deployed") %>%
  mutate(stand_label = factor(if_else(stand == "healthy", "Stand 1", "Stand 2")),
         method_label = factor(lab_sys[as.character(method)], levels = lab_sys))
num <- get_number
set.seed(20260930)

# ---- (a) diel cycle by system ------------------------------------------------------------------------
# hourly means over days with 95% confidence intervals across days (2_analysis/03_main_analyses.R)
common <- d %>% distinct(stand, hour_of_obs, method) %>% count(stand, hour_of_obs) %>% filter(n == 2)
diel <- read.csv(file.path(out_dir, "diel_common_window.csv"))
pa <- ggplot(diel, aes(hour, mean, colour = method, fill = method)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = a_band, colour = NA) +
  geom_line(linewidth = lw_main) + geom_point(size = pt_mean) +
  scale_colour_manual(values = pal, labels = lab_sys, name = NULL) + scale_fill_manual(values = pal, labels = lab_sys, name = NULL) +
  scale_x_continuous(breaks = seq(0, 24, 4)) + labs(x = "Hour of day (EDT)", y = flux_lab) +
  theme(legend.position = c(0.25, 0.92))

# ---- (b) temperature response by system ----------------------------------------------------------
dq <- d %>% semi_join(common, by = c("stand", "hour_of_obs")) %>% filter(!is.na(s10t), fluxL_umolm2sec > 0)
tseq <- seq(min(dq$s10t), max(dq$s10t), length.out = 100)
# exponential fit for each system; band = 95% interval from resampling chambers within stands
# (the chamber, not the closure, is the unit of replication; 200 draws). The other system's fit is
# drawn in grey for comparison.
fit_exp <- function(x) { m <- nls(fluxL_umolm2sec ~ a * exp(b * s10t), data = x, start = list(a = 1, b = 0.1)); coef(m) }
pred <- bind_rows(lapply(split(dq, dq$method), function(x) {
  p <- fit_exp(x)
  ids <- x %>% distinct(stand, id)
  bt <- replicate(200, { bi <- ids %>% group_by(stand) %>% slice_sample(prop = 1, replace = TRUE) %>% ungroup() %>% mutate(bid = row_number())
    pb <- fit_exp(bi %>% inner_join(x, by = c("stand", "id"), relationship = "many-to-many")); pb["a"] * exp(pb["b"] * tseq) })
  data.frame(method = x$method[1], s10t = tseq, f = p["a"] * exp(p["b"] * tseq),
             lo = apply(bt, 1, quantile, 0.025), hi = apply(bt, 1, quantile, 0.975))
})) %>% mutate(method_label = factor(lab_sys[as.character(method)], levels = lab_sys))
other <- pred %>% mutate(method_label = factor(lab_sys[if_else(method == "fluxbot", "autochamber", "fluxbot")], levels = lab_sys))
qlab <- data.frame(method_label = factor(lab_sys, levels = lab_sys),
                   lab = c(sprintf("Q[10] == %.2f", num("q10_autochamber_q10")), sprintf("Q[10] == %.2f", num("q10_fluxbot_q10"))))
pb <- ggplot(dq, aes(s10t, fluxL_umolm2sec)) +
  geom_point(aes(colour = method), size = pt_dense, alpha = a_dense, stroke = 0) +
  geom_ribbon(data = pred, aes(y = f, ymin = lo, ymax = hi), alpha = 0.3, fill = col_fit) +
  geom_line(data = pred, aes(y = f), linewidth = 0.6) +
  geom_line(data = other, aes(y = f), linewidth = 0.5, colour = "white") +
  geom_line(data = other, aes(y = f), linewidth = 0.4, colour = "black", linetype = "22") +
  geom_text(data = qlab, aes(x = -Inf, y = Inf, label = lab), parse = TRUE, hjust = -0.05, vjust = 1.3, size = 2.5) +
  facet_wrap(~method_label) + scale_colour_manual(values = pal, guide = "none") +
  labs(x = "Soil temperature at 10 cm, HF001 (\u00b0C)", y = flux_lab)
fig <- (pa | pb) + plot_layout(widths = c(1, 1.7)) + tags_afm()
save_afm(fig, "Fig4_drivers", 190, 85)
