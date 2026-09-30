# Agreement metrics appropriate to a comparison of two arrays on independent collars.
#  1. Metric panel (r, R2, OLS and SMA/Deming regression, bias, RMSE, unbiased RMSE,
#     MAE, Bland-Altman limits, CCC and its components) at hourly, 3-h and daily scales.
#  2. Spatial-null benchmark: disagreement between two disjoint subsets of the SAME
#     system (autochamber vs autochamber, Fluxbot vs Fluxbot) compared with subsets of
#     equal size from DIFFERENT systems. If cross-system disagreement falls within the
#     within-system distribution, it is explained by spatial sampling of collars.
#  3. Q10 difference with chamber-level random slopes, and chamber-level Q10 spread.
#  4. Diel shape on a relative scale; leave-one-chamber-out influence on the offset;
#     sensitivity of array agreement to the minimum-chamber threshold.

source("afm_revision/00_prep.R")
suppressPackageStartupMessages({ library(zoo); library(epiR); library(lme4) })
set.seed(20260930)

d <- readRDS(file.path(out_dir, "dataset_main.rds"))

metrics <- function(x, y) {  # x = reference (autochamber), y = Fluxbot
  ok <- is.finite(x) & is.finite(y); x <- x[ok]; y <- y[ok]
  r <- cor(x, y); dif <- y - x
  sma_b <- sign(r) * sd(y) / sd(x)
  dem_b <- { sxx <- var(x); syy <- var(y); sxy <- cov(x, y)
             (syy - sxx + sqrt((syy - sxx)^2 + 4 * sxy^2)) / (2 * sxy) }
  cc <- epi.ccc(x, y)
  data.frame(n = length(x), mean_ref = mean(x), mean_fb = mean(y), r = r, r2 = r^2,
             ols_slope = unname(coef(lm(y ~ x))[2]), ols_int = unname(coef(lm(y ~ x))[1]),
             sma_slope = sma_b, sma_int = mean(y) - sma_b * mean(x),
             deming_slope = dem_b, deming_int = mean(y) - dem_b * mean(x),
             bias = mean(dif), bias_pct = 100 * mean(dif) / mean(x),
             rmse = sqrt(mean(dif^2)), nrmse_pct = 100 * sqrt(mean(dif^2)) / mean(x),
             ubrmse = sd(dif), mae = mean(abs(dif)),
             loa_lo = mean(dif) - 1.96 * sd(dif), loa_hi = mean(dif) + 1.96 * sd(dif),
             ccc = cc$rho.c$est, cb = cc$C.b,
             # agreement of dynamics once each series is scaled by its own mean
             rel_rmse_pct = 100 * sqrt(mean((y / mean(y) - x / mean(x))^2)),
             ratio_cv = sd(y / x) / mean(y / x))
}

# ---- 1. array means at several time scales ------------------------------------------------
array_series <- function(d, k = 5) {
  hrs <- matched_hours(d, k)
  d %>% filter(hour_of_obs %in% hrs) %>%
    group_by(hour_of_obs, stand, method) %>% summarise(f = mean(fluxL_umolm2sec), .groups = "drop") %>%
    group_by(hour_of_obs, method) %>% summarise(f = mean(f), .groups = "drop") %>%
    pivot_wider(names_from = method, values_from = f)
}
hr <- array_series(d)
grid <- data.frame(hour_of_obs = seq(min(hr$hour_of_obs), max(hr$hour_of_obs), by = "hour")) %>%
  left_join(hr, by = "hour_of_obs") %>%
  mutate(ac3 = rollapply(autochamber, 3, mean, fill = NA, align = "right"),
         fb3 = rollapply(fluxbot, 3, mean, fill = NA, align = "right"))
# daily: days with >= 12 matched hours
dy <- hr %>% mutate(day = as.Date(hour_of_obs)) %>% group_by(day) %>%
  filter(n() >= 12) %>% summarise(ac = mean(autochamber), fb = mean(fluxbot))
panel <- bind_rows(
  cbind(scale = "hourly", metrics(hr$autochamber, hr$fluxbot)),
  cbind(scale = "3-h rolling", metrics(grid$ac3, grid$fb3)),
  cbind(scale = "daily", metrics(dy$ac, dy$fb)))
write.csv(panel, file.path(out_dir, "agreement_metric_panel.csv"), row.names = FALSE)
print(panel %>% mutate(across(where(is.numeric), ~ round(., 3))))

# threshold sensitivity (minimum chambers per system x stand)
thr <- bind_rows(lapply(1:6, function(k) {
  s <- array_series(d, k); cbind(k = k, metrics(s$autochamber, s$fluxbot)) }))
write.csv(thr, file.path(out_dir, "agreement_threshold_sensitivity.csv"), row.names = FALSE)
print(thr %>% select(k, n, r, bias, rmse, ccc) %>% mutate(across(where(is.numeric), ~ round(., 3))))

# ---- 2. spatial-null benchmark --------------------------------------------------------------
# Subsets of m chambers per stand. For each draw build hourly array-mean series (mean of the
# two stand means, requiring >= 2 of the m chambers reporting in each stand) for two
# DISJOINT subsets, then compute agreement metrics on hourly and daily means.
m <- 3
ids <- d %>% distinct(method, stand, id) %>% mutate(id = as.character(id))
chour <- d %>% mutate(id = as.character(id)) %>% select(method, stand, id, hour_of_obs, f = fluxL_umolm2sec)
subset_series <- function(sel) {  # sel: data.frame(stand, id)
  chour %>% semi_join(sel, by = c("stand", "id")) %>%
    group_by(stand, hour_of_obs) %>% filter(n() >= 2) %>% summarise(f = mean(f), .groups = "drop") %>%
    group_by(hour_of_obs) %>% filter(n() == 2) %>% summarise(f = mean(f), .groups = "drop")
}
draw <- function(sys_a, sys_b) {
  pick <- function(sys, exclude = NULL) bind_rows(lapply(c("healthy", "unhealthy"), function(st) {
    pool <- ids %>% filter(method == sys, stand == st, !(id %in% exclude))
    pool[sample(nrow(pool), m), c("stand", "id")] }))
  a <- pick(sys_a); b <- pick(sys_b, exclude = if (sys_a == sys_b) a$id else NULL)
  s <- inner_join(subset_series(a), subset_series(b), by = "hour_of_obs", suffix = c("_a", "_b"))
  if (nrow(s) < 50) return(NULL)
  sd_ <- s %>% mutate(day = as.Date(hour_of_obs)) %>% group_by(day) %>% filter(n() >= 12) %>%
    summarise(a = mean(f_a), b = mean(f_b))
  h <- metrics(s$f_a, s$f_b); dd <- metrics(sd_$a, sd_$b)
  data.frame(abs_bias = abs(h$bias) / mean(c(h$mean_ref, h$mean_fb)), r_hourly = h$r, ccc_hourly = h$ccc,
             nrmse_hourly = h$nrmse_pct, r_daily = dd$r, ccc_daily = dd$ccc, nrmse_daily = dd$nrmse_pct)
}
N <- 400
null <- bind_rows(
  bind_rows(replicate(N, draw("autochamber", "autochamber"), simplify = FALSE)) %>% mutate(pair = "autochamber vs autochamber"),
  bind_rows(replicate(N, draw("fluxbot", "fluxbot"), simplify = FALSE)) %>% mutate(pair = "Fluxbot vs Fluxbot"),
  bind_rows(replicate(N, draw("autochamber", "fluxbot"), simplify = FALSE)) %>% mutate(pair = "autochamber vs Fluxbot"))
saveRDS(null, file.path(out_dir, "spatial_null.rds"))
null_sum <- null %>% group_by(pair) %>% summarise(across(everything(), list(med = median,
  lo = ~ quantile(., 0.1), hi = ~ quantile(., 0.9))), n = n())
write.csv(null_sum, file.path(out_dir, "spatial_null_summary.csv"), row.names = FALSE)
print(as.data.frame(null_sum %>% select(pair, n, starts_with("abs_bias"), starts_with("r_hourly"),
                                        starts_with("ccc_daily"), starts_with("nrmse_daily"))), digits = 3)
# where does each cross-system draw fall relative to the pooled within-system distribution?
within <- null %>% filter(pair != "autochamber vs Fluxbot")
cross <- null %>% filter(pair == "autochamber vs Fluxbot")
for (v in c("abs_bias", "r_hourly", "ccc_hourly", "nrmse_hourly", "r_daily", "ccc_daily", "nrmse_daily")) {
  record(paste0("null_within_median_", v), median(within[[v]]), "spatial_null", paste0("m = ", m, " chambers per stand"))
  record(paste0("null_cross_median_", v), median(cross[[v]]), "spatial_null")
  record(paste0("null_within_ac_median_", v), median(within[[v]][within$pair == "autochamber vs autochamber"]), "spatial_null")
  record(paste0("null_within_fb_median_", v), median(within[[v]][within$pair == "Fluxbot vs Fluxbot"]), "spatial_null")
  # probability a cross-system draw is worse than a random within-system draw
  worse <- if (grepl("^r_|^ccc", v)) mean(outer(cross[[v]], within[[v]], "<")) else mean(outer(cross[[v]], within[[v]], ">"))
  record(paste0("null_p_cross_worse_", v), worse, "spatial_null", "P(cross-system draw worse than within-system draw); 0.5 = indistinguishable")
}

# ---- 3. Q10 with chamber random slopes ---------------------------------------------------------
common <- d %>% distinct(stand, hour_of_obs, method) %>% count(stand, hour_of_obs) %>% filter(n == 2)
dq <- d %>% semi_join(common, by = c("stand", "hour_of_obs")) %>% filter(!is.na(s10t), fluxL_umolm2sec > 0.5) %>%
  mutate(T10 = s10t - 15)
m_int <- lmer(log(fluxL_umolm2sec) ~ T10 * method + (1 | id), data = dq)
m_slp <- lmer(log(fluxL_umolm2sec) ~ T10 * method + (1 + T10 | id), data = dq,
              control = lmerControl(optimizer = "bobyqa"))
for (nm in c("int", "slp")) {
  mm <- get(paste0("m_", nm)); co <- summary(mm)$coefficients
  b_ac <- co["T10", 1]; b_x <- co["T10:methodfluxbot", 1]
  record(paste0("q10lmm_", nm, "_autochamber"), exp(10 * b_ac), "Q10_random_slopes", "flux > 0.5, common stand-hours")
  record(paste0("q10lmm_", nm, "_fluxbot"), exp(10 * (b_ac + b_x)), "Q10_random_slopes")
  record(paste0("q10lmm_", nm, "_interaction_t"), co["T10:methodfluxbot", "t value"], "Q10_random_slopes")
  record(paste0("q10lmm_", nm, "_interaction_p"), 2 * pnorm(-abs(co["T10:methodfluxbot", "t value"])), "Q10_random_slopes")
  ci <- co["T10:methodfluxbot", 1] + c(-1.96, 1.96) * co["T10:methodfluxbot", 2]
  record(paste0("q10lmm_", nm, "_ratio_lo"), exp(10 * ci[1]), "Q10_random_slopes", "Fluxbot/autochamber Q10 ratio")
  record(paste0("q10lmm_", nm, "_ratio_hi"), exp(10 * ci[2]), "Q10_random_slopes")
}
record("q10lmm_slp_sd_chamber_slope_as_q10factor", exp(10 * attr(VarCorr(m_slp)$id, "stddev")["T10"]), "Q10_random_slopes",
       "between-chamber SD of slope expressed as a Q10 multiplier")
# per-chamber Q10 (log-linear fit per chamber, >= 100 obs)
cq <- dq %>% group_by(method, stand, id) %>% filter(n() >= 100) %>%
  summarise(q10 = exp(10 * coef(lm(log(fluxL_umolm2sec) ~ s10t))[2]), n = n(), .groups = "drop")
write.csv(cq, file.path(out_dir, "q10_by_chamber.csv"), row.names = FALSE)
for (mt in c("autochamber", "fluxbot")) {
  x <- cq$q10[cq$method == mt]
  record(paste0("q10_chamber_median_", mt), median(x), "Q10_random_slopes")
  record(paste0("q10_chamber_min_", mt), min(x), "Q10_random_slopes")
  record(paste0("q10_chamber_max_", mt), max(x), "Q10_random_slopes")
}
record("q10_chamber_wilcox_p", wilcox.test(q10 ~ method, data = cq)$p.value, "Q10_random_slopes")
print(cq %>% arrange(method, q10), n = 40)

# ---- 4a. diel shape on a relative scale ----------------------------------------------------------
diel <- read.csv(file.path(out_dir, "diel_common_window.csv")) %>% group_by(method) %>%
  mutate(rel = mean / mean(mean)) %>% ungroup()
w <- diel %>% select(method, hour, rel) %>% pivot_wider(names_from = method, values_from = rel)
record("diel_rel_amplitude_autochamber", 100 * diff(range(w$autochamber)), "diel_relative", "% of daily mean")
record("diel_rel_amplitude_fluxbot", 100 * diff(range(w$fluxbot)), "diel_relative")
record("diel_rel_r", cor(w$autochamber, w$fluxbot), "diel_relative")
record("diel_rel_max_abs_diff_pct", 100 * max(abs(w$fluxbot - w$autochamber)), "diel_relative")
lag_r <- sapply(-3:3, function(L) cor(w$autochamber, w$fluxbot[((seq_len(24) - 1 + L) %% 24) + 1]))
record("diel_rel_best_lag_h", (-3:3)[which.max(lag_r)], "diel_relative", "Fluxbot lag maximizing correlation")
record("diel_rel_r_best_lag", max(lag_r), "diel_relative")

# ---- 4b. leave-one-chamber-out: influence on the array offset ------------------------------------
cm <- d %>% semi_join(common, by = c("stand", "hour_of_obs")) %>% group_by(method, stand, id) %>%
  summarise(f = mean(fluxL_umolm2sec), .groups = "drop")
offset <- function(x) { s <- x %>% group_by(method, stand) %>% summarise(f = mean(f), .groups = "drop") %>%
  group_by(method) %>% summarise(f = mean(f)); s$f[s$method == "fluxbot"] - s$f[s$method == "autochamber"] }
loo <- bind_rows(lapply(seq_len(nrow(cm)), function(i) data.frame(dropped = cm$id[i], method = cm$method[i],
  stand = cm$stand[i], chamber_mean = cm$f[i], offset = offset(cm[-i, ]))))
write.csv(loo, file.path(out_dir, "offset_leave_one_out.csv"), row.names = FALSE)
record("offset_chamber_means", offset(cm), "loo")
record("offset_loo_min", min(loo$offset), "loo"); record("offset_loo_max", max(loo$offset), "loo")
print(cm %>% arrange(stand, method, f), n = 40)

print(write_numbers("numbers_agreement.csv"), row.names = FALSE)

# ---- 5. what drives the Fluxbot/autochamber ratio? ------------------------------------------
hrs3 <- matched_hours(d, 3)
rs <- d %>% filter(hour_of_obs %in% hrs3) %>% group_by(hour_of_obs, stand, method) %>%
  summarise(f = mean(fluxL_umolm2sec), .groups = "drop") %>% pivot_wider(names_from = method, values_from = f) %>%
  group_by(hour_of_obs) %>% summarise(ac = mean(autochamber), fb = mean(fluxbot))
metr <- load_met() %>% mutate(hour_of_obs = floor_date(with_tz(Time, "America/New_York"), "hour")) %>%
  group_by(hour_of_obs) %>% summarise(airt = mean(airt), slrr = mean(slrr), s10t = mean(s10t))
rx <- rs %>% inner_join(metr, by = "hour_of_obs") %>% mutate(lr = log(fb / ac), grad = airt - s10t)
fr <- lm(lr ~ grad + slrr, data = rx); sr <- summary(fr)
record("ratio_grad_pct_per_C", 100 * (exp(coef(fr)["grad"]) - 1), "ratio_drivers", "% change in Fluxbot/autochamber ratio per degC air minus soil")
record("ratio_grad_se_pct", 100 * sr$coefficients["grad", 2], "ratio_drivers")
record("ratio_slrr_pct_per_100Wm2", 100 * (exp(100 * coef(fr)["slrr"]) - 1), "ratio_drivers", "% change per 100 W m-2")
record("ratio_model_r2", sr$r.squared, "ratio_drivers")
record("ratio_model_n", nrow(rx), "ratio_drivers", "array-hours, >= 3 chambers per system x stand")
record("ratio_grad_range_lo", min(rx$grad), "ratio_drivers"); record("ratio_grad_range_hi", max(rx$grad), "ratio_drivers")
write_numbers("numbers_agreement.csv")

# ---- Fig S6: spatial-null benchmark ----------------------------------------------------------
suppressPackageStartupMessages({ library(ggplot2); library(patchwork) })
pal3 <- c("autochamber vs autochamber" = "#3B8F63", "Fluxbot vs Fluxbot" = "#8C8C8C", "autochamber vs Fluxbot" = "#2F5D9E")
nl <- null %>% mutate(pair = factor(pair, levels = names(pal3)), abs_bias = 100 * abs_bias)
pan <- function(v, lab) ggplot(nl, aes(pair, .data[[v]], fill = pair)) +
  geom_violin(colour = NA, alpha = 0.6) + geom_boxplot(width = 0.15, outliers = FALSE, fill = "white", linewidth = 0.3) +
  scale_fill_manual(values = pal3, guide = "none") + labs(x = NULL, y = lab) +
  scale_x_discrete(labels = c("AC vs AC", "FB vs FB", "AC vs FB")) + theme_classic(base_size = 8)
pS6 <- pan("abs_bias", "Offset between subsets\n(% of mean flux)") + pan("nrmse_daily", "Daily RMSE\n(% of mean flux)") +
  pan("r_hourly", "Hourly correlation (r)") + pan("ccc_daily", "Daily CCC") + plot_layout(ncol = 4) +
  plot_annotation(tag_levels = "a")
fd <- file.path(out_dir, "figures"); dir.create(fd, showWarnings = FALSE)
ggsave(file.path(fd, "FigS6_spatial_null.pdf"), pS6, width = 190, height = 60, units = "mm", device = cairo_pdf)
ggsave(file.path(fd, "FigS6_spatial_null.png"), pS6, width = 190, height = 60, units = "mm", dpi = 300, device = ragg::agg_png)
