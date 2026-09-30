# AFM revision analyses (tasks A1-A4, A6, A8 of AFM_REVISION_HANDOFF.md).
# Writes outputs/afm_revision/numbers_for_text.csv (every number quoted in the
# revised text), sensitivity_table.csv, and intermediate tables for figures.
#
# Run from manuscript_code/:  Rscript afm_revision/02_afm_analyses.R

source("afm_revision/00_prep.R")
suppressPackageStartupMessages({
  library(mgcv); library(lme4); library(zoo); library(epiR); library(ineq)
})
set.seed(20260930)

# ================================================================================
# Core statistics, as a function of the analysis dataset, so they can be re-run
# under each QC / flux-definition / timestamp / pressure alternative (A6-A8).
# ================================================================================

# hours used for Figs 4-5: both systems with >= 5 chambers reporting in both stands
fig5_hours <- function(d) matched_hours(d, k = 5)

# stand-hours in which both systems reported (common window for Q10, diel, effort)
common_stand_hours <- function(d) {
  d %>% distinct(stand, hour_of_obs, method) %>% count(stand, hour_of_obs) %>%
    filter(n == 2) %>% select(stand, hour_of_obs)
}

fit_gam <- function(d) {
  gam(fluxL_umolm2sec ~ stand + method + s(hour, bs = "cc", k = 12) + s(s10t) + s(id, bs = "re"),
      data = d, method = "REML", knots = list(hour = c(-0.5, 23.5)))
}

# Array-wide hourly means (mean of the two stand means) in hours where both
# systems had >= k chambers in both stands; 3-h rolling means are computed on a
# complete hourly grid so that a window never spans a data gap.
# (The .qmd's filter let through hours in which one system x stand combination
# was missing entirely, because all() of an empty vector is TRUE, and its
# rolling mean ran across gaps.)
array_agreement <- function(d, k = 5, window = 3) {
  hrs <- matched_hours(d, k)
  pv <- d %>% filter(hour_of_obs %in% hrs) %>%
    group_by(hour_of_obs, stand, method) %>% summarise(f = mean(fluxL_umolm2sec), .groups = "drop") %>%
    pivot_wider(names_from = method, values_from = f) %>%
    group_by(hour_of_obs) %>%
    summarise(ac = mean(autochamber), fb = mean(fluxbot), .groups = "drop")
  grid <- data.frame(hour_of_obs = seq(min(pv$hour_of_obs), max(pv$hour_of_obs), by = "hour"))
  pv <- grid %>% left_join(pv, by = "hour_of_obs") %>% arrange(hour_of_obs) %>%
    mutate(ac3 = rollapply(ac, window, mean, fill = NA, align = "right"),
           fb3 = rollapply(fb, window, mean, fill = NA, align = "right")) %>%
    filter(!is.na(ac3), !is.na(fb3))
  fit <- lm(fb3 ~ ac3, data = pv)
  cc <- epi.ccc(pv$ac3, pv$fb3)
  # circular block bootstrap (blocks of 24 consecutive values) for the CI of the
  # mean paired difference
  diffs <- pv$fb3 - pv$ac3; nd <- length(diffs); nb <- ceiling(nd / 24)
  boot <- replicate(2000, {
    starts <- sample(seq_len(nd), nb, replace = TRUE)
    idx <- unlist(lapply(starts, function(s) ((s - 1 + 0:23) %% nd) + 1))[seq_len(nd)]
    mean(diffs[idx])
  })
  list(data = pv, n = nrow(pv), intercept = unname(coef(fit)[1]), slope = unname(coef(fit)[2]),
       slope_ci = unname(confint(fit)[2, ]), intercept_ci = unname(confint(fit)[1, ]),
       r2 = summary(fit)$r.squared, r = cor(pv$ac3, pv$fb3),
       ccc = cc$rho.c$est, ccc_lo = cc$rho.c$lower, ccc_hi = cc$rho.c$upper, cb = cc$C.b,
       bias = mean(diffs), bias_ci = unname(quantile(boot, c(0.025, 0.975))),
       mean_ac = mean(pv$ac3), mean_fb = mean(pv$fb3))
}

q10_nls <- function(dd) {
  m <- nls(fluxL_umolm2sec ~ a * exp(b * s10t), data = dd, start = list(a = 1, b = 0.1))
  b <- unname(coef(m)["b"]); se <- sqrt(vcov(m)["b", "b"])
  r2 <- 1 - sum(residuals(m)^2) / sum((dd$fluxL_umolm2sec - mean(dd$fluxL_umolm2sec))^2)
  c(q10 = exp(10 * b), lo = exp(10 * (b - 1.96 * se)), hi = exp(10 * (b + 1.96 * se)),
    r2 = r2, tmin = min(dd$s10t), tmax = max(dd$s10t), n = nrow(dd))
}
# log-linear mixed model with chamber random intercepts: log F = a + b T + u_chamber
q10_lmm <- function(dd) {
  m <- lmer(log(fluxL_umolm2sec) ~ s10t + (1 | id), data = dd)
  b <- fixef(m)["s10t"]; se <- sqrt(vcov(m)["s10t", "s10t"])
  c(q10 = unname(exp(10 * b)), lo = unname(exp(10 * (b - 1.96 * se))),
    hi = unname(exp(10 * (b + 1.96 * se))))
}
q10_common <- function(d, min_flux = 0) {
  dd <- d %>% semi_join(common_stand_hours(d), by = c("stand", "hour_of_obs")) %>%
    filter(!is.na(s10t), fluxL_umolm2sec > min_flux)
  lapply(split(dd, dd$method), function(x)
    list(nls = q10_nls(x), lmm = q10_lmm(x %>% filter(fluxL_umolm2sec > 0))))
}

gini_arrays <- function(d) {
  d %>% group_by(hour_of_obs) %>%
    filter(sum(method == "autochamber") >= 12 & sum(method == "fluxbot") >= 12) %>% ungroup() %>%
    group_by(method, id) %>% summarise(mf = mean(fluxL_umolm2sec), .groups = "drop") %>%
    group_by(method) %>% summarise(gini = Gini(mf), .groups = "drop") %>%
    { setNames(.$gini, .$method) }
}

key_stats <- function(d, label) {
  g <- fit_gam(d); s <- summary(g)
  est <- s$p.table["methodfluxbot", 1]; se <- s$p.table["methodfluxbot", 2]
  ag <- array_agreement(d)
  q <- q10_common(d)
  gi <- gini_arrays(d)
  data.frame(
    scenario = label, n = nrow(d),
    n_fluxbot = sum(d$method == "fluxbot"), n_autochamber_hours = sum(d$method == "autochamber"),
    max_flux = max(d$fluxL_umolm2sec),
    gam_intercept = s$p.table["(Intercept)", 1], gam_method = est, gam_method_se = se,
    gam_method_lo = est - 1.96 * se, gam_method_hi = est + 1.96 * se,
    gam_method_p = s$p.table["methodfluxbot", 4], gam_r2 = s$r.sq,
    ccc = ag$ccc, ccc_lo = ag$ccc_lo, ccc_hi = ag$ccc_hi, r = ag$r, cb = ag$cb,
    slope = ag$slope, intercept = ag$intercept, r2 = ag$r2,
    paired_bias = ag$bias, paired_bias_lo = ag$bias_ci[1], paired_bias_hi = ag$bias_ci[2],
    q10_ac = q$autochamber$nls["q10"], q10_ac_lo = q$autochamber$nls["lo"], q10_ac_hi = q$autochamber$nls["hi"],
    q10_fb = q$fluxbot$nls["q10"], q10_fb_lo = q$fluxbot$nls["lo"], q10_fb_hi = q$fluxbot$nls["hi"],
    q10lmm_ac = q$autochamber$lmm["q10"], q10lmm_fb = q$fluxbot$lmm["q10"],
    gini_ac = gi["autochamber"], gini_fb = gi["fluxbot"], row.names = NULL)
}

# ================================================================================
# Main dataset: manuscript QC (negatives removed, pooled 1.5 x IQR fences),
# linear-slope fluxes, HF001 joined in EST.
# ================================================================================
d <- build_dataset()
saveRDS(d, file.path(out_dir, "dataset_main.rds"))

record("n_obs", nrow(d), "data")
record("n_fluxbot_obs", sum(d$method == "fluxbot"), "data")
record("n_autochamber_chamber_hours", sum(d$method == "autochamber"), "data")
record("n_fluxbot_units_with_data", n_distinct(d$id[d$method == "fluxbot"]), "data")
for (st in c("healthy", "unhealthy")) record(paste0("n_fluxbot_units_", st),
  n_distinct(d$id[d$method == "fluxbot" & d$stand == st]), "data")
record("fluxbot_last_obs_doy", max(yday(d$hour_of_obs[d$method == "fluxbot"])), "data")
record("autochamber_last_obs_doy", max(yday(d$hour_of_obs[d$method == "autochamber"])), "data")

# ---- A1/A2: GAM, method offset, CI, TOST ----------------------------------------
g <- fit_gam(d); sg <- summary(g)
saveRDS(g, file.path(out_dir, "gam_re.rds"))
capture.output(sg, file = file.path(out_dir, "gam_re_summary.txt"))
est <- sg$p.table["methodfluxbot", 1]; se <- sg$p.table["methodfluxbot", 2]
int <- sg$p.table["(Intercept)", 1]
record("gam_intercept", int, "A1", "autochamber, stand 1 reference")
record("gam_stand2", sg$p.table["standunhealthy", 1], "A1")
record("gam_stand2_se", sg$p.table["standunhealthy", 2], "A1")
record("gam_stand2_p", sg$p.table["standunhealthy", 4], "A1")
record("gam_method", est, "A1")
record("gam_method_se", se, "A1")
record("gam_method_p", sg$p.table["methodfluxbot", 4], "A1")
record("gam_method_ci95_lo", est - 1.96 * se, "A1")
record("gam_method_ci95_hi", est + 1.96 * se, "A1")
record("gam_method_ci90_lo", est - 1.645 * se, "A2")
record("gam_method_ci90_hi", est + 1.645 * se, "A2")
record("gam_method_pct_of_intercept", 100 * est / int, "A1")
record("gam_r2adj", sg$r.sq, "A1")
record("gam_dev_expl", sg$dev.expl, "A1")
record("gam_edf_hour", sg$s.table["s(hour)", "edf"], "A1")
record("gam_edf_s10t", sg$s.table["s(s10t)", "edf"], "A1")
record("gam_p_hour", sg$s.table["s(hour)", "p-value"], "A1")
record("gam_p_s10t", sg$s.table["s(s10t)", "p-value"], "A1")
# old random-effect specification, for the record
g_fs <- gam(fluxL_umolm2sec ~ stand + s(hour) + method + s(id, bs = "fs") + s(s10t),
            data = d, method = "REML")
record("gam_fs_method", summary(g_fs)$p.table["methodfluxbot", 1], "A1", "bs='fs' as submitted, EST join")
record("gam_fs_method_se", summary(g_fs)$p.table["methodfluxbot", 2], "A1")

vc <- gam.vcomp(g, rescale = FALSE)
sd_chamber <- unname(vc["s(id)", "std.dev"]); sd_resid <- sqrt(g$sig2)
record("sd_between_chamber_gam", sd_chamber, "A1", "random-intercept SD, both systems")
record("sd_residual_gam", sd_resid, "A1")
record("method_offset_over_sd_chamber", abs(est) / sd_chamber, "A1")

# variance components with system-specific between-chamber SD and a shared
# stand x hour term that absorbs common temporal variation
lm_v <- lmer(fluxL_umolm2sec ~ stand * method +
               (0 + dummy(method, "autochamber") | id) + (0 + dummy(method, "fluxbot") | id) +
               (1 | stand:hour_of_obs), data = d %>% mutate(hour_of_obs = factor(hour_of_obs)),
             control = lmerControl(calc.derivs = FALSE))
vcl <- as.data.frame(VarCorr(lm_v))

sd_ac <- vcl$sdcor[grepl("^id", vcl$grp) & grepl("autochamber", vcl$var1)]
sd_fb <- vcl$sdcor[grepl("^id", vcl$grp) & grepl("fluxbot", vcl$var1)]
record("sd_chamber_autochamber", sd_ac, "A1", "lmer, system-specific chamber SD")
record("sd_chamber_fluxbot", sd_fb, "A1")
record("sd_stand_hour", vcl$sdcor[grepl("^stand", vcl$grp)], "A1")
record("sd_resid_lmer", vcl$sdcor[vcl$grp == "Residual"], "A1")

# chamber-level summaries (the "1.5-4.0" medians in the text)
chamber <- d %>% group_by(method, stand, id) %>%
  summarise(median = median(fluxL_umolm2sec), mean = mean(fluxL_umolm2sec),
            q1 = quantile(fluxL_umolm2sec, .25), q3 = quantile(fluxL_umolm2sec, .75),
            n = n(), .groups = "drop")
write.csv(chamber, file.path(out_dir, "chamber_summary.csv"), row.names = FALSE)
record("chamber_median_min", min(chamber$median), "A1")
record("chamber_median_max", max(chamber$median), "A1")
record("chamber_q1_min", min(chamber$q1), "A1")
record("chamber_q3_max", max(chamber$q3), "A1")
for (m in c("autochamber", "fluxbot")) {
  x <- chamber %>% filter(method == m)
  record(paste0("chamber_mean_range_", m, "_lo"), min(x$mean), "A1")
  record(paste0("chamber_mean_range_", m, "_hi"), max(x$mean), "A1")
  # within-system between-chamber SD of chamber means (pooled within stands)
  record(paste0("sd_chamber_means_", m),
         sqrt(sum(tapply(x$mean, x$stand, function(v) sum((v - mean(v))^2))) / (nrow(x) - 2)), "A1")
}

# TOST on the GAM method effect, bounds = +/-10% and +/-20% of the autochamber mean
ac_mean <- mean(d$fluxL_umolm2sec[d$method == "autochamber"])
record("autochamber_mean_flux", ac_mean, "A2")
record("fluxbot_mean_flux", mean(d$fluxL_umolm2sec[d$method == "fluxbot"]), "A2")
for (pct in c(10, 20, 30)) {
  bnd <- pct / 100 * ac_mean
  p_tost <- max(pnorm((est + bnd) / se, lower.tail = FALSE), pnorm((est - bnd) / se))
  record(paste0("tost_bound_", pct, "pct"), bnd, "A2")
  record(paste0("tost_p_", pct, "pct"), p_tost, "A2", "equivalence if < 0.05")
}
# smallest symmetric bound for which equivalence would be declared (alpha = 0.05)
record("tost_min_equiv_bound", abs(est) + 1.645 * se, "A2")
record("tost_min_equiv_bound_pct", 100 * (abs(est) + 1.645 * se) / ac_mean, "A2")

# chamber-cluster bootstrap of the method difference (resample chambers within
# system x stand), a check on the GAM SE that makes no distributional assumptions
cm <- d %>% group_by(method, stand, id) %>% summarise(m = mean(fluxL_umolm2sec), .groups = "drop")
bt <- replicate(5000, {
  b <- cm %>% group_by(method, stand) %>% slice_sample(prop = 1, replace = TRUE) %>%
    summarise(m = mean(m), .groups = "drop") %>% group_by(method) %>% summarise(m = mean(m))
  b$m[b$method == "fluxbot"] - b$m[b$method == "autochamber"]
})
obs_diff <- with(cm %>% group_by(method, stand) %>% summarise(m = mean(m), .groups = "drop") %>%
                   group_by(method) %>% summarise(m = mean(m)), m[method == "fluxbot"] - m[method == "autochamber"])
record("chamber_boot_diff", obs_diff, "A2")
record("chamber_boot_ci95_lo", quantile(bt, 0.025), "A2")
record("chamber_boot_ci95_hi", quantile(bt, 0.975), "A2")

# ---- Array-level agreement (Fig 5), without the `autochamber > 1` filter --------
ag <- array_agreement(d)
saveRDS(ag$data, file.path(out_dir, "fig5_data.rds"))
record("fig5_n_hours", ag$n, "agreement")
record("ccc", ag$ccc, "agreement"); record("ccc_lo", ag$ccc_lo, "agreement"); record("ccc_hi", ag$ccc_hi, "agreement")
record("ccc_pearson_r", ag$r, "agreement", "precision component")
record("ccc_cb", ag$cb, "agreement", "accuracy (bias-correction) component")
record("fig5_slope", ag$slope, "agreement"); record("fig5_slope_lo", ag$slope_ci[1], "agreement")
record("fig5_slope_hi", ag$slope_ci[2], "agreement")
record("fig5_intercept", ag$intercept, "agreement"); record("fig5_intercept_lo", ag$intercept_ci[1], "agreement")
record("fig5_intercept_hi", ag$intercept_ci[2], "agreement")
record("fig5_r2", ag$r2, "agreement")
record("fig5_mean_autochamber", ag$mean_ac, "agreement")
record("fig5_mean_fluxbot", ag$mean_fb, "agreement")
record("paired_bias", ag$bias, "agreement", "fluxbot - autochamber, 3-h array means")
record("paired_bias_lo", ag$bias_ci[1], "agreement", "circular block bootstrap, 24-value blocks")
record("paired_bias_hi", ag$bias_ci[2], "agreement")
record("paired_bias_pct", 100 * ag$bias / ag$mean_ac, "agreement")
record("fig5_intercept_pct_of_mean", 100 * abs(ag$intercept) / ag$mean_ac, "agreement")
ag1 <- array_agreement(d, window = 1)
record("ccc_hourly_nosmooth", ag1$ccc, "agreement", "array-wide hourly means, no rolling mean")
record("ccc_hourly_nosmooth_n", ag1$n, "agreement")
ag3 <- array_agreement(d, k = 3)
record("ccc_k3", ag3$ccc, "agreement", ">= 3 chambers per system x stand")
record("ccc_k3_n", ag3$n, "agreement")
# per-stand agreement (stand-hour means, >= 3 chambers of each system in that stand)
for (st in c("healthy", "unhealthy")) {
  ds <- d %>% filter(stand == st)
  hs <- ds %>% count(hour_of_obs, method) %>% group_by(hour_of_obs) %>%
    filter(n() == 2, all(n >= 3)) %>% distinct(hour_of_obs) %>% pull()
  pvs <- ds %>% filter(hour_of_obs %in% hs) %>% group_by(hour_of_obs, method) %>%
    summarise(f = mean(fluxL_umolm2sec), .groups = "drop") %>% pivot_wider(names_from = method, values_from = f)
  pvs <- data.frame(hour_of_obs = seq(min(pvs$hour_of_obs), max(pvs$hour_of_obs), by = "hour")) %>%
    left_join(pvs, by = "hour_of_obs") %>%
    mutate(ac3 = rollapply(autochamber, 3, mean, fill = NA, align = "right"),
           fb3 = rollapply(fluxbot, 3, mean, fill = NA, align = "right")) %>% filter(!is.na(ac3), !is.na(fb3))
  cs <- epi.ccc(pvs$ac3, pvs$fb3)
  record(paste0("ccc_stand_", st), cs$rho.c$est, "agreement", "stand-hour 3-h means, >=3 chambers each")
  record(paste0("ccc_stand_", st, "_r"), cor(pvs$ac3, pvs$fb3), "agreement")
  record(paste0("ccc_stand_", st, "_bias"), mean(pvs$fb3 - pvs$ac3), "agreement")
  record(paste0("ccc_stand_", st, "_n"), nrow(pvs), "agreement")
}
# the submitted pipeline's CCC (hour filter as coded, autochamber > 1, gaps bridged)
record("ccc_as_submitted_logic", 0.70464, "agreement", "from 01_baseline.R fig5_ccc (NY-parsed met; met-independent)")

# temporal agreement once each system's own mean is removed (hourly anomalies)
an <- ag$data %>% mutate(a = ac3 - mean(ac3), f = fb3 - mean(fb3))
record("anomaly_r", cor(an$a, an$f), "agreement", "correlation of 3-h anomalies")

# ---- Fig 4 means ---------------------------------------------------------------
f4 <- d %>% filter(hour_of_obs %in% fig5_hours(d)) %>% group_by(method) %>%
  summarise(m = mean(fluxL_umolm2sec), s = sd(fluxL_umolm2sec), n = n())
for (m in f4$method) {
  record(paste0("fig4_mean_", m), f4$m[f4$method == m], "fig4")
  record(paste0("fig4_sd_", m), f4$s[f4$method == m], "fig4")
}

# ---- Table 2: morning (05-10 h) and evening (15-20 h) summaries -----------------------
t2 <- d %>% mutate(timeofday = case_when(hour >= 15 & hour <= 20 ~ "evening",
                                         hour >= 5 & hour <= 10 ~ "morning")) %>%
  filter(!is.na(timeofday)) %>% group_by(method, stand_label, timeofday) %>%
  summarise(mean = mean(fluxL_umolm2sec), median = median(fluxL_umolm2sec),
            p95 = quantile(fluxL_umolm2sec, 0.95), se = sd(fluxL_umolm2sec) / sqrt(n()),
            n = n(), .groups = "drop")
write.csv(t2, file.path(out_dir, "table2.csv"), row.names = FALSE)

# ---- Diel (Fig 6), common stand-hours ---------------------------------------------
dc <- d %>% semi_join(common_stand_hours(d), by = c("stand", "hour_of_obs"))
diel <- dc %>% mutate(day = as.Date(hour_of_obs)) %>%
  group_by(method, day, hour) %>% summarise(f = mean(fluxL_umolm2sec), .groups = "drop") %>%
  group_by(method, hour) %>%
  summarise(mean = mean(f), se = sd(f) / sqrt(n()), n_days = n(), .groups = "drop") %>%
  mutate(lo = mean - 1.96 * se, hi = mean + 1.96 * se)
write.csv(diel, file.path(out_dir, "diel_common_window.csv"), row.names = FALSE)
for (m in c("autochamber", "fluxbot")) {
  x <- diel %>% filter(method == m)
  record(paste0("diel_peak_hour_", m), x$hour[which.max(x$mean)], "diel")
  record(paste0("diel_peak_", m), max(x$mean), "diel")
  record(paste0("diel_min_hour_", m), x$hour[which.min(x$mean)], "diel")
  record(paste0("diel_min_", m), min(x$mean), "diel")
  record(paste0("diel_amplitude_", m), max(x$mean) - min(x$mean), "diel")
  record(paste0("diel_ci_halfwidth_median_", m), median(1.96 * x$se), "diel", "across days")
}
record("diel_r_between_systems", cor(diel$mean[diel$method == "autochamber"],
                                     diel$mean[diel$method == "fluxbot"]), "diel")

# ---- A4: Q10 on the common window --------------------------------------------------
# All filters dropped except flux > 0 (required by the log/exponential model).
# The submitted analysis additionally required flux > 0.5; reported as sensitivity.
dq <- d %>% semi_join(common_stand_hours(d), by = c("stand", "hour_of_obs")) %>% filter(!is.na(s10t))
record("q10_common_tmin", min(dq$s10t), "A4"); record("q10_common_tmax", max(dq$s10t), "A4")
for (thr in c(0, 0.5)) {
  q <- q10_common(d, min_flux = thr); tag <- ifelse(thr == 0, "", "_gt05")
  for (m in names(q)) {
    for (k in c("q10", "lo", "hi", "r2", "n")) record(paste0("q10", tag, "_", m, "_", k), q[[m]]$nls[k], "A4",
                                                      "nls exponential, common stand-hours")
    for (k in c("q10", "lo", "hi")) record(paste0("q10lmm", tag, "_", m, "_", k), q[[m]]$lmm[k], "A4",
                                           "log-linear mixed model, chamber random intercept")
  }
}
# full-record fits (each system's own temperature range), for comparison with the submitted text
dall <- d %>% filter(!is.na(s10t), fluxL_umolm2sec > 0)
for (m in c("autochamber", "fluxbot")) {
  q <- q10_nls(dall[dall$method == m, ])
  for (k in c("q10", "lo", "hi", "tmin", "tmax")) record(paste0("q10_fullrecord_", m, "_", k), q[k], "A4")
}
# Q10 of the hourly array-mean flux (spatial noise averaged out)
qa <- dq %>% group_by(method, hour_of_obs) %>%
  summarise(fluxL_umolm2sec = mean(fluxL_umolm2sec), s10t = mean(s10t), .groups = "drop")
for (m in c("autochamber", "fluxbot")) {
  q <- q10_nls(qa[qa$method == m, ])
  for (k in c("q10", "lo", "hi", "r2")) record(paste0("q10_arraymean_", m, "_", k), q[k], "A4")
}
# difference in temperature slope between systems (interaction), mixed model
mi <- lmer(log(fluxL_umolm2sec) ~ s10t * method + (1 | id), data = dq %>% filter(fluxL_umolm2sec > 0))
ct <- summary(mi)$coefficients["s10t:methodfluxbot", ]
record("q10_interaction_t", ct["t value"], "A4", "log-linear LMM s10t x method")
record("q10_interaction_p_approx", 2 * pnorm(-abs(ct["t value"])), "A4", "normal approximation")

# ---- Spatial heterogeneity (Fig 8) -------------------------------------------------
gi <- gini_arrays(d)
record("gini_autochamber", gi["autochamber"], "fig8"); record("gini_fluxbot", gi["fluxbot"], "fig8")
# stand-level CVs of chamber means over the common window
cv_tab <- dc %>% group_by(method, stand, id) %>% summarise(m = mean(fluxL_umolm2sec), .groups = "drop") %>%
  group_by(method, stand) %>% summarise(cv = sd(m) / mean(m), n_chambers = n(), mean = mean(m), .groups = "drop")
write.csv(cv_tab, file.path(out_dir, "cv_chamber_means.csv"), row.names = FALSE)
for (i in seq_len(nrow(cv_tab))) record(paste0("cv_", cv_tab$method[i], "_", cv_tab$stand[i]), cv_tab$cv[i], "A3")

# ---- A3: sampling effort -------------------------------------------------------------
# For each system x stand: chamber means over the common window, then
#  (1) analytic n for a 95% CI half-width of +/-E:  n = (t_{n-1} * CV / E)^2
#  (2) bootstrap: resample n chambers with replacement, relative error of the mean
#      vs the full-array mean; n at which 95% of draws fall within +/-E.
# Repeated for single-hour "snapshots" (median over hours of the per-hour CV).
n_required <- function(cv, E) {
  n <- 2
  while (qt(0.975, n - 1) * cv / sqrt(n) > E && n < 1000) n <- n + 1
  n
}
effort <- list()
for (m in c("autochamber", "fluxbot")) for (st in c("healthy", "unhealthy")) {
  cmn <- dc %>% filter(method == m, stand == st) %>% group_by(id) %>%
    summarise(f = mean(fluxL_umolm2sec), .groups = "drop") %>% pull(f)
  cv <- sd(cmn) / mean(cmn)
  hourly_cv <- dc %>% filter(method == m, stand == st) %>% group_by(hour_of_obs) %>%
    filter(n() >= 4) %>% summarise(cv = sd(fluxL_umolm2sec) / mean(fluxL_umolm2sec)) %>% pull(cv)
  cv_h <- median(hourly_cv)
  boot_err <- sapply(1:20, function(n) quantile(abs(replicate(4000, mean(sample(cmn, n, TRUE))) / mean(cmn) - 1), 0.95))
  effort[[paste(m, st)]] <- data.frame(method = m, stand = st, n = 1:20, err95 = boot_err,
                                       analytic = qt(0.975, pmax(1:20 - 1, 1)) * cv / sqrt(1:20),
                                       analytic_hourly = qt(0.975, pmax(1:20 - 1, 1)) * cv_h / sqrt(1:20),
                                       cv = cv, cv_hourly = cv_h, n_chambers = length(cmn))
  key <- paste0(m, "_", st)
  record(paste0("effort_cv_", key), cv, "A3", "CV of chamber means, common window")
  record(paste0("effort_cv_hourly_", key), cv_h, "A3", "median per-hour CV across chambers")
  record(paste0("effort_n10_", key), n_required(cv, 0.10), "A3", "chambers for +/-10% (95% CI), deployment mean")
  record(paste0("effort_n20_", key), n_required(cv, 0.20), "A3")
  record(paste0("effort_n10_hourly_", key), n_required(cv_h, 0.10), "A3", "single-hour stand mean")
  record(paste0("effort_n20_hourly_", key), n_required(cv_h, 0.20), "A3")
  record(paste0("effort_boot_n10_", key), which(boot_err <= 0.10)[1], "A3", "bootstrap, 95% of draws within 10%")
  record(paste0("effort_boot_n20_", key), which(boot_err <= 0.20)[1], "A3")
}
effort <- bind_rows(effort)
write.csv(effort, file.path(out_dir, "sampling_effort.csv"), row.names = FALSE)
# pooled-within-stand CV per system
for (m in c("autochamber", "fluxbot")) {
  x <- effort %>% filter(method == m, n == 1)
  record(paste0("effort_cv_mean_", m), mean(x$cv), "A3")
  record(paste0("effort_cv_hourly_mean_", m), mean(x$cv_hourly), "A3")
  record(paste0("effort_n10_mean_", m), n_required(mean(x$cv), 0.10), "A3")
  record(paste0("effort_n20_mean_", m), n_required(mean(x$cv), 0.20), "A3")
  record(paste0("effort_n10_hourly_mean_", m), n_required(mean(x$cv_hourly), 0.10), "A3")
  record(paste0("effort_n20_hourly_mean_", m), n_required(mean(x$cv_hourly), 0.20), "A3")
}

# ---- A5: temperature range -------------------------------------------------------------
met_oct <- load_met(c(274, 308))
record("s10t_min_deployment", min(d$s10t, na.rm = TRUE), "A5")
record("s10t_max_deployment", max(d$s10t, na.rm = TRUE), "A5")
record("s10t_min_autochamber", min(d$s10t[d$method == "autochamber"], na.rm = TRUE), "A5")
record("s10t_min_fluxbot", min(d$s10t[d$method == "fluxbot"], na.rm = TRUE), "A5")
record("n_fluxbot_obs_after_oct31", sum(d$method == "fluxbot" & d$hour_of_obs >= as.POSIXct("2023-11-01", tz = "America/New_York")), "A5")

# ---- QC diagnostics (A6) ------------------------------------------------------------------
raw_fb <- load_fluxbot(); raw_ac <- load_autochamber()
for (nm in c("fluxbot", "autochamber")) {
  x <- if (nm == "fluxbot") raw_fb else raw_ac
  pos <- x$flux[!is.na(x$flux) & x$flux >= 0]
  q <- quantile(pos, c(.25, .75)); up <- q[2] + 1.5 * diff(q)
  record(paste0("qc_n_raw_", nm), nrow(x), "A6")
  record(paste0("qc_n_negative_", nm), sum(x$flux < 0, na.rm = TRUE), "A6")
  record(paste0("qc_iqr_upper_fence_", nm), up, "A6")
  record(paste0("qc_n_above_fence_", nm), sum(pos > up), "A6")
  record(paste0("qc_pct_removed_iqr_", nm), 100 * (1 - mean(pos <= up & pos >= q[1] - 1.5 * diff(q))), "A6")
  record(paste0("qc_max_raw_", nm), max(pos), "A6")
}

# ================================================================================
# Sensitivity table (A6 QC, A8 linear vs quadratic, timestamps, A7 pressure)
# ================================================================================
scen <- list()
scen[["Main (goFlux best model, fit-based QC)"]] <- d
scen[["Flux model: linear (LM) for all closures"]] <- build_dataset(flux_col = "LM.flux")
scen[["Flux model: Hutchinson-Mosier (HM) for all closures"]] <- build_dataset(flux_col = "HM.flux")
scen[["QC: submitted rule (negatives removed, pooled 1.5 x IQR)"]] <- build_dataset(qc = "iqr")
scen[["QC: negatives removed only"]] <- build_dataset(qc = "none")
# autochamber logger clock read as EDT, as in the submitted analysis (1 h early)
d_shift <- d %>% select(-s10t, -bar, -air_t, -precip) %>%
  mutate(hour_of_obs = if_else(method == "autochamber", hour_of_obs - 3600, hour_of_obs), hour = hour(hour_of_obs)) %>%
  difference_left_join(load_met(), by = c("hour_of_obs" = "Time"), max_dist = as.difftime(60, units = "mins")) %>%
  reframe(s10t = mean(s10t), .by = c(id, hour_of_obs, stand, method, day_of_year, hour, fluxL_umolm2sec, stand_label))
scen[["Autochamber clock read as EDT (as submitted)"]] <- d_shift
p_ratio <- as.numeric(read.csv(file.path(flux_dir, "flux_run_metadata.csv")) %>% filter(key == "pressure_ratio_local_hf001") %>% pull(value))
scen[["Pressure: HF001 sea-level (as submitted)"]] <- d %>% mutate(fluxL_umolm2sec = fluxL_umolm2sec / p_ratio)
scen[["Autochamber: HF-published fluxes (HF293)"]] <- build_dataset_hf293()
scen[["Submitted flux files and QC"]] <- build_dataset(qc = "iqr", flux_col = "fluxL_umolm2sec", source = "submitted")
sens <- bind_rows(lapply(names(scen), function(k) key_stats(scen[[k]], k)))
write.csv(sens, file.path(out_dir, "sensitivity_table.csv"), row.names = FALSE)
print(sens %>% select(scenario, n, gam_method, gam_method_lo, gam_method_hi, ccc, paired_bias,
                      q10_ac, q10_fb, gini_ac, gini_fb), digits = 3)
for (i in seq_len(nrow(sens))) {
  tag <- c("main", "lm", "hm", "qc_iqr", "qc_none", "ac_edt", "pressure_sealevel", "hf293", "submitted")[i]
  for (k in c("gam_method", "gam_method_lo", "gam_method_hi", "ccc", "paired_bias", "q10_ac", "q10_fb",
              "gini_ac", "gini_fb", "max_flux", "n", "gam_intercept"))
    record(paste0("sens_", tag, "_", k), sens[[k]][i], "sensitivity", sens$scenario[i])
}

tab <- write_numbers("numbers_for_text.csv")
cat("\nWrote", nrow(tab), "numbers to", file.path(out_dir, "numbers_for_text.csv"), "\n")
