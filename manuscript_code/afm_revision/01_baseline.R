# Reproduce the headline numbers of the submitted (Ecosphere) text from the
# .qmd logic, so later changes can be compared against a verified baseline.

source("afm_revision/00_prep.R")
suppressPackageStartupMessages({ library(mgcv); library(zoo); library(epiR); library(ineq) })

# submitted flux files, submitted QC, HF001 parsed in local (New York) time as the .qmd did
d <- build_dataset(qc = "iqr", flux_col = "fluxL_umolm2sec", source = "submitted", met = load_met(tz = "America/New_York"))

# ---- n --------------------------------------------------------------------------
record("n_obs_merged", nrow(d), "baseline", "rows in merged_data_with_met (text: 13591)")
record("n_fluxbot_after_qc", sum(d$method == "fluxbot"), "baseline")
record("n_autochamber_hours_after_qc", sum(d$method == "autochamber"), "baseline")

# ---- GAM (Table 1), as specified in the .qmd -------------------------------------
gam_fs <- gam(fluxL_umolm2sec ~ stand + s(hour) + method + s(id, bs = "fs") + s(s10t),
              data = d, method = "REML")
s <- summary(gam_fs)
record("gam_fs_intercept", s$p.table["(Intercept)", 1], "baseline", "text 2.687")
record("gam_fs_stand2", s$p.table["standunhealthy", 1], "baseline", "text -0.013")
record("gam_fs_method", s$p.table["methodfluxbot", 1], "baseline", "text -0.332")
record("gam_fs_method_se", s$p.table["methodfluxbot", 2], "baseline", "text 0.250")
record("gam_fs_method_p", s$p.table["methodfluxbot", 4], "baseline", "text 0.185")
record("gam_fs_r2adj", s$r.sq, "baseline", "text 0.541")
record("gam_fs_edf_id", s$s.table["s(id)", "edf"], "baseline", "text 23.93")
saveRDS(gam_fs, file.path(out_dir, "gam_fs.rds"))

# ---- Fig 4/5 subset: .qmd logic verbatim ------------------------------------------
merged_data <- d  # qmd applies the chamber filter to merged_data (pre-met), same rows
chamber_counts <- merged_data %>% group_by(hour_of_obs, method, stand) %>%
  summarise(n_chambers = n_distinct(id), .groups = "drop")
valid_hours <- chamber_counts %>% group_by(hour_of_obs) %>%
  filter(all(c("fluxbot", "autochamber") %in% method) &
           all(c("healthy", "unhealthy") %in% stand) &
           all(n_chambers[method == "fluxbot" & stand == "healthy"] >= 5) &
           all(n_chambers[method == "fluxbot" & stand == "unhealthy"] >= 5) &
           all(n_chambers[method == "autochamber" & stand == "healthy"] >= 5) &
           all(n_chambers[method == "autochamber" & stand == "unhealthy"] >= 5)) %>%
  ungroup() %>% distinct(hour_of_obs)
filtered_data <- merged_data %>% filter(hour_of_obs %in% valid_hours$hour_of_obs)
record("fig4_n_hours", nrow(valid_hours), "baseline")
m4 <- filtered_data %>% group_by(method) %>% summarise(m = mean(fluxL_umolm2sec))
record("fig4_mean_autochamber", m4$m[m4$method == "autochamber"], "baseline", "qmd comment 2.690")
record("fig4_mean_fluxbot", m4$m[m4$method == "fluxbot"], "baseline", "qmd comment 2.389")

ccc_pipeline <- function(filtered_data, drop_low_auto = TRUE) {
  pv <- filtered_data %>% group_by(hour_of_obs, stand, method) %>%
    summarise(mean_flux = mean(fluxL_umolm2sec), .groups = "drop") %>%
    pivot_wider(names_from = method, values_from = mean_flux)
  if (drop_low_auto) pv <- pv %>% filter(autochamber > 1)
  pv <- pv %>% group_by(hour_bin = floor_date(hour_of_obs, "hour")) %>%
    summarise(autochamber_mean = mean(autochamber, na.rm = TRUE),
              fluxbot_mean = mean(fluxbot, na.rm = TRUE)) %>%
    filter(!is.na(autochamber_mean) & !is.na(fluxbot_mean)) %>%
    arrange(hour_bin) %>%
    mutate(ac = rollapply(autochamber_mean, 3, mean, fill = NA, align = "right"),
           fb = rollapply(fluxbot_mean, 3, mean, fill = NA, align = "right")) %>%
    filter(!is.na(ac) & !is.na(fb))
  fit <- lm(fb ~ ac, data = pv)
  cc <- epi.ccc(pv$ac, pv$fb)
  list(data = pv, n = nrow(pv), intercept = coef(fit)[1], slope = coef(fit)[2],
       r2 = summary(fit)$r.squared, ccc = cc$rho.c$est, ccc_lo = cc$rho.c$lower,
       ccc_hi = cc$rho.c$upper, bias = mean(pv$fb - pv$ac))
}
cp <- ccc_pipeline(filtered_data)
record("fig5_n", cp$n, "baseline")
record("fig5_ccc", cp$ccc, "baseline", "text 0.70")
record("fig5_intercept", cp$intercept, "baseline", "text -0.49")
record("fig5_slope", cp$slope, "baseline", "text 1.09")
record("fig5_r2", cp$r2, "baseline", "text 0.67")
cp_all <- ccc_pipeline(filtered_data, drop_low_auto = FALSE)
record("fig5_ccc_no_autochamber_gt1_filter", cp_all$ccc, "baseline",
       "qmd drops hours with autochamber stand-mean <= 1 before CCC")
record("fig5_n_no_autochamber_gt1_filter", cp_all$n, "baseline")

# ---- Q10 (Fig 7) as in .qmd ---------------------------------------------------------
q10_fit <- function(dd) {
  m <- nls(fluxL_umolm2sec ~ a * exp(b * s10t), data = dd, start = list(a = 1, b = 0.1))
  b <- unname(coef(m)["b"]); se <- sqrt(vcov(m)["b", "b"])
  c(q10 = exp(10 * b), lo = exp(10 * (b - 1.96 * se)), hi = exp(10 * (b + 1.96 * se)),
    tmin = min(dd$s10t), tmax = max(dd$s10t), n = nrow(dd))
}
dq <- d %>% filter(!is.na(s10t), fluxL_umolm2sec > 0.5)
for (m in c("autochamber", "fluxbot")) {
  q <- q10_fit(dq[dq$method == m, ])
  record(paste0("q10_", m), q["q10"], "baseline", ifelse(m == "autochamber", "text 2.31", "text 2.86"))
  record(paste0("q10_", m, "_tmin"), q["tmin"], "baseline")
  record(paste0("q10_", m, "_tmax"), q["tmax"], "baseline")
}

# ---- Gini (Fig 8) as in .qmd -------------------------------------------------------
g <- d %>% group_by(hour_of_obs) %>%
  filter(sum(method == "autochamber") >= 12 & sum(method == "fluxbot") >= 12) %>% ungroup() %>%
  group_by(method, id) %>% summarise(mf = mean(fluxL_umolm2sec), .groups = "drop") %>%
  group_by(method) %>% summarise(gini = Gini(mf), n = n())
record("gini_autochamber", g$gini[g$method == "autochamber"], "baseline", "text 0.13")
record("gini_fluxbot", g$gini[g$method == "fluxbot"], "baseline", "text 0.16")

# ---- Table 2 ------------------------------------------------------------------------
# .qmd filters with `timeofday == c("morning","evening")`, which recycles the
# vector and silently drops about half the rows. Reproduce both versions.
t2_in <- dq %>% mutate(timeofday = case_when(hour >= 15 & hour <= 20 ~ "evening",
                                             hour >= 5 & hour <= 10 ~ "morning", TRUE ~ "mid"))
t2 <- function(x) x %>% group_by(method, stand_label, timeofday) %>%
  summarise(mean = mean(fluxL_umolm2sec), median = median(fluxL_umolm2sec),
            p95 = quantile(fluxL_umolm2sec, 0.95), se = sd(fluxL_umolm2sec) / sqrt(n()),
            n = n(), .groups = "drop")
t2_bug <- t2(t2_in %>% filter(timeofday == c("morning", "evening")))
t2_fix <- t2(t2_in %>% filter(timeofday %in% c("morning", "evening")))
write.csv(t2_bug, file.path(out_dir, "table2_as_submitted_logic.csv"), row.names = FALSE)
write.csv(t2_fix, file.path(out_dir, "table2_fixed_filter.csv"), row.names = FALSE)

print(write_numbers("numbers_baseline.csv"), row.names = FALSE)
print(t2_bug); print(t2_fix)
