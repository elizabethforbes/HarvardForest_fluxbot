# Does removing wet-sensor closures bias the data directionally? The autochambers measure regardless of
# Fluxbot state, so they show what the soil was doing in the hours the Fluxbots lose:
#  (a) autochamber flux in stand-hours grouped by the share of that stand's Fluxbots flagged wet,
#      raw and relative to a temperature + time-of-day model fitted to all autochamber hours;
#  (b) autochamber mean over all hours vs over the hours used for the system comparison;
#  (c) autochamber October budget from all hours vs from only the stand-hours in which the Fluxbot
#      array has retained data, gap-filled the same way as the Fluxbot budget (12_scales_budget.R).
#      The difference is the bias the Fluxbot sampling pattern alone would put into its budget.

source("afm_revision/00_prep.R")
suppressPackageStartupMessages({ library(mgcv) })
p0 <- as.POSIXct("2023-10-02", tz = "America/New_York"); p1 <- as.POSIXct("2023-11-01", tz = "America/New_York")
stand_hour <- function(x, k) x %>% group_by(stand, hour_of_obs, id) %>% summarise(f = mean(fluxL_umolm2sec), .groups = "drop") %>%
  group_by(stand, hour_of_obs) %>% filter(n() >= k) %>% summarise(f = mean(f), n = n(), .groups = "drop") %>% mutate(stand = as.character(stand))
met <- load_met() %>% mutate(hour_of_obs = floor_date(with_tz(Time, "America/New_York"), "hour")) %>%
  group_by(hour_of_obs) %>% summarise(s10t = mean(s10t), prec = sum(prec), .groups = "drop")
hours <- tibble(hour_of_obs = seq(p0, p1 - 3600, by = "hour"))

d <- build_dataset(qc = "fit") %>% filter(hour_of_obs >= p0, hour_of_obs < p1)
ac <- stand_hour(d %>% filter(method == "autochamber"), 3) %>% left_join(met, by = "hour_of_obs") %>% mutate(hr = hour(hour_of_obs))
fb <- stand_hour(d %>% filter(method == "fluxbot"), 3) %>% select(stand, hour_of_obs, n_fb = n)
# share of each stand's recording Fluxbots flagged wet in each hour (stuck lids are not counted as wet)
wet <- load_fluxbot() %>% filter(hour_of_obs >= p0, hour_of_obs < p1, !lid_fail) %>% mutate(stand = as.character(stand)) %>%
  group_by(stand, hour_of_obs) %>% summarise(wet_share = mean(wet), n_rec = n(), .groups = "drop")
ac <- ac %>% left_join(wet, by = c("stand", "hour_of_obs")) %>% left_join(fb, by = c("stand", "hour_of_obs")) %>%
  mutate(fb_used = !is.na(n_fb),
         wet_class = cut(wet_share, c(-Inf, 0, 0.5, Inf), labels = c("no Fluxbot wet", "< half wet", ">= half wet")))
g <- gam(f ~ stand + s(s10t, k = 6) + s(hr, bs = "cc", k = 8), data = ac, knots = list(hr = c(-0.5, 23.5)))
ac$rel <- ac$f / predict(g, newdata = ac)

# (a)
a <- ac %>% filter(!is.na(wet_class)) %>% group_by(wet_class) %>%
  summarise(n_hours = n(), ac_mean = mean(f), ac_rel_to_model = mean(rel), s10t = mean(s10t), .groups = "drop")
print(a); write.csv(a, file.path(out_dir, "selection_bias_by_wet_share.csv"), row.names = FALSE)
for (i in seq_len(nrow(a))) { tg <- c("none_wet", "lt_half_wet", "ge_half_wet")[as.integer(a$wet_class[i])]
  record(paste0("selbias_ac_rel_", tg), a$ac_rel_to_model[i], "selection_bias", "autochamber flux / temperature+hour model")
  record(paste0("selbias_ac_mean_", tg), a$ac_mean[i], "selection_bias"); record(paste0("selbias_n_", tg), a$n_hours[i], "selection_bias") }
mw <- lm(log(rel) ~ wet_share + stand, data = ac %>% filter(!is.na(wet_share), rel > 0))
record("selbias_ac_pct_per_wet_share", 100 * (exp(coef(mw)[["wet_share"]]) - 1), "selection_bias", "autochamber flux vs model, all vs no Fluxbots wet")

# (b) means over all hours vs the hours with >= 3 retained Fluxbots in the stand (the compared hours)
m_all <- mean(ac$f); m_used <- mean(ac$f[ac$fb_used]); r_used <- mean(ac$rel[ac$fb_used]); r_unused <- mean(ac$rel[!ac$fb_used])
record("selbias_ac_mean_all_hours", m_all, "selection_bias"); record("selbias_ac_mean_compared_hours", m_used, "selection_bias")
record("selbias_ac_compared_vs_all_pct", 100 * (m_used / m_all - 1), "selection_bias", "raw")
record("selbias_ac_rel_compared_hours", r_used, "selection_bias"); record("selbias_ac_rel_other_hours", r_unused, "selection_bias")

# (c) budget: all autochamber hours vs autochamber restricted to the Fluxbot sampling pattern
budget <- function(s) bind_rows(lapply(c("healthy", "unhealthy"), function(st) {
  x <- s %>% filter(stand == st)
  gm <- gam(f ~ s(s10t, k = 6) + s(hr, bs = "cc", k = 8), data = x, knots = list(hr = c(-0.5, 23.5)))
  full <- hours %>% left_join(met, by = "hour_of_obs") %>% mutate(hr = hour(hour_of_obs)) %>%
    left_join(x %>% select(hour_of_obs, f), by = "hour_of_obs") %>% mutate(f_filled = coalesce(f, predict(gm, newdata = .)))
  tibble(stand = st, gC_m2 = sum(full$f_filled) * 3600 * 12.011e-6, share_observed = mean(!is.na(full$f)))
}))
b_all <- budget(ac); b_sub <- budget(ac %>% filter(fb_used))
b_wetkept <- { fbw <- stand_hour(build_dataset(qc = "fit_nowet") %>% filter(method == "fluxbot", hour_of_obs >= p0, hour_of_obs < p1), 3) %>% select(stand, hour_of_obs)
               budget(ac %>% semi_join(fbw, by = c("stand", "hour_of_obs"))) }
bb <- bind_rows(b_all %>% mutate(sampling = "all autochamber hours"), b_sub %>% mutate(sampling = "Fluxbot retained hours (main QC)"),
                b_wetkept %>% mutate(sampling = "Fluxbot hours with wet closures kept"))
print(bb); write.csv(bb, file.path(out_dir, "selection_bias_budget.csv"), row.names = FALSE)
tot <- bb %>% group_by(sampling) %>% summarise(g = mean(gC_m2), obs = mean(share_observed))
for (i in seq_len(nrow(tot))) { tg <- gsub("[^a-z0-9]+", "_", tolower(tot$sampling[i]))
  record(paste0("selbias_budget_", tg), tot$g[i], "selection_bias", "autochamber, g C m-2, 2-31 Oct, mean of stands")
  record(paste0("selbias_budget_obs_share_", tg), tot$obs[i], "selection_bias") }
record("selbias_budget_main_vs_all_pct", 100 * (tot$g[tot$sampling == "Fluxbot retained hours (main QC)"] / tot$g[tot$sampling == "all autochamber hours"] - 1), "selection_bias",
       "bias from the Fluxbot sampling pattern alone")
record("selbias_budget_wetkept_vs_all_pct", 100 * (tot$g[tot$sampling == "Fluxbot hours with wet closures kept"] / tot$g[tot$sampling == "all autochamber hours"] - 1), "selection_bias")
print(write_numbers("numbers_selection_bias.csv") %>% select(key, value), row.names = FALSE)
