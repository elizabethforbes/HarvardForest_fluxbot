# Diagnostics for the difference in temporal dynamics between systems.
#  1. In-chamber air temperature (Fluxbot SHT-30): ambient (lid open) vs HF001 air
#     temperature, warming during the 5-min closure, diel pattern, link to radiation.
#  2. Our recalculated autochamber fluxes vs the Harvard Forest team's own processed
#     fluxes (HF293-07, which also includes per-chamber soil temperature).
#  3. Q10 compared with chambers as the unit of replication (cluster bootstrap).
#  4. Antecedent precipitation as a moisture proxy (no 2023 soil-moisture record found).

source("afm_revision/00_prep.R")
suppressPackageStartupMessages({ library(readxl); library(stringr); library(ggplot2); library(patchwork) })
set.seed(20260930)

hf_bots <- c(healthy = "100|13|22|114|108|101|112|111", unhealthy = "113|103|102|105|24|106|104|110")
bots <- unlist(str_split(hf_bots, "\\|"))
fdir <- file.path("..", "old R scripts", "fluxbot data 2023")
parse_vec <- function(x) suppressWarnings(as.numeric(str_split(str_remove_all(x, "\\[|\\]"), ",")[[1]]))
read_var <- function(file, col) {
  f <- file.path(fdir, file); sh <- excel_sheets(f); sh <- sh[str_extract(sh, "\\d+$") %in% bots]
  bind_rows(lapply(sh, function(s) {
    x <- read_excel(f, sheet = s)
    bind_rows(lapply(seq_len(nrow(x)), function(i) {
      t <- parse_vec(x[["device timestamps"]][i]); v <- parse_vec(x[[col]][i]); n <- min(length(t), length(v))
      data.frame(unix = t[seq_len(n)], v = v[seq_len(n)])
    })) %>% mutate(bot = str_extract(s, "\\d+$"))
  }))
}
temp <- read_var("fluxbot_temperature.xlsx", "temprerature") %>% rename(tc = v)
co2 <- read_var("fluxbot_co2.xlsx", "co2") %>% rename(co2 = v)
raw <- temp %>% distinct(bot, unix, .keep_all = TRUE) %>% inner_join(co2 %>% distinct(bot, unix, .keep_all = TRUE), by = c("bot", "unix")) %>%
  mutate(time = as.POSIXct(unix, origin = "1970-01-01", tz = "America/New_York"),
         stand = if_else(grepl(paste0("^(", hf_bots["healthy"], ")$"), bot), "healthy", "unhealthy"),
         min = minute(time) + second(time) / 60) %>%
  filter(time >= as.POSIXct("2023-10-02", tz = "America/New_York"), time < as.POSIXct("2023-11-05", tz = "America/New_York"),
         tc > -20, tc < 50, co2 < 65000)
dt_s <- raw %>% arrange(bot, time) %>% group_by(bot) %>% mutate(dt = as.numeric(difftime(time, lag(time), units = "secs"))) %>%
  filter(dt > 0, dt < 60) %>% pull(dt)
record("fluxbot_sample_interval_s_median", median(dt_s), "chamber_physics", "raw sampling interval")

# each record spans minute 55-60: 55:00-55:59 lid open, 56:00-59:59 fit window (closed)
iv <- raw %>% filter(min >= 55) %>% mutate(hour_of_obs = ceiling_date(time, "hour")) %>%
  group_by(stand, bot, hour_of_obs) %>% filter(n() >= 20) %>%
  summarise(t_open = mean(tc[min < 56]), t_start = mean(tc[min >= 56 & min < 56.5]),
            t_end = mean(tc[min >= 59.5]), t_closed = mean(tc[min >= 56]),
            co2_open = mean(co2[min < 56]), .groups = "drop") %>%
  mutate(dT_closure = t_end - t_start)
met <- load_met() %>% mutate(hour_of_obs = floor_date(with_tz(Time, "America/New_York"), "hour")) %>%
  group_by(hour_of_obs) %>% summarise(airt = mean(airt), slrr = mean(slrr), s10t = mean(s10t), prec = sum(prec), wspd = mean(wspd))
iv <- iv %>% inner_join(met, by = "hour_of_obs") %>% mutate(hod = hour(hour_of_obs))
saveRDS(iv, file.path(out_dir, "fluxbot_chamber_temperature.rds"))

record("chamberT_open_minus_airt_mean", mean(iv$t_open - iv$airt, na.rm = TRUE), "chamber_physics", "Fluxbot in-chamber T (lid open) minus HF001 air T")
record("chamberT_open_vs_airt_r", cor(iv$t_open, iv$airt, use = "complete"), "chamber_physics")
record("dT_closure_mean", mean(iv$dT_closure, na.rm = TRUE), "chamber_physics", "warming during 4-min fit window, degC")
record("dT_closure_p95", quantile(iv$dT_closure, 0.95, na.rm = TRUE), "chamber_physics")
record("dT_closure_max", max(iv$dT_closure, na.rm = TRUE), "chamber_physics")
fit_dT <- lm(dT_closure ~ slrr, data = iv)
record("dT_closure_per_100Wm2", 100 * coef(fit_dT)[2], "chamber_physics")
record("dT_closure_vs_slrr_r2", summary(fit_dT)$r.squared, "chamber_physics")
dielT <- iv %>% group_by(hod) %>% summarise(t_open = mean(t_open, na.rm = TRUE), airt = mean(airt), s10t = mean(s10t),
                                              dT = mean(dT_closure, na.rm = TRUE), slrr = mean(slrr))
write.csv(dielT, file.path(out_dir, "fluxbot_chamberT_diel.csv"), row.names = FALSE)
record("diel_amp_chamberT_open", diff(range(dielT$t_open)), "chamber_physics", "degC")
record("diel_amp_airt", diff(range(dielT$airt)), "chamber_physics")
record("diel_amp_s10t", diff(range(dielT$s10t)), "chamber_physics")
record("diel_peak_hour_chamberT_open", dielT$hod[which.max(dielT$t_open)], "chamber_physics")
record("diel_peak_hour_airt", dielT$hod[which.max(dielT$airt)], "chamber_physics")
record("diel_peak_hour_s10t", dielT$hod[which.max(dielT$s10t)], "chamber_physics")
# effect of chamber warming on the gas-law conversion is ~ (1/T); the more important physical
# effect is thermal expansion of headspace air during closure if the chamber is vented:
# mole fraction unchanged, but a closed rigid unvented chamber would pressurize. Report size:
record("dT_closure_as_pct_of_TK", 100 * mean(iv$dT_closure, na.rm = TRUE) / (mean(iv$t_start, na.rm = TRUE) + 273.15), "chamber_physics")

# does warming during closure relate to flux, and to the Fluxbot/autochamber ratio?
d <- readRDS(file.path(out_dir, "dataset_iqr.rds"))
fbx <- d %>% filter(method == "fluxbot") %>% mutate(bot = sub("fluxbot", "", as.character(id))) %>%
  inner_join(iv %>% select(bot, hour_of_obs, dT_closure, t_open, t_closed), by = c("bot", "hour_of_obs"))
record("n_fluxbot_fluxes_with_chamberT", nrow(fbx), "chamber_physics")
m1 <- lm(log(fluxL_umolm2sec) ~ s10t + dT_closure, data = fbx %>% filter(fluxL_umolm2sec > 0.5))
record("flux_pct_per_C_closure_warming", 100 * (exp(coef(m1)["dT_closure"]) - 1), "chamber_physics", "controlling for s10t")
record("flux_pct_per_C_closure_warming_se", 100 * summary(m1)$coefficients["dT_closure", 2], "chamber_physics")
m2 <- lm(log(fluxL_umolm2sec) ~ s10t + I(t_open - s10t), data = fbx %>% filter(fluxL_umolm2sec > 0.5))
record("flux_pct_per_C_air_minus_soil_fb", 100 * (exp(coef(m2)[3]) - 1), "chamber_physics", "in-chamber open-lid T minus s10t")
ac <- d %>% filter(method == "autochamber", fluxL_umolm2sec > 0.5) %>% left_join(met %>% select(hour_of_obs, airt), by = "hour_of_obs")
m3 <- lm(log(fluxL_umolm2sec) ~ s10t + I(airt - s10t), data = ac)
record("flux_pct_per_C_air_minus_soil_ac", 100 * (exp(coef(m3)[3]) - 1), "chamber_physics", "HF001 air T minus s10t")
m2b <- lm(log(fluxL_umolm2sec) ~ s10t + I(airt - s10t), data = fbx %>% filter(fluxL_umolm2sec > 0.5) %>%
            left_join(met %>% select(hour_of_obs, airt), by = "hour_of_obs"))
record("flux_pct_per_C_airHF_minus_soil_fb", 100 * (exp(coef(m2b)[3]) - 1), "chamber_physics", "HF001 air T minus s10t, Fluxbots")

# ---- 2. our autochamber fluxes vs the HF team's processed fluxes (HF293-07) -------------------
hf <- read.csv(file.path("data", "hf293-07-soil-resp-2022-2023.csv")) %>%
  mutate(time = as.POSIXct(datetime, format = "%Y-%m-%dT%H:%M", tz = "Etc/GMT+5"))
record("hf293_2023_first", as.numeric(format(min(hf$time[hf$year == 2023]), "%j")), "hf293", "doy of first 2023 record")
months23 <- table(hf$month[hf$year == 2023]); for (mm in names(months23)) record(paste0("hf293_2023_n_month", mm), months23[[mm]], "hf293")
ours <- load_autochamber() %>% mutate(ch = as.integer(id),
  t0 = as.POSIXct(start_timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York"))
hfo <- hf %>% filter(year == 2023, month == 10) %>% mutate(ch = chamber)
# match each of our intervals to the HF record of the same chamber within 10 min
mt <- difference_inner_join(ours %>% select(ch, t0, flux), hfo %>% select(ch2 = ch, time, rs, tsoil),
                            by = c("t0" = "time"), max_dist = as.difftime(10, units = "mins")) %>%
  filter(ch == ch2) %>% mutate(lag_min = as.numeric(difftime(t0, time, units = "mins"))) %>%
  group_by(ch, t0) %>% slice_min(abs(lag_min), n = 1, with_ties = FALSE) %>% ungroup()
record("hf293_matched_n", nrow(mt), "hf293")
record("hf293_lag_min_median", median(mt$lag_min), "hf293", "our interval start minus HF record time (EST parsed)")
mtp <- mt %>% filter(rs > 0, flux > 0)
record("hf293_ratio_ours_over_hf_median", median(mtp$flux / mtp$rs), "hf293")
record("hf293_r", cor(mtp$flux, mtp$rs), "hf293")
record("hf293_mean_ours", mean(mtp$flux), "hf293"); record("hf293_mean_hf", mean(mtp$rs), "hf293")
lagscan <- sapply(seq(-120, 120, 30), function(L) { h2 <- hfo %>% mutate(time = time + L * 60)
  j <- inner_join(ours %>% mutate(k = round_date(t0, "5 mins")), h2 %>% mutate(k = round_date(time, "5 mins")), by = c("ch" = "chamber", "k"))
  if (nrow(j) < 100) NA else cor(j$flux, j$rs, use = "complete") })
names(lagscan) <- seq(-120, 120, 30); print(round(lagscan, 3))
record("hf293_best_shift_min", as.numeric(names(which.max(lagscan))), "hf293", "shift of HF record (min) maximizing r with our fluxes")
# HF-processed autochamber diel amplitude and Q10 (their own flux calc and local tsoil)
hfd <- hfo %>% filter(rs > 0.5) %>% mutate(hod = hour(with_tz(time, "America/New_York")))
dh <- hfd %>% group_by(hod) %>% summarise(rs = mean(rs))
record("hf293_diel_rel_amp_pct", 100 * diff(range(dh$rs)) / mean(dh$rs), "hf293")
record("hf293_diel_peak_hour", dh$hod[which.max(dh$rs)], "hf293", "EDT")
qh <- nls(rs ~ a * exp(b * tsoil), data = hfd %>% filter(!is.na(tsoil)), start = list(a = 1, b = 0.1))
record("hf293_q10_local_tsoil", exp(10 * coef(qh)["b"]), "hf293", "HF-processed rs vs chamber tsoil, October")

# ---- 3. Q10 with chambers as replicates -----------------------------------------------------------
common <- d %>% distinct(stand, hour_of_obs, method) %>% count(stand, hour_of_obs) %>% filter(n == 2)
dq <- d %>% semi_join(common, by = c("stand", "hour_of_obs")) %>% filter(!is.na(s10t), fluxL_umolm2sec > 0.5)
cq <- dq %>% group_by(method, stand, id) %>% filter(n() >= 100) %>%
  summarise(b = coef(lm(log(fluxL_umolm2sec) ~ s10t))[2], .groups = "drop") %>% mutate(q10 = exp(10 * b))
boot <- replicate(5000, {
  bb <- cq %>% group_by(method, stand) %>% slice_sample(prop = 1, replace = TRUE) %>% group_by(method) %>% summarise(b = mean(b))
  c(ac = bb$b[bb$method == "autochamber"], fb = bb$b[bb$method == "fluxbot"]) })
for (m in c("ac", "fb")) {
  record(paste0("q10_chamberboot_", m), exp(10 * mean(cq$b[cq$method == ifelse(m == "ac", "autochamber", "fluxbot")])), "Q10_chambers", "exp(10 x mean chamber slope)")
  record(paste0("q10_chamberboot_", m, "_lo"), exp(10 * quantile(boot[m, ], 0.025)), "Q10_chambers")
  record(paste0("q10_chamberboot_", m, "_hi"), exp(10 * quantile(boot[m, ], 0.975)), "Q10_chambers")
}
rat <- exp(10 * (boot["fb", ] - boot["ac", ]))
record("q10_chamberboot_ratio", exp(10 * (mean(cq$b[cq$method == "fluxbot"]) - mean(cq$b[cq$method == "autochamber"]))), "Q10_chambers")
record("q10_chamberboot_ratio_lo", quantile(rat, 0.025), "Q10_chambers"); record("q10_chamberboot_ratio_hi", quantile(rat, 0.975), "Q10_chambers")
record("q10_chamberboot_p_ratio_le1", mean(rat <= 1), "Q10_chambers", "one-sided bootstrap p")
record("q10_chamber_ttest_p", t.test(b ~ method, data = cq)$p.value, "Q10_chambers", "Welch t on chamber log-slopes")
# excluding two Fluxbot chambers with the most extreme Q10 (13, 114; fewest observations)
cq2 <- cq %>% filter(!(id %in% c("fluxbot13", "fluxbot114")))
record("q10_ratio_excl_2_extreme", exp(10 * (mean(cq2$b[cq2$method == "fluxbot"]) - mean(cq2$b[cq2$method == "autochamber"]))), "Q10_chambers")
record("q10_ttest_p_excl_2_extreme", t.test(b ~ method, data = cq2)$p.value, "Q10_chambers")

# ---- 4. antecedent precipitation --------------------------------------------------------------------
metp <- met %>% arrange(hour_of_obs) %>% mutate(p72 = zoo::rollapply(prec, 72, sum, fill = NA, align = "right", partial = TRUE))
record("precip_total_deployment_mm", sum(met$prec[met$hour_of_obs >= as.POSIXct("2023-10-02", tz = "America/New_York") &
                                                  met$hour_of_obs < as.POSIXct("2023-11-01", tz = "America/New_York")], na.rm = TRUE), "moisture")
hrs3 <- matched_hours(d, 3)
rs3 <- d %>% filter(hour_of_obs %in% hrs3) %>% group_by(hour_of_obs, stand, method) %>%
  summarise(f = mean(fluxL_umolm2sec), .groups = "drop") %>% pivot_wider(names_from = method, values_from = f) %>%
  group_by(hour_of_obs) %>% summarise(ac = mean(autochamber), fb = mean(fluxbot)) %>%
  inner_join(metp, by = "hour_of_obs") %>% mutate(lr = log(fb / ac), grad = airt - s10t)
mp <- lm(lr ~ grad + slrr + log1p(p72), data = rs3)
record("ratio_pct_per_log1p_p72", 100 * (exp(coef(mp)["log1p(p72)"]) - 1), "moisture", "Fluxbot/autochamber ratio vs 72-h rain")
record("ratio_p72_p", summary(mp)$coefficients["log1p(p72)", 4], "moisture")
for (sys in c("ac", "fb")) {
  mm <- lm(log(rs3[[sys]]) ~ s10t + log1p(p72), data = rs3)
  record(paste0("flux_pct_per_log1p_p72_", sys), 100 * (exp(coef(mm)["log1p(p72)"]) - 1), "moisture", "array mean, controlling for s10t")
  record(paste0("flux_p72_p_", sys), summary(mm)$coefficients["log1p(p72)", 4], "moisture")
}

# ---- figure for co-authors: chamber temperature diagnostics -------------------------------------------
pA <- ggplot(dielT, aes(hod)) + geom_line(aes(y = t_open, colour = "Fluxbot chamber (lid open)")) +
  geom_line(aes(y = airt, colour = "HF001 air")) + geom_line(aes(y = s10t, colour = "HF001 soil 10 cm")) +
  labs(x = "Hour of day", y = "Temperature (°C)", colour = NULL) + theme_classic(base_size = 8) + theme(legend.position = c(0.25, 0.85))
pB <- ggplot(iv, aes(slrr, dT_closure)) + geom_point(alpha = 0.1, size = 0.4) + geom_smooth(method = "lm", formula = y ~ x) +
  labs(x = expression(Solar~radiation~(W~m^-2)), y = "Warming during closure (°C)") + theme_classic(base_size = 8)
ggsave(file.path(out_dir, "figures", "diag_chamber_temperature.png"), pA + pB, width = 180, height = 70, units = "mm", dpi = 200, device = ragg::agg_png)

print(write_numbers("numbers_chamber_physics.csv"), row.names = FALSE)
