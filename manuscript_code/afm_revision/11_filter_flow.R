# Measurement accounting and filtering flow, with agreement between systems at each stage.
#
# Definitions (per closure = one intended chamber measurement), 2-31 October 2023, when
# both systems were deployed:
#   intended   : closures at each system's programmed interval for every installed unit
#                (Fluxbot: 1 per unit-hour; autochamber: 2 per chamber-hour). Units that stopped
#                before 31 October count as downtime.
#   recorded   : scheduled closures with any raw CO2 data in the closure window
#   valid      : recorded closures with enough data to fit a flux (Fluxbot >= 75% of the 180-s
#                window; autochamber >= 120 s of the 220-s window) that are not chamber
#                failures (no significant CO2 decline; significant CO2 accumulation, i.e. the chamber
#                sealed; no stuck lid, i.e. open-lid CO2 > 500 ppm above the other units through a
#                saturated episode)
#   retained   : valid closures that pass the per-chamber spike screen (median +/- 5 MAD). This is
#                the main, "as deployed" dataset (all conditions, wet sensors included).
#   RH-screened: retained closures without a wet-sensor flag (Fluxbot in-chamber RH >= 99% in the
#                open-lid minute; condensation on the K30, Pan et al. 2024). A strict subset,
#                compared alongside the main dataset.
# Derived rates: uptime = recorded / intended; downtime = 1 - uptime;
#   measurement success = retained / intended; QC retention = retained / recorded;
#   measurement density = retained closures per unit per day; replicated coverage = share of
#   hours with >= 3 retained chambers of a system in a stand.

source("afm_revision/00_prep.R")
suppressPackageStartupMessages({ library(readr); library(ggplot2) })

p0 <- as.POSIXct("2023-10-02", tz = "America/New_York"); p1 <- as.POSIXct("2023-11-01", tz = "America/New_York")
n_days <- as.numeric(difftime(p1, p0, units = "days"))
units <- read_csv(file.path(pkg, "metadata", "fluxbot_units.csv"), col_types = cols(unit = col_character()))
chambers <- read_csv(file.path(pkg, "metadata", "autochamber_chambers.csv"), show_col_types = FALSE)

# ---- recorded closures from the raw records ------------------------------------------------------
fbr <- read_csv(file.path(pkg, "raw", "fluxbot_sensor_records_2023.csv.gz"), col_types = cols(unit = col_character())) %>%
  mutate(time = as.POSIXct(unix_time, origin = "1970-01-01", tz = "America/New_York")) %>%
  filter(!is.na(co2_ppm), minute(time) >= 56) %>%
  mutate(hr = floor_date(time, "hour") + 3600) %>% filter(hr >= p0, hr < p1) %>% distinct(unit, hr)
acr <- read_csv(file.path(pkg, "raw", "autochamber_co2_1hz_oct2023.csv.gz"),
                col_types = cols(datetime_est = col_character(), chamber = col_integer(), co2_ppm = col_double())) %>%
  filter(chamber %in% 1:12, !is.na(co2_ppm)) %>%
  left_join(chambers %>% select(chamber, slot_minute), by = "chamber") %>%
  mutate(time = as.POSIXct(datetime_est, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT+5"),
         sec = ((minute(time) %% 30) - slot_minute) * 60 + second(time)) %>%
  filter(sec >= 75, sec < 295) %>%
  mutate(slot = floor_date(time, "30 mins")) %>% filter(slot >= p0, slot < p1) %>% distinct(chamber, slot)

acct <- function(system) {
  flx <- read.csv(file.path(flux_dir, paste0(system, "_fluxes.csv")), colClasses = c(id = "character")) %>%
    mutate(t = round_hour(start_local)) %>% filter(t >= p0, t < p1) %>%
    mutate(lid_fail = if ("lid_fail" %in% names(.)) coalesce(as.logical(lid_fail), FALSE) else FALSE,
           decline = (LM.flux < 0 & !grepl("p-value", quality.check)) | grepl("p-value", quality.check) | (!is.na(LM.r2) & LM.r2 < 0.5) | lid_fail,
           wet = coalesce(as.logical(wet), FALSE))
  # "recorded" windows that yielded no flux are counted as lost at the "valid" step
  n_units <- if (system == "fluxbot") nrow(units) else nrow(chambers)
  sched <- n_units * n_days * ifelse(system == "fluxbot", 24, 48)
  rec <- if (system == "fluxbot") nrow(fbr) else nrow(acr)
  comp <- nrow(flx)
  valid <- flx %>% filter(!decline)
  ret <- valid %>% group_by(id) %>% filter(abs(LM.flux - median(LM.flux)) <= 5 * mad(LM.flux)) %>% ungroup()
  scr <- ret %>% filter(!wet)
  tibble(system, stage = c("intended", "recorded", "valid", "retained", "RH-screened"),
         n = c(sched, rec, nrow(valid), nrow(ret), nrow(scr))) %>%
    mutate(pct_of_intended = 100 * n / sched, lost = lag(n) - n, pct_lost_step = 100 * lost / lag(n))
}
acc <- bind_rows(acct("fluxbot"), acct("autochamber"))
write.csv(acc, file.path(out_dir, "measurement_accounting.csv"), row.names = FALSE)
print(acc, n = 20)
for (i in seq_len(nrow(acc))) record(paste0("acct_", acc$system[i], "_", acc$stage[i]), acc$n[i], "accounting",
                                      sprintf("%.1f%% of intended", acc$pct_of_intended[i]))
for (s in c("fluxbot", "autochamber")) {
  a <- acc %>% filter(system == s); g <- function(st) a$n[a$stage == st]
  record(paste0("uptime_pct_", s), 100 * g("recorded") / g("intended"), "accounting")
  record(paste0("success_pct_", s), 100 * g("retained") / g("intended"), "accounting", "as deployed")
  record(paste0("success_screened_pct_", s), 100 * g("RH-screened") / g("intended"), "accounting", "RH-screened")
  record(paste0("qc_retention_pct_", s), 100 * g("retained") / g("recorded"), "accounting")
  record(paste0("density_per_unit_day_", s), g("retained") / (ifelse(s == "fluxbot", nrow(units), nrow(chambers)) * n_days), "accounting")
}

# ---- agreement at each stage ------------------------------------------------------------------------
stage_agree <- function(qc) {
  d <- build_dataset(qc = qc) %>% filter(hour_of_obs < p1)
  hrs <- matched_hours(d, 3)
  s <- d %>% filter(hour_of_obs %in% hrs) %>% group_by(hour_of_obs, stand, method) %>%
    summarise(f = mean(fluxL_umolm2sec), .groups = "drop") %>% group_by(hour_of_obs, method) %>%
    summarise(f = mean(f), .groups = "drop") %>% pivot_wider(names_from = method, values_from = f)
  dd <- s %>% mutate(day = as.Date(hour_of_obs, tz = "America/New_York")) %>% group_by(day) %>%
    filter(n() >= 12) %>% summarise(a = mean(autochamber), f = mean(fluxbot))
  tibble(stage = qc, n_hours = nrow(s), offset = mean(s$fluxbot - s$autochamber),
         offset_pct = 100 * (mean(s$fluxbot) / mean(s$autochamber) - 1),
         r_hourly = cor(s$autochamber, s$fluxbot), r_daily = cor(dd$a, dd$f), n_days = nrow(dd))
}
ag <- bind_rows(lapply(c("valid", "deployed", "screened"), stage_agree)) %>%
  mutate(stage = c("valid", "retained", "RH-screened"))
write.csv(ag, file.path(out_dir, "agreement_by_stage.csv"), row.names = FALSE)
print(ag)
for (i in seq_len(nrow(ag))) for (k in c("offset_pct", "r_hourly", "r_daily"))
  record(paste0("stage_", ag$stage[i], "_", k), ag[[k]][i], "accounting", "array means, hours with >= 3 chambers per system x stand")

# ---- Fig 8: Sankey diagram of closures, and agreement at each stage -------------------------------
# Main stream (top-aligned) narrows at each stage; each loss peels off downward as a ribbon to a labelled
# terminal, coloured by reason. Heights are % of intended closures, so the two systems are comparable.
source("afm_revision/fig_style.R")
sig <- function(x0, x1, ya0, ya1, yb0, yb1, n = 40) {   # ribbon from [ya0,ya1] at x0 to [yb0,yb1] at x1
  t <- seq(0, 1, length.out = n); s <- 3 * t^2 - 2 * t^3; x <- x0 + (x1 - x0) * t
  tibble(x = c(x, rev(x)), y = c(ya1 + (yb1 - ya1) * s, rev(ya0 + (yb0 - ya0) * s)))
}
reason <- c(recorded = "No data (logger, power or transmission)", valid = "Chamber failure or too few records",
            retained = "Spike", `RH-screened` = "Wet sensor (RH-screened subset only)")
rcol <- c("No data (logger, power or transmission)" = unname(pal_state["no data (down)"]),
          "Chamber failure or too few records" = unname(pal_state["measured, removed by QC"]),
          "Spike" = "#FEE090", "Wet sensor (RH-screened subset only)" = col_wet)
sys_lab <- c(fluxbot = "Fluxbot 2.0 (16 units)", autochamber = "Autochamber (12 chambers)")
stage_lab <- c(intended = "Intended", recorded = "Recorded", valid = "Valid", retained = "Retained\n(as deployed)", `RH-screened` = "RH-screened")
w <- 0.12
sk <- lapply(c("autochamber", "fluxbot"), function(sy) {
  a <- acc %>% filter(system == sy, !(sy == "autochamber" & stage == "RH-screened")) %>% mutate(f = pct_of_intended / 100, x = row_number())
  nodes <- a %>% transmute(system = sy, x, stage, n, f, type = "node")
  flows <- bind_rows(lapply(seq_len(nrow(a) - 1), function(k) {
    f0 <- a$f[k]; f1 <- a$f[k + 1]; l <- f0 - f1
    keep <- sig(a$x[k] + w / 2, a$x[k + 1] - w / 2, 1 - f1, 1, 1 - f1, 1) %>% mutate(part = "keep")
    lossy <- -0.08 - 0.42 * (k - 1) / 4
    loss <- if (l > 0) sig(a$x[k] + w / 2, a$x[k] + 0.62, 1 - f0, 1 - f1, lossy - l, lossy) %>% mutate(part = "loss") else NULL
    bind_rows(keep, loss) %>% mutate(system = sy, k = k, id = paste(sy, k, part), reason = reason[as.character(a$stage[k + 1])],
                                     lost = a$lost[k + 1], lx = a$x[k] + 0.64, ly = lossy - l / 2)
  }))
  list(nodes = nodes, flows = flows)
})
nodes <- bind_rows(lapply(sk, `[[`, "nodes")) %>% mutate(system = factor(sys_lab[system], levels = sys_lab))
flows <- bind_rows(lapply(sk, `[[`, "flows")) %>% mutate(system = factor(sys_lab[system], levels = sys_lab))
scol <- c("Autochamber (12 chambers)" = unname(pal_sys["autochamber"]), "Fluxbot 2.0 (16 units)" = unname(pal_sys["fluxbot"]))
lossl <- flows %>% filter(part == "loss") %>% distinct(system, id, lx, ly, lost, reason)
pa <- ggplot() +
  geom_polygon(data = flows %>% filter(part == "keep"), aes(x, y, group = id, fill = system), alpha = 0.35) +
  geom_polygon(data = flows %>% filter(part == "loss"), aes(x, y, group = id), fill = rcol[flows$reason[flows$part == "loss"]], alpha = 0.9) +
  geom_rect(data = nodes, aes(xmin = x - w / 2, xmax = x + w / 2, ymin = 1 - f, ymax = 1, fill = system)) +
  geom_text(data = nodes, aes(x = x, y = 1.03, label = sprintf("%s\n%s (%.0f%%)", stage_lab[as.character(stage)], format(n, big.mark = ","), 100 * f)),
            vjust = 0, size = 2.5, lineheight = 0.9) +
  geom_text(data = lossl, aes(x = lx, y = ly, label = paste0("\u2212", trimws(format(lost, big.mark = ",")))), hjust = 0, size = 2.4, colour = "grey20") +
  facet_wrap(~ system, ncol = 1) + scale_fill_manual(values = scol, guide = "none") +
  scale_x_continuous(limits = c(0.8, 5.9)) + scale_y_continuous(limits = c(-0.62, 1.22)) +
  theme_void(base_size = 8) + theme(strip.text = element_text(face = "bold", size = 8, hjust = 0.02, margin = margin(2, 0, 2, 0)), plot.tag = element_text(face = "bold", size = 10))
key <- tibble(reason = names(rcol), y = rev(seq_along(rcol)))
pk <- ggplot(key, aes(0, y)) + geom_tile(aes(fill = reason), width = 0.4, height = 0.7) + geom_text(aes(x = 0.35, label = reason), hjust = 0, size = 2.5) +
  scale_fill_manual(values = rcol, guide = "none") + xlim(-0.3, 4) + theme_void(base_size = 8) + labs(title = "Closures lost at each step") +
  theme(plot.title = element_text(size = 8, face = "bold"))
agt <- ag %>% mutate(stage = factor(stage, levels = rev(c("valid", "retained", "RH-screened")), labels = rev(c("Valid", "Retained (as deployed)", "RH-screened subset")))) %>%
  transmute(stage, Offset = sprintf("%+.1f%%", offset_pct), `r hourly` = sprintf("%.2f", r_hourly), `r daily` = sprintf("%.2f", r_daily), hl = stage != "Valid") %>%
  tidyr::pivot_longer(c(Offset, `r hourly`, `r daily`)) %>% mutate(name = factor(name, levels = c("Offset", "r hourly", "r daily")))
pb <- ggplot(agt, aes(name, stage)) + geom_tile(aes(fill = hl), colour = "white", linewidth = 1) + geom_text(aes(label = value), size = 2.6) +
  scale_fill_manual(values = c(`TRUE` = "#DCE6F2", `FALSE` = "#F2F2F2"), guide = "none") + scale_x_discrete(position = "top") +
  labs(x = NULL, y = NULL, title = "Agreement of array means at each stage") + theme_minimal(base_size = 8) +
  theme(panel.grid = element_blank(), plot.title = element_text(size = 8, face = "bold"), axis.text = element_text(colour = "black", size = 7), plot.tag = element_text(face = "bold", size = 10))
pflow <- (pa | (pk / pb / plot_spacer() + plot_layout(heights = c(0.7, 0.9, 0.6)))) + plot_layout(widths = c(1.9, 1)) + plot_annotation(tag_levels = list(c("a", "", "b"))) & theme(plot.tag = element_text(face = "bold", size = 10))
save_afm(pflow, "Fig8_measurement_flow", 190, 130)
write_numbers("numbers_filter_flow.csv")
