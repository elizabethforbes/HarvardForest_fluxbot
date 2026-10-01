# Unit-level reliability vs array-level resilience, 2-31 October 2023 (720 stand-hours per stand).
#  Unit level  : success rate = share of the unit's hours with a retained measurement.
#  Array level : coverage_k = share of stand-hours with >= k units retained; outage = stand-hour
#                with no retained unit; longest outage (h).
#  Redundancy  : observed outage (and < 3-unit) frequencies vs those expected if units failed
#                independently with their observed success rates (Poisson-binomial). Observed >>
#                expected = failures are shared (common power/logger/pneumatics); observed ~
#                expected = the array's redundancy is realised.

source("afm_revision/00_prep.R")
suppressPackageStartupMessages({ library(ggplot2); library(patchwork) })
p0 <- as.POSIXct("2023-10-02", tz = "America/New_York"); p1 <- as.POSIXct("2023-11-01", tz = "America/New_York")
hours <- seq(p0, p1 - 3600, by = "hour")
units <- read.csv(file.path(pkg, "metadata", "fluxbot_units.csv"), colClasses = c(unit = "character"))
chambers <- read.csv(file.path(pkg, "metadata", "autochamber_chambers.csv"))
roster <- bind_rows(units %>% transmute(method = "fluxbot", stand = stand_code, id = paste0("fluxbot", unit)),
                    chambers %>% transmute(method = "autochamber", stand = stand_code, id = paste0("autochamber", chamber)))
d <- readRDS(file.path(out_dir, "dataset_main.rds")) %>% filter(hour_of_obs >= p0, hour_of_obs < p1) %>%
  mutate(id = as.character(id), stand = as.character(stand), method = as.character(method)) %>% distinct(method, stand, id, hour_of_obs)
grid <- roster %>% tidyr::crossing(hour_of_obs = hours) %>%
  left_join(d %>% mutate(ok = TRUE), by = c("method", "stand", "id", "hour_of_obs")) %>% mutate(ok = coalesce(ok, FALSE))
unit_rate <- grid %>% group_by(method, stand, id) %>% summarise(success = mean(ok), .groups = "drop")
write.csv(unit_rate, file.path(out_dir, "unit_success.csv"), row.names = FALSE)
sh <- grid %>% group_by(method, stand, hour_of_obs) %>% summarise(n_ok = sum(ok), n_units = n(), .groups = "drop")

pb_prob_lt <- function(p, k) {          # P(fewer than k successes), Poisson-binomial by recursion
  dist <- 1; for (pi in p) dist <- c(dist * (1 - pi), 0) + c(0, dist * pi); sum(dist[seq_len(k)])
}
res <- list(); curves <- list()
for (m in c("fluxbot", "autochamber")) for (st in c("healthy", "unhealthy")) {
  x <- sh %>% filter(method == m, stand == st); p <- unit_rate$success[unit_rate$method == m & unit_rate$stand == st]
  r <- rle(x$n_ok[order(x$hour_of_obs)] == 0)
  res[[paste(m, st)]] <- tibble(method = m, stand = st, n_units = length(p), mean_unit_success = mean(p),
    cov1 = mean(x$n_ok >= 1), cov3 = mean(x$n_ok >= 3), outage_hours = sum(x$n_ok == 0),
    longest_outage_h = if (any(r$values)) max(r$lengths[r$values]) else 0,
    exp_outage_frac = pb_prob_lt(p, 1), obs_outage_frac = mean(x$n_ok == 0),
    exp_lt3_frac = pb_prob_lt(p, 3), obs_lt3_frac = mean(x$n_ok < 3))
  curves[[paste(m, st)]] <- tibble(method = m, stand = st, k = 1:max(length(p), 1),
    observed = sapply(1:length(p), function(k) mean(x$n_ok >= k)),
    independent = sapply(1:length(p), function(k) 1 - pb_prob_lt(p, k)))
}
res <- bind_rows(res); curves <- bind_rows(curves)
write.csv(res, file.path(out_dir, "resilience_summary.csv"), row.names = FALSE)
print(as.data.frame(res), digits = 3)
for (i in seq_len(nrow(res))) for (k in setdiff(names(res), c("method", "stand")))
  record(sprintf("resil_%s_%s_%s", res$method[i], res$stand[i], k), res[[k]][i], "resilience")
for (m in c("fluxbot", "autochamber")) record(paste0("unit_success_median_", m), median(unit_rate$success[unit_rate$method == m]), "resilience")

# ---- figure ------------------------------------------------------------------------------------
source("afm_revision/fig_style.R")
pal <- pal_sys
stl <- c(healthy = "Stand 1", unhealthy = "Stand 2")
pa <- ggplot(unit_rate, aes(lab_sys[method], 100 * success, colour = method)) +
  geom_jitter(width = 0.12, height = 0, size = 1.4, alpha = 0.8) +
  stat_summary(fun = median, geom = "crossbar", width = 0.4, colour = "black", linewidth = 0.3) +
  scale_colour_manual(values = pal, guide = "none") + labs(x = NULL, y = "Hours with a retained\nmeasurement (% per unit)") +
  theme_afm()
pb <- ggplot(curves %>% mutate(stand = stl[stand]), aes(k, 100 * observed, colour = method)) +
  geom_line(aes(y = 100 * independent), linetype = "22", linewidth = 0.4) + geom_line(linewidth = 0.6) + geom_point(size = 1) +
  facet_wrap(~stand) + scale_colour_manual(values = pal, labels = lab_sys, name = NULL) +
  scale_x_continuous(breaks = 1:8) + labs(x = "Units reporting (at least k)", y = "Stand-hours (%)") +
  theme_afm() + theme(legend.position = "bottom", strip.background = element_blank())
pc <- ggplot(sh %>% mutate(row = paste(lab_sys[method], stl[stand]), frac = n_ok / n_units),
             aes(hour_of_obs, row, fill = n_ok)) + geom_tile() +
  scale_fill_viridis_c(name = "Units\nreporting", option = "D") + labs(x = NULL, y = NULL) +
  scale_x_datetime(date_labels = "%d %b", expand = c(0, 0)) + theme_afm()
fig <- (pa + pb + plot_layout(widths = c(1, 2.2))) / pc + plot_layout(heights = c(1.3, 1)) + tags_afm()
save_afm(fig, "FigS_resilience", 190, 120, tif = FALSE)
write_numbers("numbers_resilience.csv")

# ---- Fig. 10: measurement success and array coverage ------------------------------------------------
# a: per-unit measurement success (share of scheduled hours with a retained measurement);
# b: by system; c: daily share of each stand's units with a retained measurement;
# d: hourly state of each stand array: >= 3 units retained (replicated), 1-2 units, units measured
#    but all removed by QC (wet sensor / failures), or no data (down).
valid_ds <- build_dataset(qc = "valid") %>% filter(hour_of_obs >= p0, hour_of_obs < p1) %>%
  mutate(stand = as.character(stand), method = as.character(method)) %>% count(method, stand, hour_of_obs, name = "n_valid")
st_h <- sh %>% left_join(valid_ds, by = c("method", "stand", "hour_of_obs")) %>% mutate(n_valid = coalesce(n_valid, 0L),
  state = case_when(n_ok >= 3 ~ ">= 3 units", n_ok >= 1 ~ "1-2 units",
                    n_valid >= 1 ~ "measured, removed by QC", TRUE ~ "no data (down)"),
  state = factor(state, levels = c(">= 3 units", "1-2 units", "measured, removed by QC", "no data (down)")),
  series = factor(paste(lab_sys[method], stl[stand]), levels = c("Autochamber Stand 1", "Autochamber Stand 2", "Fluxbot 2.0 Stand 1", "Fluxbot 2.0 Stand 2")))
state_tab <- st_h %>% count(series, state) %>% group_by(series) %>% mutate(pct = 100 * n / sum(n)) %>% ungroup()
write.csv(state_tab, file.path(out_dir, "fig10_state_shares.csv"), row.names = FALSE)
for (i in seq_len(nrow(state_tab))) record(sprintf("state_%s_%s", gsub(" ", "_", state_tab$series[i]),
  c("rep3", "u12", "qc", "down")[as.integer(state_tab$state[i])]), state_tab$pct[i], "resilience", "% of stand-hours")
# colours follow the system palette used in all figures (autochamber green, Fluxbot grey):
# dark shade + solid = stand 1, light shade + dashed = stand 2
ser_pal <- pal_stand
ser_lty <- c("Autochamber Stand 1" = "solid", "Autochamber Stand 2" = "22", "Fluxbot 2.0 Stand 1" = "solid", "Fluxbot 2.0 Stand 2" = "22")
ur <- unit_rate %>% mutate(unit = reorder(sub("^(autochamber|fluxbot|fluxes_bot)", "", id), success))
pa <- ggplot(ur, aes(unit, 100 * success, fill = method)) + geom_col(width = 0.8) +
  facet_grid(~ lab_sys[method], scales = "free_x", space = "free_x") + scale_fill_manual(values = pal, guide = "none") +
  labs(x = "Chamber or unit", y = "Measurement success\n(% of intended closures)") + scale_y_continuous(limits = c(0, 100)) + theme_afm() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6))
pb <- ggplot(ur, aes(c(autochamber = "Auto-\nchamber", fluxbot = "Fluxbot\n2.0")[method], 100 * success, fill = method)) + geom_boxplot(outliers = FALSE, width = 0.6, alpha = a_mean) +
  geom_jitter(width = 0.1, size = pt_mean, alpha = a_mean) + scale_fill_manual(values = pal, guide = "none") +
  scale_y_continuous(limits = c(0, 100)) + labs(x = NULL, y = NULL) + theme_afm()
daily <- st_h %>% mutate(day = as.Date(hour_of_obs, tz = "America/New_York")) %>% group_by(series, day) %>%
  summarise(share = 100 * mean(n_ok / n_units), .groups = "drop")
pc <- ggplot(daily, aes(day, share, colour = series, linetype = series)) + geom_line(linewidth = 0.7) +
  scale_colour_manual(values = ser_pal, name = NULL) + scale_linetype_manual(values = ser_lty, name = NULL) +
  scale_y_continuous(limits = c(0, 100)) + scale_x_date(date_labels = "%d %b", expand = c(0, 0)) +
  labs(x = NULL, y = "Units reporting\n(% of stand's units, daily)") +
  theme_afm() + theme(legend.position = "top", legend.key.width = unit(8, "mm"))
# timeline: the same state colours for both systems, from good (blue, >= 3 units) to bad
# (red, no data)
st_h <- st_h %>% mutate(fill_key = as.character(state))
# good-to-bad diverging scale (ColorBrewer RdYlBu, colourblind-safe; avoids the system greens/greys)
fill_pal <- pal_state
rows <- tibble(series = factor(levels(st_h$series), levels = levels(st_h$series)))
pd <- ggplot(st_h, aes(hour_of_obs, forcats::fct_rev(series))) +
  geom_tile(aes(fill = fill_key), height = 0.8) +
  geom_tile(data = rows, aes(x = p0 + (p1 - p0) / 2, y = forcats::fct_rev(series)), width = as.numeric(difftime(p1, p0, units = "secs")),
            height = 0.8, fill = NA, colour = "grey60", linewidth = 0.3, inherit.aes = FALSE) +
  scale_fill_manual(values = fill_pal, breaks = names(fill_pal), name = NULL) +
  guides(fill = guide_legend(nrow = 1, override.aes = list(colour = "grey60", linewidth = 0.3))) +
  scale_x_datetime(date_labels = "%d %b", expand = c(0, 0)) + labs(x = NULL, y = NULL) +
  theme_afm() + theme(legend.position = "bottom", axis.line.y = element_blank(), axis.ticks.y = element_blank())
fig <- ((pa + pb + plot_layout(widths = c(4, 1))) / pc / pd) + plot_layout(heights = c(1, 0.9, 0.55)) + tags_afm()
save_afm(fig, "Fig9_uptime_coverage", 190, 175)
write_numbers("numbers_resilience.csv")
