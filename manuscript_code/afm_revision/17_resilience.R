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
pal <- c(autochamber = "#3B8F63", fluxbot = "#8C8C8C")
lab_sys <- c(autochamber = "Autochamber", fluxbot = "Fluxbot 2.0")
stl <- c(healthy = "Stand 1", unhealthy = "Stand 2")
pa <- ggplot(unit_rate, aes(lab_sys[method], 100 * success, colour = method)) +
  geom_jitter(width = 0.12, height = 0, size = 1.4, alpha = 0.8) +
  stat_summary(fun = median, geom = "crossbar", width = 0.4, colour = "black", linewidth = 0.3) +
  scale_colour_manual(values = pal, guide = "none") + labs(x = NULL, y = "Hours with a retained\nmeasurement (% per unit)") +
  theme_classic(base_size = 8)
pb <- ggplot(curves %>% mutate(stand = stl[stand]), aes(k, 100 * observed, colour = method)) +
  geom_line(aes(y = 100 * independent), linetype = "22", linewidth = 0.4) + geom_line(linewidth = 0.6) + geom_point(size = 1) +
  facet_wrap(~stand) + scale_colour_manual(values = pal, labels = lab_sys, name = NULL) +
  scale_x_continuous(breaks = 1:8) + labs(x = "Units reporting (at least k)", y = "Stand-hours (%)") +
  theme_classic(base_size = 8) + theme(legend.position = "bottom", strip.background = element_blank())
pc <- ggplot(sh %>% mutate(row = paste(lab_sys[method], stl[stand]), frac = n_ok / n_units),
             aes(hour_of_obs, row, fill = n_ok)) + geom_tile() +
  scale_fill_viridis_c(name = "Units\nreporting", option = "D") + labs(x = NULL, y = NULL) +
  scale_x_datetime(date_labels = "%d %b", expand = c(0, 0)) + theme_classic(base_size = 8)
fig <- (pa + pb + plot_layout(widths = c(1, 2.2))) / pc + plot_layout(heights = c(1.3, 1)) + plot_annotation(tag_levels = "a")
ggsave(file.path(out_dir, "figures", "Fig_resilience.pdf"), fig, width = 190, height = 120, units = "mm", device = cairo_pdf)
ggsave(file.path(out_dir, "figures", "Fig_resilience.png"), fig, width = 190, height = 120, units = "mm", dpi = 300, device = ragg::agg_png)
write_numbers("numbers_resilience.csv")
