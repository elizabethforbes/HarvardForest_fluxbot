# Fig. 6. Data flow and coverage, 2-31 October 2023.
#  (a) Sankey of closures (intended, recorded, as deployed = passed QC, RH-screened = main dataset) for each
#      system, with losses at each step (2_analysis/07_measurement_accounting.R; finer steps in SI Section S2.5)
#  (b) measurement success of each unit (share of hours with a flux in the main dataset), (c) by system
#  (d) units with a measurement in the main dataset each hour, per array, with the share of hours with >= 3 units (2_analysis/13_resilience.R)
# Agreement of system means at selected data-cleaning steps is in SI Table S2b.
source("R/setup.R")
source("R/fig_style.R")
suppressPackageStartupMessages({ library(ggplot2); library(patchwork) })
set.seed(20260930)
pal <- pal_sys
stl <- c(healthy = "Stand 1", unhealthy = "Stand 2")
p0 <- as.POSIXct("2023-10-02", tz = "America/New_York"); p1 <- as.POSIXct("2023-11-01", tz = "America/New_York")

acc <- read.csv(file.path(out_dir, "measurement_accounting.csv"))

# ---- Fig. 6a: Sankey diagram of closures at each stage -------------------------------
# Main stream (top-aligned) narrows at each stage; each loss peels off downward as a ribbon to a labelled
# terminal, coloured by reason. Heights are % of intended closures, so the two systems are comparable.
# Loss colours: reds = no data (down), oranges = measured but removed by QC,
# blue = wet sensor (RH-screened subset only). Loss ribbons are drawn at least 1% of intended wide so that
# small steps stay visible; labels give the true counts.
sig <- function(x0, x1, ya0, ya1, yb0, yb1, n = 40) {   # ribbon from [ya0,ya1] at x0 to [yb0,yb1] at x1
  t <- seq(0, 1, length.out = n); s <- 3 * t^2 - 2 * t^3; x <- x0 + (x1 - x0) * t
  tibble(x = c(x, rev(x)), y = c(ya1 + (yb1 - ya1) * s, rev(ya0 + (yb0 - ya0) * s)))
}
reason <- c(recorded = "No data (logger, power or transmission down)", retained = "Removed by QC (chamber failure, spike, too few records)",
            `RH-screened` = "Removed by QC: wet sensor")
rcol <- setNames(c(unname(pal_state["no data (down)"]), unname(pal_state["measured, removed by QC"]), col_wet), reason)
sys_lab <- c(fluxbot = "Fluxbot 2.0 (16 units)", autochamber = "Autochamber (12 chambers)")
stage_lab <- c(intended = "Intended", recorded = "Recorded", retained = "As deployed", `RH-screened` = "RH-screened (main)")
w <- 0.12
sk <- lapply(c("autochamber", "fluxbot"), function(sy) {
  a <- acc %>% filter(system == sy, stage %in% names(stage_lab), !(sy == "autochamber" & stage == "RH-screened")) %>%
    mutate(f = pct_of_intended / 100, x = row_number(), lost = lag(n) - n)
  nodes <- a %>% transmute(system = sy, x, stage, n, f, type = "node")
  flows <- bind_rows(lapply(seq_len(nrow(a) - 1), function(k) {
    f0 <- a$f[k]; f1 <- a$f[k + 1]; l <- f0 - f1; ld <- if (l > 0) max(l, 0.01) else 0   # drawn width
    keep <- sig(a$x[k] + w / 2, a$x[k + 1] - w / 2, 1 - f1, 1, 1 - f1, 1) %>% mutate(part = "keep")
    lossy <- -0.08 - 0.42 * (k - 1) / 3
    loss <- if (l > 0) sig(a$x[k] + w / 2, a$x[k] + 0.62, 1 - f1 - ld, 1 - f1, lossy - ld, lossy) %>% mutate(part = "loss") else NULL
    bind_rows(keep, loss) %>% mutate(system = sy, k = k, id = paste(sy, k, part), reason = reason[as.character(a$stage[k + 1])],
                                     lost = a$lost[k + 1], lx = a$x[k] + 0.64, ly = lossy - ld / 2)
  }))
  list(nodes = nodes, flows = flows)
})
nodes <- bind_rows(lapply(sk, `[[`, "nodes")) %>% mutate(system = factor(sys_lab[system], levels = sys_lab))
flows <- bind_rows(lapply(sk, `[[`, "flows")) %>% mutate(system = factor(sys_lab[system], levels = sys_lab))
scol <- c("Autochamber (12 chambers)" = unname(pal_sys["autochamber"]), "Fluxbot 2.0 (16 units)" = unname(pal_sys["fluxbot"]))
lossl <- flows %>% filter(part == "loss") %>% distinct(system, id, lx, ly, lost, reason)
p_sk <- ggplot() +
  geom_polygon(data = flows %>% filter(part == "keep"), aes(x, y, group = id, fill = system), alpha = 0.35) +
  geom_polygon(data = flows %>% filter(part == "loss"), aes(x, y, group = id), fill = rcol[flows$reason[flows$part == "loss"]], alpha = 0.6) +
  geom_rect(data = nodes, aes(xmin = x - w / 2, xmax = x + w / 2, ymin = 1 - f, ymax = 1, fill = system)) +
  geom_text(data = nodes, aes(x = x, y = 1.03, label = sprintf("%s\n%s\n(%.0f%%)", stage_lab[as.character(stage)], format(n, big.mark = ","), 100 * f)),
            vjust = 0, size = 2.5, lineheight = 0.9) +
  geom_text(data = lossl, aes(x = lx, y = ly, label = paste0("\u2212", trimws(format(lost, big.mark = ",")))), hjust = 0, size = 2.4, colour = "grey20") +
  facet_wrap(~ system, ncol = 1) + scale_fill_manual(values = scol, guide = "none") +
  scale_x_continuous(limits = c(0.8, 4.9)) + scale_y_continuous(limits = c(-0.62, 1.6)) + coord_cartesian(clip = "off") +
  theme_void(base_size = 8) + theme(strip.text = element_text(face = "bold", size = 8, hjust = 0.02, margin = margin(2, 0, 2, 0)), plot.tag = element_text(face = "bold", size = 10))
key <- tibble(reason = factor(names(rcol), levels = names(rcol)), y = rev(seq_along(rcol)))
p_key <- ggplot(key, aes(0, y)) + geom_tile(aes(fill = reason), width = 0.25, height = 0.6, alpha = 0.6) +
  geom_text(aes(x = 0.22, label = reason), hjust = 0, size = 2.4) +
  scale_fill_manual(values = rcol, guide = "none") + scale_x_continuous(limits = c(-0.2, 4)) + scale_y_continuous(limits = c(0.5, 4.2)) +
  annotate("text", x = -0.13, y = 4, label = "Closures lost at each step", hjust = 0, size = 2.6, fontface = "bold") + theme_void(base_size = 8)

# ---- (b-d) unit success and daily coverage --------------------------------------------------------------
unit_rate <- read.csv(file.path(out_dir, "unit_success.csv"))
st_h <- read.csv(file.path(out_dir, "stand_hour_states.csv")) %>%
  mutate(hour_of_obs = with_tz(as.POSIXct(hour_of_obs, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), "America/New_York"),
         series = factor(series, levels = c("Autochamber Stand 1", "Autochamber Stand 2", "Fluxbot 2.0 Stand 1", "Fluxbot 2.0 Stand 2")))

ur <- unit_rate %>% mutate(unit = reorder(sub("^(autochamber|fluxbot|fluxes_bot)", "", id), success))
p_unit <- ggplot(ur, aes(unit, 100 * success, fill = method)) + geom_col(width = 0.8) +
  facet_grid(~ lab_sys[method], scales = "free_x", space = "free_x") + scale_fill_manual(values = pal, guide = "none") +
  labs(x = "Chamber or unit", y = "Measurement success\n(% of hours)") + scale_y_continuous(limits = c(0, 100)) + theme_afm() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6))
p_box <- ggplot(ur, aes(c(autochamber = "Auto-\nchamber", fluxbot = "Fluxbot\n2.0")[method], 100 * success, fill = method)) + geom_boxplot(outliers = FALSE, width = 0.6, alpha = a_mean) +
  geom_jitter(width = 0.1, size = pt_mean, alpha = a_mean) + scale_fill_manual(values = pal, guide = "none") +
  scale_y_continuous(limits = c(0, 100)) + labs(x = NULL, y = NULL) + theme_afm()
# (d) units with a measurement in the main dataset each hour, per array; dashed line = 3 units (replicated
#     coverage); right: share of hours with >= 3 units
cov <- st_h %>% mutate(arr = factor(sub(" Stand", ", Stand", as.character(series)),
                                    levels = c("Autochamber, Stand 1", "Autochamber, Stand 2", "Fluxbot 2.0, Stand 1", "Fluxbot 2.0, Stand 2")))
cov_pct <- cov %>% group_by(arr) %>% summarise(p3 = 100 * mean(n_ok >= 3), .groups = "drop")
p_daily <- ggplot(cov, aes(hour_of_obs, n_ok, colour = method)) +
  geom_hline(yintercept = 3, linetype = "22", colour = "grey50", linewidth = 0.3) + geom_step(linewidth = 0.35) +
  geom_text(data = cov_pct, aes(x = p1 + 6 * 3600, y = 4, label = sprintf("%.0f%%", p3)), inherit.aes = FALSE, hjust = 0, size = 2.5) +
  facet_wrap(~ arr, ncol = 1, strip.position = "left") + scale_colour_manual(values = pal, guide = "none") +
  scale_y_continuous(breaks = c(0, 3, 6), limits = c(0, 8)) + scale_x_datetime(date_labels = "%d %b", expand = c(0, 0)) +
  coord_cartesian(clip = "off") + labs(x = NULL, y = "Units with a measurement") +
  theme_afm() + theme(strip.placement = "outside", strip.text.y.left = element_text(angle = 0, hjust = 1, size = 6.5, face = "bold"),
                      plot.margin = margin(5, 28, 5, 5))

top <- p_sk + inset_element(p_key, left = 0.58, bottom = 0.02, right = 1, top = 0.3)
fig <- wrap_elements(full = top) / (p_unit + p_box + plot_layout(widths = c(4, 1))) / p_daily +
  plot_layout(heights = c(1.25, 0.85, 0.9)) + plot_annotation(tag_levels = "a")
save_afm(fig, "Fig6_data_flow", 190, 230)
