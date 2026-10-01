# Fig. 8. Measurement flow: closures intended, recorded, flux computed, valid, retained (as deployed) and RH-screened for
# each system (Sankey), and agreement of array means at each stage. Tables from
# 2_analysis/08_measurement_accounting.R.
source("R/setup.R")
source("R/fig_style.R")
suppressPackageStartupMessages({ library(ggplot2); library(patchwork) })
acc <- read.csv(file.path(out_dir, "measurement_accounting.csv"))
ag <- read.csv(file.path(out_dir, "agreement_by_stage.csv"))

# ---- Fig 8: Sankey diagram of closures, and agreement at each stage -------------------------------
# Main stream (top-aligned) narrows at each stage; each loss peels off downward as a ribbon to a labelled
# terminal, coloured by reason. Heights are % of intended closures, so the two systems are comparable.
# Loss colours match the hourly states in Fig. 9: reds = no data (down), oranges = measured but removed by QC,
# blue = wet sensor (RH-screened subset only). Loss ribbons are drawn at least 1% of intended wide so that
# small steps stay visible; labels give the true counts.
sig <- function(x0, x1, ya0, ya1, yb0, yb1, n = 40) {   # ribbon from [ya0,ya1] at x0 to [yb0,yb1] at x1
  t <- seq(0, 1, length.out = n); s <- 3 * t^2 - 2 * t^3; x <- x0 + (x1 - x0) * t
  tibble(x = c(x, rev(x)), y = c(ya1 + (yb1 - ya1) * s, rev(ya0 + (yb0 - ya0) * s)))
}
reason <- c(recorded = "No data: no record\n(logger, power, transmission)", computed = "No data: too few records\nto fit a flux",
            valid = "Removed by QC: failed fit\nor chamber check", retained = "Removed by QC: spike",
            `RH-screened` = "Wet sensor\n(RH-screened subset only)")
rcol <- setNames(c(unname(pal_state["no data (down)"]), "#F4A09A", unname(pal_state["measured, removed by QC"]), "#FDD9A8", col_wet), reason)
sys_lab <- c(fluxbot = "Fluxbot 2.0 (16 units)", autochamber = "Autochamber (12 chambers)")
stage_lab <- c(intended = "Intended", recorded = "Recorded", computed = "Flux\ncomputed", valid = "Valid", retained = "Retained\n(as deployed)", `RH-screened` = "RH-screened")
w <- 0.12
sk <- lapply(c("autochamber", "fluxbot"), function(sy) {
  a <- acc %>% filter(system == sy, !(sy == "autochamber" & stage == "RH-screened")) %>% mutate(f = pct_of_intended / 100, x = row_number())
  nodes <- a %>% transmute(system = sy, x, stage, n, f, type = "node")
  flows <- bind_rows(lapply(seq_len(nrow(a) - 1), function(k) {
    f0 <- a$f[k]; f1 <- a$f[k + 1]; l <- f0 - f1; ld <- if (l > 0) max(l, 0.01) else 0   # drawn width
    keep <- sig(a$x[k] + w / 2, a$x[k + 1] - w / 2, 1 - f1, 1, 1 - f1, 1) %>% mutate(part = "keep")
    lossy <- -0.08 - 0.42 * (k - 1) / 5
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
pa <- ggplot() +
  geom_polygon(data = flows %>% filter(part == "keep"), aes(x, y, group = id, fill = system), alpha = 0.35) +
  geom_polygon(data = flows %>% filter(part == "loss"), aes(x, y, group = id), fill = rcol[flows$reason[flows$part == "loss"]], alpha = 0.9) +
  geom_rect(data = nodes, aes(xmin = x - w / 2, xmax = x + w / 2, ymin = 1 - f, ymax = 1, fill = system)) +
  geom_text(data = nodes, aes(x = x, y = 1.03, label = sprintf("%s\n%s\n(%.0f%%)", stage_lab[as.character(stage)], format(n, big.mark = ","), 100 * f)),
            vjust = 0, size = 2.5, lineheight = 0.9) +
  geom_text(data = lossl, aes(x = lx, y = ly, label = paste0("\u2212", trimws(format(lost, big.mark = ",")))), hjust = 0, size = 2.4, colour = "grey20") +
  facet_wrap(~ system, ncol = 1) + scale_fill_manual(values = scol, guide = "none") +
  scale_x_continuous(limits = c(0.8, 6.9)) + scale_y_continuous(limits = c(-0.62, 1.32)) +
  theme_void(base_size = 8) + theme(strip.text = element_text(face = "bold", size = 8, hjust = 0.02, margin = margin(2, 0, 2, 0)), plot.tag = element_text(face = "bold", size = 10))
key <- tibble(reason = names(rcol), y = rev(seq_along(rcol)))
pk <- ggplot(key, aes(0, y)) + geom_tile(aes(fill = reason), width = 0.4, height = 0.7) + geom_text(aes(x = 0.35, label = reason), hjust = 0, size = 2.4, lineheight = 0.9) +
  scale_fill_manual(values = rcol, guide = "none") + xlim(-0.3, 4) + theme_void(base_size = 8) + labs(title = "Closures lost at each step") +
  theme(plot.title = element_text(size = 8, face = "bold"))
# (b) heatmap: each cell coloured by its value (offset: diverging, 0 = white; r: sequential)
col_off <- scales::col_numeric(c(unname(pal_div["low"]), "white", unname(pal_div["high"])), domain = c(-15, 15))
col_r <- scales::col_numeric(c("white", unname(pal_div["high"])), domain = c(0.5, 1))
agt <- ag %>% mutate(stage = factor(stage, levels = rev(c("valid", "retained", "RH-screened")), labels = rev(c("Valid", "Retained\n(as deployed)", "RH-screened\nsubset")))) %>%
  select(stage, offset_pct, r_hourly, r_daily) %>% tidyr::pivot_longer(-stage) %>%
  mutate(fill = if_else(name == "offset_pct", col_off(pmax(pmin(value, 15), -15)), col_r(pmax(value, 0.5))),
         label = if_else(name == "offset_pct", sprintf("%+.1f%%", value), sprintf("%.2f", value)),
         name = factor(name, levels = c("offset_pct", "r_hourly", "r_daily"), labels = c("Offset", "r, hourly", "r, daily")))
pb <- ggplot(agt, aes(name, stage)) + geom_tile(aes(fill = fill), colour = "white", linewidth = 0.8) + geom_text(aes(label = label), size = 2.6) +
  scale_fill_identity() + scale_x_discrete(position = "top", expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) +
  labs(x = NULL, y = NULL, title = "Agreement of array means at each stage") + theme_minimal(base_size = 8) +
  theme(panel.grid = element_blank(), plot.title = element_text(size = 8, face = "bold"), axis.text = element_text(colour = "black", size = 7), plot.tag = element_text(face = "bold", size = 10))
pflow <- (pa | (pk / pb / plot_spacer() + plot_layout(heights = c(0.7, 0.9, 0.6)))) + plot_layout(widths = c(1.9, 1)) + plot_annotation(tag_levels = list(c("a", "", "b"))) & theme(plot.tag = element_text(face = "bold", size = 10))
save_afm(pflow, "Fig8_measurement_flow", 190, 130)
