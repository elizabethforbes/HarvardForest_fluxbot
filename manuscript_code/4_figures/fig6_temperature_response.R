# Fig. 6. Temperature response (exponential fits, Q10) over common stand-hours, as deployed.
source("R/setup.R")
source("R/fig_style.R")
suppressPackageStartupMessages({ library(ggplot2); library(patchwork) })
pal <- pal_sys
save_fig <- function(p, name, width_mm, height_mm) save_afm(p, name, width_mm, height_mm)
d <- load_chamber_hours("deployed") %>%
  mutate(stand_label = factor(if_else(stand == "healthy", "Stand 1", "Stand 2")),
         method_label = factor(lab_sys[as.character(method)], levels = lab_sys))
num <- get_number
set.seed(20260930)

# ---- Fig 6: temperature response (common stand-hours) -------------------------------------
common <- d %>% distinct(stand, hour_of_obs, method) %>% count(stand, hour_of_obs) %>% filter(n == 2)
dq <- d %>% semi_join(common, by = c("stand", "hour_of_obs")) %>% filter(!is.na(s10t), fluxL_umolm2sec > 0)
tseq <- seq(min(dq$s10t), max(dq$s10t), length.out = 100)
pred <- bind_rows(lapply(split(dq, dq$method), function(x) {
  m <- nls(fluxL_umolm2sec ~ a * exp(b * s10t), data = x, start = list(a = 1, b = 0.1))
  p <- coef(m); J <- cbind(exp(p["b"] * tseq), p["a"] * tseq * exp(p["b"] * tseq))
  se <- sqrt(rowSums((J %*% vcov(m)) * J))
  data.frame(method = x$method[1], s10t = tseq, f = p["a"] * exp(p["b"] * tseq), se = se)
})) %>% mutate(method_label = factor(lab_sys[as.character(method)], levels = lab_sys))
qlab <- data.frame(method_label = factor(lab_sys, levels = lab_sys),
                   lab = c(sprintf("Q[10] == %.2f ~ (%.2f-%.2f)", num("q10_autochamber_q10"), num("q10_autochamber_lo"), num("q10_autochamber_hi")),
                           sprintf("Q[10] == %.2f ~ (%.2f-%.2f)", num("q10_fluxbot_q10"), num("q10_fluxbot_lo"), num("q10_fluxbot_hi"))))
p8 <- ggplot(dq, aes(s10t, fluxL_umolm2sec)) +
  geom_point(aes(colour = method), size = pt_dense, alpha = a_dense, stroke = 0) +
  geom_ribbon(data = pred, aes(y = f, ymin = f - 1.96 * se, ymax = f + 1.96 * se), alpha = 0.3, fill = col_fit) +
  geom_line(data = pred, aes(y = f), linewidth = 0.6) +
  geom_text(data = qlab, aes(x = -Inf, y = Inf, label = lab), parse = TRUE, hjust = -0.05, vjust = 1.3, size = 2.5) +
  facet_wrap(~method_label) + scale_colour_manual(values = pal, guide = "none") +
  labs(x = "Soil temperature at 10 cm, HF001 (\u00b0C)", y = flux_lab)
save_fig(p8, "Fig6_temperature_response", 140, 70)
