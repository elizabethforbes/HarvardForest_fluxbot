# Fig. 3. Agreement of hourly and daily system means (scatter with 1:1 line and OLS fit; Bland-Altman), main dataset
# (RH-screened, goFlux best model).
source("R/setup.R")
source("R/fig_style.R")
suppressPackageStartupMessages({ library(ggplot2); library(patchwork) })
pal <- pal_sys
save_fig <- function(p, name, width_mm, height_mm) save_afm(p, name, width_mm, height_mm)
d <- load_chamber_hours()

# ---- Fig. 3: array-level agreement --------------------------------------------------------
# hourly system means (mean of the two stand, i.e. array, means) in compared hours (>= 3 units of each
# system per stand), main dataset. Outlined points: daily means (>= 12 compared hours).
# (b) Bland-Altman plot of the same hourly means: mean difference and 95% limits of agreement.
arr <- function(dd) { hrs <- matched_hours(dd, 3)
  dd %>% filter(hour_of_obs %in% hrs) %>% group_by(hour_of_obs, stand, method) %>% summarise(f = mean(flux_umolm2sec), .groups = "drop") %>%
    group_by(hour_of_obs, method) %>% summarise(f = mean(f), .groups = "drop") %>% pivot_wider(names_from = method, values_from = f) }
hd <- arr(d) %>% mutate(subset = "Hourly means")
dy <- hd %>% mutate(day = as.Date(hour_of_obs, tz = "America/New_York")) %>% group_by(day) %>% filter(n() >= 12) %>%
  summarise(autochamber = mean(autochamber), fluxbot = mean(fluxbot))
stt <- function(a, f) c(sprintf("%.2f", cor(a, f)), sprintf("%+.2f", mean(f - a)), sprintf("%.2f", epiR::epi.ccc(a, f)$rho.c$est),
                        sprintf("%.0f%%", 100 * sqrt(mean((f - a)^2)) / mean(a)))
tabd <- rbind(stt(hd$autochamber, hd$fluxbot), stt(dy$autochamber, dy$fluxbot))
dimnames(tabd) <- list(c("Hourly", "Daily"), c("r", "Offset", "CCC", "nRMSE"))
tg <- gridExtra::tableGrob(tabd, theme = gridExtra::ttheme_minimal(base_size = 6.5, padding = unit(c(3, 1.6), "mm"),
        core = list(fg_params = list(hjust = 1, x = 0.9)), rowhead = list(fg_params = list(hjust = 0, x = 0.05, fontface = "plain")),
        colhead = list(fg_params = list(fontface = "bold"))))
cols4 <- c("Hourly means" = unname(pal["fluxbot"]))
ols <- coef(lm(fluxbot ~ autochamber, data = hd))   # OLS fit of hourly means
lim <- c(0.8, 5)
lgd <- theme(legend.position = c(0.02, 0.98), legend.justification = c(0, 1), legend.title = element_blank(),
             legend.background = element_rect(fill = alpha("white", 0.8), colour = NA), legend.key.size = unit(3, "mm"),
             legend.spacing.y = unit(0, "mm"), legend.margin = margin(1, 2, 1, 2))
p4a <- ggplot(hd, aes(autochamber, fluxbot)) +
  geom_abline(aes(intercept = 0, slope = 1, linetype = "1:1"), colour = col_ref, linewidth = 0.4) +
  geom_abline(aes(intercept = ols[1], slope = ols[2], linetype = "OLS fit (hourly)"), colour = "black", linewidth = 0.5) +
  geom_point(aes(colour = subset), size = pt_mean, alpha = a_mean, stroke = 0) +
  geom_point(data = dy, aes(fill = "Daily means"), shape = 21, size = pt_big, colour = "black", stroke = 0.35) +
  scale_colour_manual(values = cols4) + scale_fill_manual(values = c("Daily means" = col_accent)) +
  scale_linetype_manual(values = c("1:1" = "22", "OLS fit (hourly)" = "solid")) +
  guides(colour = guide_legend(order = 1, override.aes = list(size = 2, alpha = 1)), fill = guide_legend(order = 2), linetype = guide_legend(order = 3)) +
  coord_equal(xlim = lim, ylim = lim, expand = FALSE) + lgd +
  labs(x = expression(Autochamber ~ system ~ mean ~ (mu * mol ~ m^-2 ~ s^-1)), y = expression(Fluxbot ~ system ~ mean ~ (mu * mol ~ m^-2 ~ s^-1)), tag = "a") +
  inset_element(tg, left = 0.42, bottom = 0.02, right = 0.99, top = 0.21, align_to = "panel", ignore_tag = TRUE)
ba <- hd %>% mutate(m = (autochamber + fluxbot) / 2, df = fluxbot - autochamber)
mu <- mean(ba$df); lo <- mu - 1.96 * sd(ba$df); hi <- mu + 1.96 * sd(ba$df)
p4b <- ggplot(ba, aes(m, df)) + geom_hline(yintercept = 0, colour = "grey75") +
  geom_point(aes(colour = subset), size = pt_mean, alpha = a_mean, stroke = 0) +
  geom_hline(aes(yintercept = mu, linetype = "Mean difference"), linewidth = 0.5) +
  geom_hline(aes(yintercept = lo, linetype = "95% limits of agreement"), linewidth = 0.4) + geom_hline(aes(yintercept = hi, linetype = "95% limits of agreement"), linewidth = 0.4) +
  annotate("text", x = 5.2, y = c(mu, lo, hi), label = sprintf("%.2f", c(mu, lo, hi)), hjust = 1, vjust = -0.4, size = txt) +
  scale_colour_manual(values = cols4) + scale_linetype_manual(values = c("Mean difference" = "solid", "95% limits of agreement" = "22"), breaks = c("Mean difference", "95% limits of agreement")) +
  guides(colour = guide_legend(order = 1, override.aes = list(size = 2, alpha = 1)), linetype = guide_legend(order = 2)) +
  coord_equal(xlim = c(0.5, 5.3), ylim = c(-2.4, 2.4), expand = FALSE) + lgd +   # same scale on both axes
  labs(x = expression(Mean ~ of ~ the ~ two ~ systems ~ (mu * mol ~ m^-2 ~ s^-1)), y = expression(Fluxbot - autochamber ~ (mu * mol ~ m^-2 ~ s^-1)), tag = "b")
save_fig((p4a | p4b), "Fig3_array_agreement", 190, 100)
