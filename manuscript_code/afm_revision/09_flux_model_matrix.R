# Fluxbot vs autochamber offset under each combination of flux model (linear, quadratic,
# HF-published best 1-min window); curvature = quadratic / linear initial slope.

source("afm_revision/00_prep.R")
fb <- load_fluxbot("fluxQ_umolm2sec") %>% rename(fq = flux) %>% mutate(fl = load_fluxbot()$flux)
ac <- load_autochamber("fluxQ_umolm2sec") %>% rename(fq = flux) %>% mutate(fl = load_autochamber()$flux)
for (x in list(fb=fb, ac=ac)) { y <- x %>% filter(fl > 0.3, fq > 0); print(quantile(y$fq/y$fl, c(.1,.25,.5,.75,.9))) }
cat("Fluxbot length_interval s:"); print(summary(fb$length_interval)); cat("AC length_interval s:"); print(summary(ac$length_interval))
# system x flux-model matrix on matched stand-hours: chamber-hour means, then mean over common stand-hours
hf <- read.csv("data/hf293-07-soil-resp-2022-2023.csv") %>% filter(year == 2023, month == 10, rs > 0) %>%
  mutate(ts = as.POSIXct(datetime, format = "%Y-%m-%dT%H:%M", tz = "Etc/GMT+5"), hour_of_obs = round_hour(format(with_tz(ts, "America/New_York"), "%Y-%m-%d %H:%M:%S")),
         id = as.character(chamber), stand = if_else(chamber <= 6, "unhealthy", "healthy"))
acq <- ac %>% mutate(hour_of_obs = hour_of_obs + 3600) # EST logger -> EDT
agg <- function(x, v) x %>% filter(.data[[v]] > 0) %>% group_by(stand, id, hour_of_obs) %>% summarise(f = mean(.data[[v]]), .groups = "drop")
S <- list(FB_linear = agg(fb, "fl"), FB_quadratic = agg(fb, "fq"),
          AC_linear = agg(acq, "fl"), AC_quadratic = agg(acq, "fq"), AC_HF_best1min = agg(hf %>% mutate(rs = rs), "rs"))
sh <- function(x) x %>% group_by(stand, hour_of_obs) %>% filter(n() >= 3) %>% summarise(f = mean(f), .groups = "drop")
SH <- lapply(S, sh)
res <- expand.grid(fb = c("FB_linear", "FB_quadratic"), ac = c("AC_linear", "AC_quadratic", "AC_HF_best1min"), stringsAsFactors = FALSE)
res <- bind_rows(lapply(seq_len(nrow(res)), function(i) {
  j <- inner_join(SH[[res$fb[i]]], SH[[res$ac[i]]], by = c("stand", "hour_of_obs"), suffix = c("_fb", "_ac"))
  a <- j %>% group_by(hour_of_obs) %>% filter(n() == 2) %>% summarise(fb = mean(f_fb), ac = mean(f_ac))
  data.frame(fluxbot = res$fb[i], autochamber = res$ac[i], n_hours = nrow(a), mean_fb = mean(a$fb), mean_ac = mean(a$ac),
             offset_pct = 100 * (mean(a$fb) / mean(a$ac) - 1), r = cor(a$fb, a$ac))
}))
print(res, digits = 3)
write.csv(res, "outputs/afm_revision/flux_model_matrix.csv", row.names = FALSE)
