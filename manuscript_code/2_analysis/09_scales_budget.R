# (A) Agreement as a function of temporal aggregation, benchmarked against the agreement
#     between independent chamber subsets of the same system at the same scale.
# (B) Cumulative October CO2-C efflux per stand from each system and flux model.
# Both for the main "as deployed" dataset (all conditions) and the RH-screened subset (Fluxbot
# wet-sensor closures removed; the autochamber data are the same in both).

source("R/setup.R")
suppressPackageStartupMessages({ library(mgcv); library(epiR) })
set.seed(20260930)
p1 <- as.POSIXct("2023-11-01", tz = "America/New_York")
blocks <- c(1, 3, 6, 12, 24, 72)

# stand-hour means from a set of chambers (>= k reporting), array = mean of the two stands
stand_hour <- function(x, k) x %>% group_by(method, stand, hour_of_obs) %>% filter(n() >= k) %>%
  summarise(f = mean(fluxL_umolm2sec), .groups = "drop")
array_hour <- function(sh) sh %>% group_by(method, hour_of_obs) %>% filter(n() == 2) %>% summarise(f = mean(f), .groups = "drop")
block_means <- function(a, b, L) {
  a %>% mutate(blk = floor(as.numeric(difftime(hour_of_obs, as.POSIXct("2023-10-02", tz = "America/New_York"), units = "hours")) / L)) %>%
    group_by(blk) %>% filter(n() >= max(1, L / 2)) %>% summarise(x = mean(x), y = mean(y), .groups = "drop")
}
agree <- function(x, y) c(n = length(x), r = cor(x, y), offset_pct = 100 * (mean(y) / mean(x) - 1),
                          nrmse = 100 * sqrt(mean((y - x)^2)) / mean(x), ccc = epi.ccc(x, y)$rho.c$est)

scale_table <- function(d) {
  d <- d %>% filter(hour_of_obs < p1)
  ah <- array_hour(stand_hour(d, 3)) %>% pivot_wider(names_from = method, values_from = f) %>%
    filter(!is.na(autochamber), !is.na(fluxbot)) %>% rename(x = autochamber, y = fluxbot)
  cross <- bind_rows(lapply(blocks, function(L) { b <- block_means(ah, NULL, L); c(L = L, agree(b$x, b$y)) }))
  # within-system benchmark: disjoint subsets of 3 chambers per stand
  ids <- d %>% distinct(method, stand, id) %>% mutate(id = as.character(id))
  dd <- d %>% mutate(id = as.character(id))
  pick <- function(sys, excl = NULL) bind_rows(lapply(c("healthy", "unhealthy"), function(st) {
    p <- ids %>% filter(method == sys, stand == st, !id %in% excl); p[sample(nrow(p), 3), ] }))
  sub_series <- function(sel) array_hour(stand_hour(dd %>% semi_join(sel, by = c("method", "stand", "id")) %>%
                                                      mutate(method = "x"), 2)) %>% select(hour_of_obs, f)
  bench <- bind_rows(lapply(c("autochamber", "fluxbot", "cross"), function(pr) bind_rows(replicate(150, {
    a <- if (pr == "cross") pick("autochamber") else pick(pr)
    b <- if (pr == "cross") pick("fluxbot") else pick(pr, a$id)
    s <- inner_join(sub_series(a), sub_series(b), by = "hour_of_obs") %>% rename(x = f.x, y = f.y)
    bind_rows(lapply(blocks, function(L) { bm <- block_means(s, NULL, L)
      if (nrow(bm) < 5) return(NULL); c(L = L, agree(bm$x, bm$y)) })) %>% mutate(pair = pr)
  }, simplify = FALSE))))
  list(cross = cross, bench = bench)
}

res <- list(LM = scale_table(build_dataset(flux_col = "LM.flux")), best = scale_table(build_dataset(flux_col = "best.flux")),
            LMscreened = scale_table(build_dataset(qc = "screened", flux_col = "LM.flux")))
for (m in names(res)) {
  write.csv(res[[m]]$cross, file.path(out_dir, paste0("scale_agreement_", m, ".csv")), row.names = FALSE)
  bs <- res[[m]]$bench %>% group_by(pair, L) %>% summarise(across(c(r, nrmse, offset_pct), list(med = median,
          lo = ~ quantile(., .1), hi = ~ quantile(., .9)), .names = "{.col}_{.fn}"), n_draws = n(), .groups = "drop")
  write.csv(bs, file.path(out_dir, paste0("scale_benchmark_", m, ".csv")), row.names = FALSE)
  print(res[[m]]$cross, digits = 3); print(as.data.frame(bs %>% select(pair, L, r_med, nrmse_med)), digits = 3)
  for (i in seq_len(nrow(res[[m]]$cross))) for (k in c("r", "offset_pct", "nrmse", "n"))
    record(sprintf("scale_%s_%dh_%s", m, res[[m]]$cross$L[i], k), res[[m]]$cross[[k]][i], "scales", "full arrays, block means")
  for (i in seq_len(nrow(bs))) for (k in c("r_med", "nrmse_med"))
    record(sprintf("scalebench_%s_%s_%dh_%s", m, bs$pair[i], bs$L[i], k), bs[[k]][i], "scales", "3 chambers per stand per subset")
}
# ---- (B) cumulative October efflux (2-31 Oct) ----------------------------------------------------
hours <- tibble(hour_of_obs = seq(as.POSIXct("2023-10-02 00:00", tz = "America/New_York"), p1 - 3600, by = "hour"))
met <- load_met() %>% mutate(hour_of_obs = floor_date(with_tz(Time, "America/New_York"), "hour")) %>%
  group_by(hour_of_obs) %>% summarise(s10t = mean(s10t), .groups = "drop")
budget <- function(x) {   # x: chamber-hour fluxes for one system; returns g C m-2 per stand
  sh <- stand_hour(x %>% filter(hour_of_obs < p1), 3)
  bind_rows(lapply(c("healthy", "unhealthy"), function(st) {
    s <- sh %>% filter(stand == st) %>% left_join(met, by = "hour_of_obs") %>% mutate(hr = hour(hour_of_obs))
    g <- gam(f ~ s(s10t, k = 6) + s(hr, bs = "cc", k = 8), data = s, knots = list(hr = c(-0.5, 23.5)))
    full <- hours %>% left_join(met, by = "hour_of_obs") %>% mutate(hr = hour(hour_of_obs)) %>%
      left_join(s %>% select(hour_of_obs, f), by = "hour_of_obs") %>%
      mutate(pred = predict(g, newdata = .), f_filled = coalesce(f, pred))
    tibble(stand = st, gC_m2 = sum(full$f_filled) * 3600 * 12.011e-6, share_observed = mean(!is.na(full$f)),
           mean_flux = mean(full$f_filled))
  }))
}
chamber_boot <- function(x, B = 200) {
  ids <- x %>% distinct(stand, id)
  replicate(B, {
    bi <- ids %>% group_by(stand) %>% slice_sample(prop = 1, replace = TRUE) %>% mutate(bid = paste0(id, "_", row_number())) %>% ungroup()
    xb <- bi %>% inner_join(x, by = c("stand", "id"), relationship = "many-to-many") %>% mutate(id = bid)
    sum(budget(xb)$gC_m2) / 2
  })
}
bud <- list(); boots <- list()
for (cfg in list(c("LM.flux", "deployed"), c("best.flux", "deployed"), c("LM.flux", "screened"))) {
  m <- cfg[1]; q <- cfg[2]
  d <- build_dataset(qc = q, flux_col = m)
  for (sys in c("autochamber", "fluxbot")) {
    if (q == "screened" && sys == "autochamber") next     # identical to the deployed autochamber data
    x <- d %>% filter(method == sys)
    b <- budget(x); bt <- chamber_boot(x); boots[[paste(m, q, sys)]] <- bt
    bud[[paste(m, q, sys)]] <- b %>% mutate(model = m, dataset = q, system = sys, mean_stands_gC = mean(b$gC_m2),
                                            lo = quantile(bt, 0.025), hi = quantile(bt, 0.975))
  }
}
# Fluxbot / autochamber budget ratio with a bootstrap CI (chambers resampled independently per system)
for (cfg in list(c("LM.flux", "deployed"), c("best.flux", "deployed"), c("LM.flux", "screened"))) {
  fbb <- boots[[paste(cfg[1], cfg[2], "fluxbot")]]; acb <- boots[[paste(cfg[1], "deployed", "autochamber")]]
  fbm <- bud[[paste(cfg[1], cfg[2], "fluxbot")]]$mean_stands_gC[1]; acm <- bud[[paste(cfg[1], "deployed", "autochamber")]]$mean_stands_gC[1]
  tag <- sprintf("budget_ratio_%s_%s", sub(".flux", "", cfg[1]), cfg[2])
  record(tag, fbm / acm, "budget", "Fluxbot / autochamber, gap-filled 2-31 Oct")
  record(paste0(tag, "_lo"), quantile(fbb / acb, 0.025), "budget"); record(paste0(tag, "_hi"), quantile(fbb / acb, 0.975), "budget")
}
x <- build_dataset_hf293() %>% filter(method == "autochamber")
b <- budget(x); bud[["HF293"]] <- b %>% mutate(model = "HF293 published", dataset = "deployed", system = "autochamber", mean_stands_gC = mean(b$gC_m2), lo = NA, hi = NA)
bud <- bind_rows(bud)
write.csv(bud, file.path(out_dir, "october_budget.csv"), row.names = FALSE)
print(bud, digits = 3)
for (i in seq_len(nrow(bud))) record(sprintf("budget_%s%s_%s_%s", sub(".flux", "", bud$model[i]), if_else(bud$dataset[i] == "screened", "screened", ""), bud$system[i], bud$stand[i]),
                                     bud$gC_m2[i], "budget", "g C m-2, 2-31 Oct, gap-filled")
bm <- bud %>% distinct(model, dataset, system, mean_stands_gC, lo, hi)
for (i in seq_len(nrow(bm))) {
  tag <- sprintf("budget_%s%s_%s_mean", sub(".flux", "", bm$model[i]), if_else(bm$dataset[i] == "screened", "screened", ""), bm$system[i])
  record(tag, bm$mean_stands_gC[i], "budget"); record(paste0(tag, "_lo"), bm$lo[i], "budget"); record(paste0(tag, "_hi"), bm$hi[i], "budget")
}
write_numbers()
