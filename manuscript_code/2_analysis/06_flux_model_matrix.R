# Fluxbot vs autochamber offset under each combination of flux model: goFlux linear (LM),
# Hutchinson-Mosier (HM) and best.flux selection for both systems, plus the fluxes published
# by the Harvard Forest team for the autochambers (HF293-07). Offsets are computed on
# stand-hours where each system had >= 3 chambers, averaged over the two stands.

source("R/setup.R")

sh <- function(x) x %>% group_by(stand, id, hour_of_obs) %>% summarise(f = mean(flux), .groups = "drop") %>%
  group_by(stand, hour_of_obs) %>% filter(n() >= 3) %>% summarise(f = mean(f), .groups = "drop")
models <- c(LM = "LM.flux", HM = "HM.flux", best = "best.flux")
FB <- lapply(models, function(m) sh(apply_qc(load_fluxbot(m), "deployed")))
AC <- c(lapply(models, function(m) sh(apply_qc(load_autochamber(m), "deployed"))),
        list(HF293 = sh(load_hf293() %>% group_by(id) %>% filter(abs(flux - median(flux)) <= 5 * mad(flux)) %>% ungroup())))
res <- bind_rows(lapply(names(FB), function(i) bind_rows(lapply(names(AC), function(j) {
  a <- inner_join(FB[[i]], AC[[j]], by = c("stand", "hour_of_obs"), suffix = c("_fb", "_ac")) %>%
    group_by(hour_of_obs) %>% filter(n() == 2) %>% summarise(fb = mean(f_fb), ac = mean(f_ac))
  data.frame(fluxbot = i, autochamber = j, n_hours = nrow(a), mean_fb = mean(a$fb), mean_ac = mean(a$ac),
             offset = mean(a$fb - a$ac), offset_pct = 100 * (mean(a$fb) / mean(a$ac) - 1), r = cor(a$fb, a$ac))
})))) 
print(res, digits = 3)
write.csv(res, file.path(out_dir, "flux_model_matrix.csv"), row.names = FALSE)
for (k in seq_len(nrow(res))) {
  tag <- paste0("fm_", res$fluxbot[k], "_vs_", res$autochamber[k])
  record(paste0(tag, "_offset_pct"), res$offset_pct[k], "flux_model")
  record(paste0(tag, "_r"), res$r[k], "flux_model")
}
# curvature (HM/LM) by system
for (s in c("fluxbot", "autochamber")) {
  x <- read.csv(file.path(flux_dir, paste0(s, "_fluxes.csv"))) %>% filter(LM.flux > 0.3, !is.na(curvature))
  record(paste0("curvature_median_", s), median(x$curvature), "flux_model", "HM/LM initial slope")
  record(paste0("share_HM_", s), mean(x$model == "HM"), "flux_model", "best.flux chose HM")
}
# closure level: goFlux best / linear flux, and how often the HM model was chosen (as deployed; Fig. S3a)
cl <- bind_rows(apply_qc(load_fluxbot("LM.flux"), "deployed") %>% mutate(system = "Fluxbot 2.0"),
                apply_qc(load_autochamber("LM.flux"), "deployed") %>% mutate(system = "Autochamber")) %>%
  filter(LM.flux > 0.3) %>% mutate(ratio = best.flux / LM.flux, hm = model == "HM")
s <- cl %>% group_by(system) %>% summarise(hm_pct = 100 * mean(hm), ratio_med = median(ratio), ratio_mean = mean(ratio), .groups = "drop")
print(s)
for (i in seq_len(nrow(s))) { tg <- if_else(grepl("Fluxbot", s$system[i]), "fluxbot", "autochamber")
  record(paste0("fm_hm_selected_pct_", tg), s$hm_pct[i], "flux_model", "goFlux best.flux chose HM")
  record(paste0("fm_best_lm_ratio_median_", tg), s$ratio_med[i], "flux_model", "closure-level best / LM") }
write.csv(cl %>% select(system, ratio, hm), file.path(out_dir, "flux_model_closures.csv"), row.names = FALSE)
write.csv(s, file.path(out_dir, "flux_model_closure_summary.csv"), row.names = FALSE)
write_numbers()
