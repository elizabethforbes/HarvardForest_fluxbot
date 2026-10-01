# One table of the two datasets compared with the autochambers, so every number in the paper can
# name its subset:
#   as deployed (main): all conditions; chamber failures (short, CO2 decline, stuck lid) and spikes removed
#   RH-screened       : as deployed minus Fluxbot closures with in-chamber RH >= 99% before closure
# Comparisons:
#   hourly / daily agreement : array means over compared hours (>= 3 units of each system per stand)
#   72-h blocks              : block means of the compared hours
#   budget                   : 2-31 Oct, every hour, gaps filled per stand (s10t + time of day GAM)
# Reads numbers written by 07, 11, 12 and 22.

source("afm_revision/00_prep.R")
n <- bind_rows(lapply(list.files(out_dir, "^numbers_.*\\.csv$", full.names = TRUE) %>% setdiff(file.path(out_dir, "numbers_all.csv")), read.csv))
v <- function(k) { x <- n$value[n$key == k]; if (length(x)) x[1] else NA_real_ }
acc <- read.csv(file.path(out_dir, "measurement_accounting.csv"))
row <- function(ds, label, stage, scale_tag, bud_tag) tibble(
  dataset = label,
  fluxbot_closures = acc$n[acc$system == "fluxbot" & acc$stage == stage],
  pct_of_intended = acc$pct_of_intended[acc$system == "fluxbot" & acc$stage == stage],
  compared_hours = v(sprintf("panel_%s_hourly_n", ds)),
  offset_pct = v(sprintf("panel_%s_hourly_bias_pct", ds)),
  r_hourly = v(sprintf("panel_%s_hourly_r", ds)), ccc_hourly = v(sprintf("panel_%s_hourly_ccc", ds)),
  r_daily = v(sprintf("panel_%s_daily_r", ds)), ccc_daily = v(sprintf("panel_%s_daily_ccc", ds)),
  r_72h = v(sprintf("scale_%s_72h_r", scale_tag)), nrmse_72h = v(sprintf("scale_%s_72h_nrmse", scale_tag)),
  budget_fluxbot = v(sprintf("budget_%s_fluxbot_mean", bud_tag)), budget_autochamber = v("budget_LM_autochamber_mean"),
  budget_ratio = v(sprintf("budget_ratio_LM_%s", ds)), budget_ratio_lo = v(sprintf("budget_ratio_LM_%s_lo", ds)),
  budget_ratio_hi = v(sprintf("budget_ratio_LM_%s_hi", ds)),
  sampling_bias_pct = v(sprintf("selbias_budget_%s_vs_all_pct", ds)))
tab <- bind_rows(row("deployed", "As deployed (main; all conditions)", "retained", "LM", "LM"),
                 row("screened", "RH-screened (wet-sensor closures removed)", "RH-screened", "LMscreened", "LMscreened"))
write.csv(tab, file.path(out_dir, "table_datasets_compared.csv"), row.names = FALSE)
print(as.data.frame(tab), digits = 3)
