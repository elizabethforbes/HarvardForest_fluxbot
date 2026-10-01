# Supplementary tables, formatted for the SI (outputs/si_tables/). Each table names the
# dataset(s) it uses: "as deployed" (main, all conditions) and/or "RH-screened" (wet-sensor closures
# removed). Compared hours = >= 3 units of each system in each stand; period 2-31 Oct 2023.

source("R/setup.R")
td <- si_dir
rd <- function(f) read.csv(file.path(out_dir, f), check.names = FALSE)
w <- function(x, f) { write.csv(x, file.path(td, f), row.names = FALSE); cat("\n==", f, "\n"); print(x, row.names = FALSE) }
r2 <- function(x, k = 2) round(x, k)

# S-accounting: measurement accounting
acc <- rd("measurement_accounting.csv") %>%
  transmute(System = recode(system, fluxbot = "Fluxbot (16 units)", autochamber = "Autochamber (12 chambers)"),
            Stage = recode(stage, intended = "Intended", recorded = "Recorded", computed = "Flux computed", valid = "Valid", retained = "Retained (as deployed)", `RH-screened` = "RH-screened subset"),
            Closures = n, `% of intended` = r2(pct_of_intended, 1), `Removed at this step` = lost,
            Reason = recode(stage, intended = "", recorded = "no data (logger, power or transmission down)",
                            computed = "too few records to fit a flux",
                            valid = "failed fit or chamber check (no CO2 accumulation, CO2 decline, linear-fit R2 < 0.5, stuck lid)",
                            retained = "spike (> 5 MAD from chamber median)", `RH-screened` = "wet sensor (in-chamber RH >= 99% before closure)"))
w(acc, "TableS_accounting.csv")

# S-datasets: the two datasets side by side (hourly, daily, 72-h, budget)
dc <- rd("table_datasets_compared.csv")
w(dc %>% transmute(Dataset = dataset, `Fluxbot closures` = fluxbot_closures, `% of intended` = r2(pct_of_intended, 1),
                   `Compared hours` = compared_hours, `Offset (%)` = r2(offset_pct, 1),
                   `r (hourly)` = r2(r_hourly), `CCC (hourly)` = r2(ccc_hourly), `r (daily)` = r2(r_daily), `CCC (daily)` = r2(ccc_daily),
                   `r (72-h)` = r2(r_72h), `nRMSE (72-h, %)` = r2(nrmse_72h, 1),
                   `Budget, Fluxbot (g C m-2)` = r2(budget_fluxbot, 1), `Budget, autochamber (g C m-2)` = r2(budget_autochamber, 1),
                   `Budget ratio (95% CI)` = sprintf("%.2f (%.2f-%.2f)", budget_ratio, budget_ratio_lo, budget_ratio_hi),
                   `Sampling-pattern bias in budget (%)` = r2(sampling_bias_pct, 1)), "TableS_datasets_compared.csv")

# S-metrics: full agreement metric panel, both datasets x three scales
pm <- bind_rows(rd("agreement_metric_panel.csv") %>% mutate(Dataset = "As deployed"),
                rd("agreement_metric_panel_screened.csv") %>% mutate(Dataset = "RH-screened")) %>%
  transmute(Dataset, Scale = scale, n, `Mean, autochamber` = r2(mean_ref), `Mean, Fluxbot` = r2(mean_fb), r = r2(r),
            `SMA slope` = r2(sma_slope), `Bias` = r2(bias), `Bias (%)` = r2(bias_pct, 1), RMSE = r2(rmse), `nRMSE (%)` = r2(nrmse_pct, 1),
            `Limits of agreement` = sprintf("%.2f to %.2f", loa_lo, loa_hi), CCC = r2(ccc), `Cb` = r2(cb))
w(pm, "TableS_agreement_metrics.csv")

# S-sensitivity: processing and QC choices
sens <- rd("sensitivity_table.csv") %>% filter(!grepl("HF293", scenario)) %>%
  transmute(Scenario = scenario, `Fluxbot closures` = n_fluxbot, `Method effect (GAM, umol m-2 s-1; 95% CI)` = sprintf("%.2f (%.2f to %.2f)", gam_method, gam_method_lo, gam_method_hi),
            `Paired bias, 3-h array means (umol m-2 s-1; block-bootstrap 95% CI)` = sprintf("%.2f (%.2f to %.2f)", paired_bias, paired_bias_lo, paired_bias_hi),
            `r (3-h array means)` = r2(r), `CCC (3-h array means)` = r2(ccc), `Q10 autochamber` = r2(q10_ac), `Q10 Fluxbot` = r2(q10_fb))
w(sens, "TableS_sensitivity.csv")

# S-fluxmodel: flux-model pairings (as deployed)
fm <- rd("flux_model_matrix.csv") %>% filter(autochamber != "HF293") %>% transmute(`Fluxbot model` = fluxbot, `Autochamber model` = autochamber, `Compared hours` = n_hours,
                                                 `Mean, Fluxbot` = r2(mean_fb), `Mean, autochamber` = r2(mean_ac), `Offset (%)` = r2(offset_pct, 1), r = r2(r))
w(fm, "TableS_flux_models.csv")

# S-q10: chamber-level Q10 (as deployed)
q <- rd("q10_by_chamber.csv") %>% transmute(System = method, Stand = stand, Chamber = sub("^(autochamber|fluxbot)", "", id), Q10 = r2(q10), n) %>% arrange(System, Stand, Chamber)
w(q, "TableS_q10_by_chamber.csv")

# S-srate: sampling-rate emulation
sr <- rd("sampling_rate_emulation.csv") %>% transmute(`Thinned to` = scenario, Closures = n, `LM flux ratio, median` = r2(LM_ratio_median, 3),
                                                      `LM flux ratio, CV` = r2(LM_ratio_cv, 3), `LM SE ratio` = r2(LM_SE_ratio), `r (closure LM)` = r2(LM_r, 4),
                                                      `best flux ratio, median` = r2(best_ratio_median, 3), `MDF ratio` = r2(MDF_ratio))
w(sr, "TableS_sampling_rate.csv")

# S-lab: laboratory tests, per sensor and phase
lab <- rd("lab_failure_modes_by_phase.csv") %>%
  transmute(Test = test, Phase = phase, Sensor = sensor, Group = group, `Error codes (%)` = r2(err_pct, 1), `Longest gap (s)` = max_gap_s,
            `tau (s)` = refit_tau, Gain = r2(refit_b), `RMSE (ppm)` = r2(refit_rmse, 1), `Bias vs own dry calibration (ppm)` = r2(bias_vs_dry, 1),
            `Scatter vs dry (MAD, ppm)` = r2(scatter_vs_dry, 1), `Spikes (%)` = r2(spike_pct, 1))
w(lab, "TableS_lab_tests.csv")

# S-wet: effect of wet sensors and stuck lids on agreement
wb <- rd("wet_bias_scenarios.csv") %>% transmute(Scenario = scenario, `Fluxbot closures` = fb_closures, `Compared hours` = n_hours,
                                                  `Offset (%)` = r2(offset_pct, 1), `r (hourly)` = r2(r_hourly), `r (daily)` = r2(r_daily))
w(wb, "TableS_wet_scenarios.csv")

# S-resilience: coverage and outages by stand and system (as deployed)
rs <- rd("resilience_summary.csv") %>% transmute(System = method, Stand = stand, Units = n_units, `Mean unit success (%)` = r2(100 * mean_unit_success, 1),
                                                  `Hours with >= 1 unit (%)` = r2(100 * cov1, 1), `Hours with >= 3 units (%)` = r2(100 * cov3, 1),
                                                  `Longest outage (h)` = longest_outage_h,
                                                  `Hours < 3 units, observed (%)` = r2(100 * obs_lt3_frac, 1), `... expected if independent (%)` = r2(100 * exp_lt3_frac, 1))
w(rs, "TableS_resilience.csv")
