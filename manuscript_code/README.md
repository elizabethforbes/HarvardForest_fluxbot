# Analysis code

This code reproduces every flux, dataset, number, figure and table in the paper from the raw data in `../data_package/`. Run it from this folder:

```
Rscript run_all.R                  # everything (~11 min on a laptop, ~2 min of it flux calculation)
SKIP_FLUXES=1 Rscript run_all.R    # reuse ../data_clean/fluxes/
```

`run_all.R` runs each script in a fresh R session, in folder and file order. Each script's console output goes to `outputs/logs/`, along with `sessionInfo.txt`.

- R version: 4.4.3.
- Package versions: recorded in `renv.lock`. Restore them with `renv::restore(lockfile = "renv.lock")`.
- Main packages: goFlux 0.4.0, dplyr, tidyr, readr, lubridate, fuzzyjoin, mgcv, lme4, zoo, epiR, ineq, ggplot2, patchwork, gridExtra, ragg.

## Data flow

```
../data_package/          raw records, metadata, ancillary (HF001, HF293), lab-test raw data
      |  1_clean/01-02    flux calculation (goFlux), both systems, one procedure
      v
../data_clean/fluxes/     one row per closure, every closure (25 Sep - 5 Nov 2023), before QC
      |  1_clean/03       data hygiene: QC flags, status of every closure, analysis datasets
      v
../data_clean/            closures_{fluxbot,autochamber}.csv   every closure in 2-31 Oct + QC flags + qc_status
                          chamber_hours_deployed.csv           MAIN dataset (as deployed)
                          chamber_hours_screened.csv           RH-screened subset
                          qc_log.csv, data_dictionary.csv
      |  2_analysis, 3_lab_tests
      v
outputs/results/          analysis tables and model objects
outputs/numbers/          every number quoted in the paper, one file per script; numbers_all.csv (with the script)
      |  4_figures, 5_tables
      v
outputs/figures/          Fig2-Fig6 (PDF, PNG, 600 dpi TIFF) and FigS01-FigS11 (PDF, PNG)
outputs/si_tables/        SI tables
outputs/diagnostics/      figures not in the paper (lab-test full runs, chamber temperature)
```

The analyses read only `../data_clean/`. A few sensor-level diagnostics also read `../data_package/`: pressure, open-lid baselines, sensor delay, wet episodes, sampling rate and the lab tests. Shared code is in `R/`:
- `setup.R`: paths, loaders, QC rules, hourly assembly, and the `record()`/`write_numbers()` helpers;
- `fig_style.R`: palette, sizes and theme;
- `filter_iqr.R`: the originally submitted QC rule, kept for the sensitivity analysis.

## Datasets and comparisons

| Term | Definition |
|---|---|
| **as deployed** (main) | All conditions. Chamber failures and spikes removed (rules below). File: `data_clean/chamber_hours_deployed.csv`. |
| **RH-screened** | As deployed, minus Fluxbot closures with in-chamber RH ≥ 99% in the open-lid minute (54:00–55:00), i.e. wet K30 sensors. A strict subset. File: `data_clean/chamber_hours_screened.csv`. |
| **compared hours** | Hours with ≥ 3 units of each system in each stand. The array mean is the mean of the two stand means. |
| **period** | 2–31 October 2023 (the autochamber record ends on 31 October). Days are defined in local time. |
| **budgets** | Every hour of 2–31 Oct. Missing stand-hours are gap-filled with a GAM of flux on 10-cm soil temperature and hour of day. CIs come from resampling chambers. |

### Data-hygiene rules (`1_clean/03_clean_datasets.R`)

Each closure gets the first rule that removes it (`qc_status`), for the main linear-flux model:

1. **no flux computed**
2. **too few records**: goFlux `nb.obs` flag.
3. **chamber failure: stuck lid** (Fluxbot): a saturated episode in which the open-lid CO2 stays > 500 ppm above the stand's other units. Flagged in `01_fluxes.R`.
4. **chamber failure: no CO2 accumulation**: the slope is not significant (goFlux `p-value` flag).
5. **chamber failure: CO2 decline**: a significant negative slope.
6. **chamber failure: poor linear fit**: R² < 0.5.
7. **spike**: outside the chamber's median ± 5 MAD, among closures passing rules 1–6.

Closures that pass every rule are **retained** (the as-deployed dataset). Retained Fluxbot closures with the wet-sensor flag are removed in the RH-screened subset. `qc_log.csv` counts closures at each status. Sensitivity analyses rebuild other datasets from the closure tables with `apply_qc()` in `R/setup.R`: other flux models, the originally submitted QC rule, and negatives only.

The two chamber-hour datasets are written as CSV, then read back and checked against the in-memory datasets. Decimal text does not round-trip doubles to the last bit, so values can differ by about 1e-16 relative (one unit in the last place). No reported number depends on that.

## Flux calculation (`1_clean/01_fluxes.R`)

The same goFlux procedure is used for both systems.

- **Model:** linear (LM) for the main analysis; `best.flux` (LM or Hutchinson–Mosier) as a sensitivity analysis.
- **Fit windows:**
  - Fluxbot 57:00–60:00. The sensitivity run (`02_fluxes_window56.R`) uses 56:00–60:00.
  - Autochamber: slot + 75 s to + 295 s.
- **Pressure:** station pressure from the Fluxbot LPS22 sensors (hourly stand median, excluding unit 100). The fallback is HF001 multiplied by the median station/HF001 ratio.
- **Temperature:** Fluxbot in-chamber air temperature; autochamber HF001 air temperature.
- **Water vapour:** no correction.
- **Instrument precision:** estimated from the data.
- **Removed or masked:**
  - Fluxbot error codes (65535, 65533) are removed as whole rows.
  - SHT-30 sentinel values are set to missing.
- **Clocks:**
  - Fluxbot: UNIX time.
  - Autochamber logger and HF001: EST.

## Scripts

| Script | What it does | Paper |
|---|---|---|
| **1_clean** | | |
| `01_fluxes.R` | Per-closure fluxes for both systems from the raw records; wet-sensor and stuck-lid flags | Methods |
| `02_fluxes_window56.R` | Fluxbot fluxes with the 56:00 window (sensitivity) | Fig. S2 |
| `03_clean_datasets.R` | QC status of every closure; as-deployed and RH-screened chamber-hour datasets; QC log; data dictionary | Methods, Table 2 |
| **2_analysis** | | |
| `01_submitted_baseline.R` | Reproduces the originally submitted numbers from the submitted flux files | change log |
| `02_station_pressure.R` | Station pressure vs HF001 sea-level pressure | Methods |
| `03_main_analyses.R` | GAM stand and system effects, TOST, variance components, array agreement, diel cycle, Q10, sampling effort, sensitivity table | Results, Table S3 |
| `04_soil_temperature.R` | Autochamber soil probes (HF293) vs HF001 | Methods |
| `05_agreement_metrics.R` | Agreement-metric panels (both datasets), spatial-null benchmark, random-slope Q10, diel shape, ratio drivers | Table 2, Fig. S5 |
| `06_chamber_physics.R` | In-chamber temperature, chamber-level Q10 bootstrap, antecedent rain | Results |
| `07_flux_model_matrix.R` | Offset under every pairing of flux models; closure-level best/linear ratio | Table S4, Fig. S3 |
| `08_measurement_accounting.R` | Closures intended, recorded, valid, retained, RH-screened; agreement at each stage | Fig. 6a, Table S2 |
| `09_scales_budget.R` | Agreement vs averaging scale with within-system benchmark; October budgets and ratios | Fig. 2c, Fig. S4 |
| `10_sampling_rate.R` | Autochamber closures thinned to the Fluxbot sampling rate | SI |
| `11_q10_moisture.R` | Q10 components and drivers; sensor delay (breakpoint) | SI |
| `12_window_sensitivity.R` | 56:00 vs 57:00 Fluxbot window, array and closure level | Fig. S2 |
| `13_vent_wet.R` | Open-lid baseline anomaly | SI |
| `14_resilience.R` | Coverage and outages; independence expectation; hourly array states | Fig. 6d, Fig. S11 |
| `15_wet_recovery.R` | Field wet-sensor episodes: stuck lid vs wet sensor, recovery, data loss and bias | Fig. S10 |
| `16_wet_selection_bias.R` | Selection bias from removing wet periods | Results |
| `17_dataset_summary.R` | The two datasets side by side | Table 2 |
| **3_lab_tests** | | |
| `01_ptfe_test_sep.R` | Laboratory test, 22 Sep 2023 | Fig. S8 |
| `02_cover_test_dec.R` | Laboratory test, 13 Dec 2023 | Fig. S8 |
| `03_failure_modes.R` | Failure-mode diagnostics from both tests | Fig. S9 |
| **4_figures** | One script per figure: `fig2_fluxes_budgets.R`, `fig3_array_agreement.R`, `fig4_drivers.R`, `fig5_heterogeneity_effort.R`, `fig6_data_flow.R`, `figS01_site_map.R` … `figS11_resilience.R` | |
| **5_tables** | `01_si_tables.R`: formatted SI tables | SI |

Fig. 1 is a field photograph and is not generated by code. Fig. S1 (site map) downloads basemap tiles (Esri) and so needs an internet connection; everything else runs offline.

## What changed from the originally submitted analysis

The original code and flux files are in `../legacy/submitted_analysis/`. `2_analysis/01_submitted_baseline.R` reproduces the submitted numbers from them.

1. **Fluxes recomputed for both systems with one procedure** (`1_clean/01_fluxes.R`). The original Fluxbot function had two data-handling bugs:
   - CO2 error-code rows were removed from the CO2 vector only, so R recycled it against the timestamps (24% of closures).
   - Failed temperature reads (44,755 °C) entered the gas law (11% of closures).

   The original autochamber code joined met data with a ~4 h lag.
2. **Autochamber clock:** it was read as EDT; it is EST.
3. **Pressure:** HF001 sea-level pressure was used, making fluxes ~4% high. Station pressure is used now.
4. **HF001 time zone:** it was parsed in the computer's local time zone; it is EST.
5. **QC:** the original rule (negatives removed, pooled 1.5 × IQR) is replaced by the data-hygiene rules above. Wet sensors are kept in the main dataset and removed in the RH-screened subset.
6. **Hour filters:**
   - The CCC hour filter accepted hours in which a whole system × stand was missing; this is fixed.
   - The Table 2 filter recycled a vector; this is fixed.
   - A single compared-hours rule (≥ 3 units) is now used throughout.
7. **Day boundaries:** daily means used UTC days; they now use local days.
8. **Fluxbot sampling interval:** ~6 s, not 1 Hz.
