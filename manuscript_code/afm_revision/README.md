# AFM manuscript analyses

The whole workflow reads only `../data_package/` (see the README there for the input files). Run from `manuscript_code/`:

```
Rscript afm_revision/run_all.R                 # everything, including flux calculation
SKIP_FLUXES=1 Rscript afm_revision/run_all.R   # reuse outputs/afm_revision/fluxes/
```

Each script runs in a fresh R session. Outputs go to `outputs/afm_revision/`:
- `fluxes/`: per-closure fluxes for both systems (main 57:00 Fluxbot window, plus the 56:00 sensitivity run).
- `numbers_all.csv`: every number quoted in the manuscript and SI, as key, value, section, note.
- `figures/`: main figures `Fig2`–`Fig9` (vector PDF and 600 dpi TIFF, Elsevier column widths) and SI figures `FigS_*`.
- `si_tables/`: formatted SI tables.
- `table_datasets_compared.csv`: the two datasets side by side (main-text Table 2).

R packages: dplyr, tidyr, readr, lubridate, purrr, goFlux (0.4.0), mgcv, lme4, zoo, epiR, ineq, ggplot2, patchwork, ragg.

## Datasets and comparisons

| Term | Definition |
|---|---|
| **as deployed** (main; `qc = "deployed"`) | All conditions. Removes chamber failures, then spikes. |
| **RH-screened** (`qc = "screened"`) | As deployed, minus Fluxbot closures with in-chamber RH ≥ 99% in the open-lid minute (54:00–55:00). A strict subset. |
| **chamber failure** | Any of: too few records to fit; a significant CO2 decline; no significant CO2 accumulation, or a poor linear fit (R² < 0.5) (chamber not sealed); or, for Fluxbots, a stuck lid. A stuck lid is a saturated episode in which the open-lid CO2 stays > 500 ppm above the other units in the stand; it is flagged in `10_fluxes.R`. |
| **spikes** | Per-chamber values beyond median ± 5 MAD. |
| **compared hours** | Hours with ≥ 3 units of each system in each stand. The array mean is the mean of the two stand means. |
| **period** | 2–31 October 2023 (the autochamber record ends on 31 October). Days are defined in local time. |
| **budgets** | Every hour of 2–31 Oct. Missing stand-hours are gap-filled with a GAM of flux on 10-cm soil temperature and hour of day; CIs come from resampling chambers. |

## Flux calculation (`10_fluxes.R`)

The same goFlux procedure is used for both systems.
- **Model:** linear (LM) for the main analysis; `best.flux` (LM or Hutchinson–Mosier) as a sensitivity analysis.
- **Fit windows:**
  - Fluxbot 57:00–60:00. The sensitivity run (`FB_WINDOW_START=56`) uses 56:00–60:00.
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

| Script | What it does |
|---|---|
| `00_prep.R` | Shared functions: loaders, QC modes (`deployed`, `screened`, `valid`, `dry`, `iqr`, ...), hourly assembly, met join, `matched_hours()`, `record()` |
| `10_fluxes.R` | Fluxes for both systems from the raw records; wet and stuck-lid flags |
| `01_baseline.R` | Reproduces the originally submitted numbers from the submitted flux files |
| `02_afm_analyses.R` | GAM stand and system effects, TOST, variance components, array agreement, diel cycle, Q10, sampling effort, sensitivity table |
| `03_pressure.R` | Station pressure vs HF001 sea-level pressure |
| `04_local_soil_temperature.R` | Autochamber soil probes (HF293) vs HF001 |
| `05_figures.R` | Main Figs 2–7; SI GAM fit and flux distributions |
| `06_uptime.R` | Data collection rates |
| `07_agreement_metrics.R` | Agreement-metric panels (both datasets), spatial-null benchmark, random-slope Q10, diel shape, ratio drivers |
| `08_chamber_physics.R` | In-chamber temperature, chamber-level Q10 bootstrap, antecedent rain |
| `09_flux_model_matrix.R` | Offset under every pairing of flux models |
| `11_filter_flow.R` | Measurement accounting and agreement by stage; Fig 8 |
| `12_scales_budget.R` | Agreement vs averaging scale, with within-system benchmark; October budgets, budget ratios with CIs |
| `13_sampling_rate.R` | Autochamber closures thinned to 6 s |
| `14_q10_moisture.R` | Q10 components and drivers; sensor delay (breakpoint) |
| `15_window_sensitivity.R` | 56:00 vs 57:00 Fluxbot window |
| `16_vent_wet.R` | Open-lid baseline anomaly |
| `17_resilience.R` | Coverage and outages, independence expectation; Fig 9 and SI resilience figure |
| `18_ptfe_lab_test.R` | Laboratory test, 22 Sep 2023 |
| `19_cover_test_dec.R` | Laboratory test, 13 Dec 2023 |
| `20_lab_failure_modes.R` | Failure-mode diagnostics from both lab tests |
| `21_wet_recovery.R` | Field wet-sensor episodes: stuck lid vs wet sensor, recovery, data loss and bias |
| `22_wet_selection_bias.R` | Selection bias from removing wet periods (autochamber flux in wet hours; budget under the Fluxbot sampling pattern) |
| `23_fig_moisture_si.R` | Combined SI lab-test figure |
| `24_subset_summary.R` | The two datasets side by side |
| `25_fig_fit_window.R` | SI fit-window figure |
| `26_fig_flux_model.R` | SI flux-model figure |
| `27_si_tables.R` | Formatted SI tables |

## What changed from the originally submitted analysis

1. **Fluxes recomputed for both systems with one procedure** (`10_fluxes.R`). The original Fluxbot function had two data-handling bugs:
   - CO2 error-code rows were removed from the CO2 vector only, so R recycled it against the timestamps (24% of closures).
   - Failed temperature reads (44,755 °C) entered the gas law (11% of closures).

   The original autochamber code joined met data with a ~4 h lag.
2. **Autochamber clock:** it was read as EDT; it is EST.
3. **Pressure:** HF001 sea-level pressure was used, making fluxes ~4% high. Station pressure is used now.
4. **HF001 time zone:** it was parsed in the computer's local time zone; it is EST.
5. **QC:** the original rule (negatives removed, pooled 1.5 × IQR) is replaced by the chamber-failure rules above plus the spike screen. Wet sensors are kept in the main dataset and removed in the RH-screened subset.
6. **Hour filters:**
   - The CCC hour filter accepted hours in which a whole system × stand was missing; this is fixed.
   - The Table 2 filter recycled a vector; this is fixed.
   - A single compared-hours rule (≥ 3 units) is now used throughout.
7. **Day boundaries:** daily means used UTC days; they now use local days.
8. **Fluxbot sampling interval:** ~6 s, not 1 Hz.
