# AFM revision analyses

Run from `manuscript_code/`:

```
Rscript afm_revision/run_all.R
```

Outputs go to `outputs/afm_revision/`. Every number quoted in the revised
manuscript is in `numbers_all.csv` (key, value, section, note). Figures
(vector PDF plus 600 dpi TIFF, sized to Elsevier column widths) are in
`outputs/afm_revision/figures/`.

| Script | What it does |
|---|---|
| `00_prep.R` | Shared data preparation: the .qmd's import, QC, hourly rounding and met join, as functions with switchable QC and flux definition |
| `01_baseline.R` | Reproduces the submitted numbers from the .qmd logic |
| `02_afm_analyses.R` | GAM, CI and TOST for the method offset, variance components, array agreement, diel cycle, common-window Q10, sampling effort, QC diagnostics, and the sensitivity table |
| `03_pressure.R` | In-chamber LPS22 pressure vs HF001 `bar` |
| `04_local_soil_temperature.R` | Autochamber soil probes vs HF001 `s10t`; Q10 with local temperature |
| `05_figures.R` | Figs 2–10 |
| `06_uptime.R` | Re-runs `sys_performance/system_performance.R` (uptime and coverage numbers) |

Data added under `data/`:
- `hf001-10-15min-m_2023.csv`: the 2023 extract of the Fisher met station record, so the analysis runs offline.
- `HFauto_site{1,2}_2023October_tsoil.csv`: per-chamber autochamber soil temperature (HF293 processed files), recovered from commit `e99e240`.

## Differences from the submitted (.qmd) analysis

1. **`filter_iqr.R`** had been moved to `deprecated/`, which broke the .qmd. It is restored to `manuscript_code/`, minus the example line that errored when sourced.
2. **Met time zone.** HF001 timestamps are EST year-round. The .qmd parsed them in the machine's local time zone, so the soil-temperature join depended on the computer it ran on. `America/New_York` reproduces the submitted numbers exactly. The revision parses in EST (`Etc/GMT+5`), which moves the method offset from −0.332 to −0.330 and Q10 from 2.31/2.86 to 2.28/2.83 (full record).
3. **GAM random effect.** `s(id, bs = "fs")` with no covariate is now `s(id, bs = "re")`. The hour smooth is cyclic (`bs = "cc"`). The estimates are unchanged to 3 decimals.
4. **Fig 5 / CCC hour filter.** The .qmd kept hours in which one system × stand combination had no chambers at all (`all()` of an empty vector is `TRUE`). It also dropped hours with autochamber stand means ≤ 1, and its 3-h rolling mean ran across data gaps. Enforcing ≥ 5 chambers per system × stand and rolling on a complete hourly grid changes the CCC from 0.70 to 0.57 (95% CI 0.50–0.64).
5. **Table 2 filter.** `filter(timeofday == c("morning", "evening"))` recycled the vector and dropped about half the rows. Fixed with `%in%` in both the .qmd and here. Means and medians barely change; the n values double. The Ecosphere value "5.28" (stand-2 autochamber evening median) is 2.58.
6. **Q10** is fit on stand-hours in which both systems reported (a common window, 11.2–19.1 °C). The flux > 0.5 filter is dropped (flux > 0 is kept, as required by the exponential model), and 95% CIs are reported.

## Findings the co-authors should see

- **Pressure.** HF001 `bar` is reduced to sea level (Oct 2023 mean 1017 hPa). The in-chamber LPS22 sensors read 977 hPa at the stands. Both systems' fluxes used HF001 `bar`, so absolute fluxes are overestimated by about 4% (factor 0.961 ± 0.008). The inter-system comparison is essentially unaffected (sensitivity table).
- **Linear vs quadratic flux.** All analyses use the linear slope (`fluxL`). With the quadratic initial slope the Fluxbot offset changes sign (+0.57, 95% CI −0.04 to 1.19) and the CCC falls to 0.15. The Fluxbot CSV's `final_flux_umolm2sec` column is the quadratic value.
- **Autochamber clock.** The autochamber diel peak is 1 h earlier than the Fluxbot peak, which would be consistent with an EST logger clock. Shifting the autochambers by +1 h changes no conclusion (sensitivity table). Still worth confirming with Mark Van Scoy.
- **Autochamber flux inputs.** The autochamber fluxes were computed with HF001 air temperature (not chamber temperature), the fitting window started 90 s after the start of each 5-min slot (a 45 s dead band after lid closure, then a 3.5-min fit), and no water-vapour correction was applied.
