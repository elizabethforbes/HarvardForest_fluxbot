# AFM revision analyses

The whole workflow reads only `../data_package/`. See the README there for the input files. Run from `manuscript_code/`:

```
Rscript afm_revision/run_all.R            # everything, ~10 min
SKIP_FLUXES=1 Rscript afm_revision/run_all.R   # reuse outputs/afm_revision/fluxes/
```

Outputs go to `outputs/afm_revision/`:
- `fluxes/`: per-closure fluxes for both systems.
- `numbers_all.csv`: every number quoted in the text, as key, value, section, note.
- `figures/`: vector PDF and 600 dpi TIFF, sized to Elsevier column widths.

R packages: dplyr, tidyr, readr, lubridate, fuzzyjoin, goFlux (0.4.0), mgcv, lme4, zoo, epiR, ineq, ggplot2, patchwork, ragg.

| Script | What it does |
|---|---|
| `10_fluxes.R` | Computes fluxes for BOTH systems from the raw records with one procedure (goFlux LM and HM, `best.flux`); see the header for shared rules |
| `00_prep.R` | Shared functions: loaders (reprocessed or submitted fluxes), QC rules, hourly assembly, met join |
| `01_baseline.R` | Reproduces the submitted (Ecosphere) numbers exactly from the submitted flux files |
| `02_afm_analyses.R` | GAM system effect (CI, TOST), variance components, array agreement, diel cycle, Q10, sampling effort, sensitivity table |
| `03_pressure.R` | In-chamber station pressure vs HF001 sea-level pressure |
| `04_local_soil_temperature.R` | Autochamber soil probes (HF293) vs HF001; Q10 with local temperature |
| `05_figures.R` | Figs 2–10 |
| `06_uptime.R` | Data collection and replicated coverage |
| `07_agreement_metrics.R` | Agreement-metric panel, spatial-sampling benchmark, random-slope Q10, diel shape, ratio drivers |
| `08_chamber_physics.R` | In-chamber temperature, comparison with HF293-published fluxes, chamber-level Q10 bootstrap, antecedent rain |
| `09_flux_model_matrix.R` | System offset under every combination of flux model (LM, HM, best, HF293) |

## Main-analysis choices
- **Flux:** goFlux `best.flux`: the Hutchinson–Mosier model where curvature is supported, otherwise linear.
- **Station pressure:** from the Fluxbot pressure sensors, for both systems.
- **Clocks:**
  - Fluxbot timestamps are device UNIX times, which are absolute.
  - Autochamber logger times are EST.
- **QC ("fit"):**
  - Drop closures with a significant CO2 decline, which are chamber failures.
  - Drop per-chamber spikes beyond median ± 5 MAD.
  - No value-based trimming of the pooled data.
- **Analysis period:** 2 Oct–4 Nov 2023. Autochambers stop on 31 Oct.

## What changed from the submitted analysis
1. **The two systems used different flux code.** The Fluxbot function had two data-handling bugs:
   - CO2 error-code rows were removed from the CO2 vector only, so R recycled it against the timestamps (24% of closures).
   - Failed temperature reads (44,755 °C) entered the gas law (11% of closures).
   - The autochamber copy joined met data with a ~4 h lag.
   All of this is replaced by `10_fluxes.R`.
2. **Autochamber clock.** It was read as EDT; it is EST, so the submitted analysis placed autochambers 1 h early.
3. **Pressure.** HF001 sea-level pressure was used (fluxes ~4% high). Station pressure is used now.
4. **HF001 time zone.** It was parsed in the computer's local time zone; it is EST.
5. **Fig. 5 / CCC hour filter.** It accepted hours with a whole system × stand missing; it is now fixed.
6. **Table 2 filter.** It recycled a vector (`==` against a length-2 vector) and silently dropped half the rows; it is now fixed.
7. **Fluxbot sampling interval.** It is ~6 s, not 1 Hz.
