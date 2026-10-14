# Harvard Forest: fluxbot project 2023
## Repository for fluxbot data collected in fall 2023 at Harvard Forest and associated datasets (LGR, auto-chambers, etc.)

This repository contains the following datasets:
- Harvard Forest fluxbot array, fall 2023 (16 total fluxbots deployed from late September 2023 to early November 2023)
- LGR (sensor testing in the growth chamber, PTFE moisture testing)
- auto-chamber array (existing autochamber setup in healthy, unhealthy hemlock forest at Harvard Forest, long-term installation)
- Harvard Forest weather data (meta-data with which to interpret flux estimates)

This repository contains the data and code for the manuscript "Low-cost autonomous chambers enable high spatial and temporal resolution monitoring of soil CO2 efflux across landscapes", which compares two arrays of low-cost Fluxbot 2.0 chambers with the Harvard Forest autochamber network.

## Reproducing the analysis (Agricultural and Forest Meteorology manuscript)

| Folder | Contents |
|---|---|
| `data_package/` | Raw data: every Fluxbot sensor record, 1 Hz autochamber CO2, unit and chamber metadata, HF001 met and HF293 extracts, lab-test raw data. See its README. |
| `data_clean/` | Cleaned datasets generated from the raw data: per-closure fluxes, the QC status of every closure, and the chamber-hour datasets used by all analyses. See its README. |
| `manuscript_code/` | Analysis code. `run_all.R` runs, in order: `1_clean/` (fluxes and data hygiene), `2_analysis/`, `3_lab_tests/`, `4_figures/` (one script per figure), `5_tables/`. See its README. |
| `legacy/` | Earlier scripts and the raw spreadsheet exports from which `data_package/` was assembled (`data_package/build_data_package.R`). Not used by the analysis pipeline. |

```
cd manuscript_code
Rscript run_all.R
```

