# Harvard Forest: fluxbot project 2023
## Repository for fluxbot data collected in fall 2023 at Harvard Forest and associated datasets (LGR, auto-chambers, etc.)

This repository contains the following datasets:
- Harvard Forest fluxbot array, fall 2023 (16 total fluxbots deployed from late September 2023 to early November 2023)
- LGR (sensor testing in the growth chamber, PTFE moisture testing)
- auto-chamber array (existing autochamber setup in healthy, unhealthy hemlock forest at Harvard Forest, long-term installation)
- Harvard Forest weather data (meta-data with which to interpret flux estimates)

The aim of this repository is to contain the data and code associated with a manuscript written by ANONYMIZED FOR REVIEW and which will demonstrate the utility of a low-cost DIY fluxbot array in detecting small-scale variability in soil carbon fluxes across heterogeneous forest contexts.

## Reproducing the analysis (Agricultural and Forest Meteorology manuscript)

| Folder | Contents |
|---|---|
| `data_package/` | Raw data: every Fluxbot sensor record, 1 Hz autochamber CO2, unit and chamber metadata, HF001 met and HF293 extracts, lab-test raw data. See its README. |
| `data_clean/` | Cleaned datasets generated from the raw data: per-closure fluxes, the QC status of every closure, and the chamber-hour datasets used by all analyses. See its README. |
| `manuscript_code/` | Analysis code. `run_all.R` runs, in order: `1_clean/` (fluxes and data hygiene), `2_analysis/`, `3_lab_tests/`, `4_figures/` (one script per figure), `5_tables/`. See its README. |
| `legacy/` | The originally submitted analysis code and flux files, and older scripts. They are kept for reference and are not used by the current pipeline, except to reproduce the submitted numbers. |

```
cd manuscript_code
Rscript run_all.R
```

