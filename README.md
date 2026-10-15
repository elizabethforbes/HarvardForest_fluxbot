# Fluxbot 2.0 and autochamber soil CO2 fluxes, Harvard Forest, October 2023

Data and code for the manuscript "Low-cost autonomous chambers enable high spatial and temporal resolution monitoring of soil CO2 efflux across landscapes" (Gewirtzman et al.). The study compares two arrays of eight low-cost, autonomous Fluxbot 2.0 chambers with the Harvard Forest autochamber network in two hemlock stands from 2 to 31 October 2023.

## Contents

| Folder | Contents |
|---|---|
| `data_package/` | Raw data: every Fluxbot sensor record, 1 Hz autochamber CO2, unit and chamber metadata, HF001 met and HF293 extracts, and raw data from two laboratory tests of the Fluxbot CO2 sensor. See its README and `data_dictionary.csv`. |
| `data_clean/` | Cleaned datasets generated from the raw data: per-closure fluxes, the QC status of every closure, and the chamber-hour datasets used by all analyses. See its README. |
| `manuscript_code/` | Analysis code. `run_all.R` runs, in order: `1_clean/` (fluxes and data hygiene), `2_analysis/`, `3_lab_tests/`, `4_figures/` (one script per figure), `5_tables/`. Outputs, including every number quoted in the paper, are in `manuscript_code/outputs/`. See its README. |
| `legacy/` | Repository only (not in the Zenodo archive): earlier scripts and the raw spreadsheet exports from which `data_package/` was assembled by `data_package/build_data_package.R`. Not used by the analysis. |

## Reproducing the analysis

Requires R 4.4.3 and the package versions in `manuscript_code/renv.lock` (including goFlux 0.4.0).

```
cd manuscript_code
Rscript run_all.R                  # everything, about 11 min
SKIP_FLUXES=1 Rscript run_all.R    # reuse the computed fluxes in data_clean/fluxes/
```

## Archive and citation

Data, cleaned datasets, code and results are archived on Zenodo: https://doi.org/10.5281/zenodo.15660442 (concept DOI; resolves to the latest version). Please cite the archive and the paper. The HF001 and HF293 extracts in `data_package/ancillary/` are from the Harvard Forest Data Archive; cite those datasets as listed in `data_package/README.md`.

## License

Code is released under the MIT License (`LICENSE`). Data, cleaned datasets and outputs are released under the Creative Commons Attribution 4.0 International license (CC BY 4.0).

## Contact

Elizabeth Forbes and Jonathan Gewirtzman. Please open an issue on this repository for questions about the data or code.
