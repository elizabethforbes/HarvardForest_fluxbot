# Cleaned datasets: Fluxbot 2.0 and autochamber soil CO2 fluxes, Harvard Forest, October 2023

These files are generated from `../data_package/` (raw data) by `manuscript_code/1_clean/`. Every analysis reads them. Do not edit them by hand; to regenerate them, run from `manuscript_code/`:

```
Rscript run_all.R
```

Column definitions are in `data_dictionary.csv`.

The closure tables contain records that reached the flux-output stage. For intended, recorded and computed counts, see `manuscript_code/outputs/si_tables/TableS_accounting.csv`.

| File | Contents | Made by |
|---|---|---|
| `fluxes/fluxbot_fluxes.csv`, `fluxes/autochamber_fluxes.csv` | One row per closure that reached the flux-output stage, 25 Sep–5 Nov 2023, before QC. Includes linear and Hutchinson–Mosier fluxes (goFlux), fit statistics, gas-law inputs, and the wet-sensor and stuck-lid flags | `1_clean/01_fluxes.R` |
| `fluxes/fluxbot_fluxes_w56.csv` | Fluxbot fluxes with the 56:00–60:00 fit window (sensitivity analysis) | `1_clean/02_fluxes_window56.R` |
| `fluxes/flux_run_metadata.csv` | Instrument precision and the station/HF001 pressure ratio used by the flux run (`pressure_ratio_local_hf001`, 0.9597; the fallback when no LPS22 pressure was available) | `1_clean/01_fluxes.R` |
| `closures_fluxbot.csv`, `closures_autochamber.csv` | Computed closures in the analysis period (2–31 Oct 2023): flux columns, QC flags, `qc_status`, `in_deployed`, `in_screened` | `1_clean/03_clean_datasets.R` |
| `chamber_hours_screened.csv` | **Main analysis dataset ("RH-screened")**: one row per chamber-hour of retained closures, wet-sensor Fluxbot closures removed, with HF001 soil and air temperature, pressure and precipitation | `1_clean/03_clean_datasets.R` |
| `chamber_hours_deployed.csv` | As-deployed dataset (wet-sensor closures kept; sensitivity analysis) | `1_clean/03_clean_datasets.R` |
| `qc_log.csv` | Number of closures at each QC status, by system. The 661 wet-sensor Fluxbot closures are included in the 6,962 retained Fluxbot closures | `1_clean/03_clean_datasets.R` |

## Data-hygiene rules

Each closure's `qc_status` is the first rule that removed it:
1. no flux computed;
2. too few records;
3. chamber failure: stuck lid;
4. chamber failure: whole array (autochambers; three or more of the array's chambers failing in the same hour);
5. chamber failure: no CO2 accumulation;
6. chamber failure: CO2 decline;
7. spike (sensor artifact; outside the chamber's median ± 5 MAD).

Closures that pass every rule are **retained** (as deployed). The main dataset, **RH-screened**, also removes Fluxbot closures whose in-chamber RH was ≥ 99% in the open-lid minute before closure (wet K30 sensor). `manuscript_code/README.md` gives the full definitions.

## Times

- `start_local` is local time (America/New_York).
- `start_utc` and `hour_of_obs` are UTC (ISO 8601).
- The analysis period and day boundaries use local time.

## License

CC BY 4.0. Cite the Zenodo archive (https://doi.org/10.5281/zenodo.15660442) and the paper.
