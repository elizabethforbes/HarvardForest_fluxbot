# Data package: Fluxbot 2.0 and autochamber soil CO2 fluxes, Harvard Forest, October 2023

This folder holds every input needed to reproduce the analysis. The analysis code in `manuscript_code/afm_revision/` reads only this folder. The same files are intended for the Zenodo archive (https://doi.org/10.5281/zenodo.15660443).

To reproduce all fluxes, statistics and figures, run from `manuscript_code/`:

```
Rscript afm_revision/run_all.R
```

This takes a few minutes. `10_fluxes.R` recomputes every closure from the raw records with goFlux.

`build_data_package.R` rebuilds this folder from the original sources (Google Sheets exports, logger files, the Harvard Forest Data Archive). Maintainers need it only if the sources change.

## Files

| File | Contents |
|---|---|
| `raw/fluxbot_sensor_records_2023.csv.gz` | Every Fluxbot sensor record, 25 Sep–5 Nov 2023, for the 16 units at Harvard Forest: CO2, in-chamber air temperature, relative humidity and pressure. Records are every ~6 s, logged 54:00–60:00 of each hour. Values are as transmitted, error codes included. |
| `raw/autochamber_co2_1hz_oct2023.csv.gz` | Autochamber analyzer CO2, 1 Hz, October 2023, chambers 1–12 (chamber 0 = between-chamber flush). The logger clock is EST. |
| `metadata/fluxbot_units.csv` | Unit ID, stand, GPS position (17 Nov 2023), chamber volume and collar area |
| `metadata/autochamber_chambers.csv` | Chamber ID, stand, analyzer, collar height, total system volume, collar area, measurement slot |
| `ancillary/hf001-10-15min-m_2023.csv` | Fisher meteorological station (HF001), 15-min, 2023, EST. `bar` is reduced to sea level. |
| `ancillary/hf293-07-soil-resp-2023.csv` | Autochamber soil respiration (`rs`) and soil temperature (`tsoil`) as published by the Harvard Forest team (HF293-07), 2023, EST |

## Variables

See `data_dictionary.csv`.

## Known issues in the raw records (handled in `10_fluxes.R`)

- Fluxbot CO2 = 65535 or 65533 are transmission and read error codes. These rows are removed as whole rows.
- Fluxbot air temperature = 44755.7 °C and RH = 25600.4 % are failed sensor reads (0xFFFF). They are treated as missing.
- The Fluxbot unit 100 pressure sensor reads ~22 hPa low, and unit 112's pressure sensor was reported faulty. Station pressure is taken as the per-stand median of the remaining sensors.
- Fluxbot units 24, 111, 114 and 13 have many closures in which CO2 falls: the lid did not seal, or did not vent between closures. These closures are excluded as chamber failures.
- The autochamber logger clock is EST. It matches the HF293 record exactly.
- HF001 `bar` is sea-level pressure, 3.9% above station pressure at the stands.

## Sources and credit

Fluxbot data: Forbes, Gewirtzman et al. (this study).

Autochamber raw data: Harvard Forest hemlock autochamber network (A. Keiser, M. Nieland, A. Sow, M. Van Scoy).

HF001: Boose & VanScoy (2025), Harvard Forest Data Archive HF001.

HF293: Harvard Forest Data Archive HF293.
