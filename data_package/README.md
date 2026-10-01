# Data package: Fluxbot 2.0 and autochamber soil CO2 fluxes, Harvard Forest, October 2023

This folder holds every input needed to reproduce the analysis. `manuscript_code/1_clean/` turns these raw files into the cleaned datasets in `data_clean/`, which every analysis reads. The same files are intended for the Zenodo archive (concept DOI https://doi.org/10.5281/zenodo.15660442, which resolves to the latest version).

To reproduce all fluxes, statistics and figures, run from `manuscript_code/`:

```
Rscript run_all.R
```

This takes about 11 minutes. `1_clean/01_fluxes.R` recomputes every closure from the raw records with goFlux.

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

## Known issues in the raw records (handled in `manuscript_code/1_clean/01_fluxes.R`)

- Fluxbot CO2 = 65535 or 65533 are transmission and read error codes. These rows are removed as whole rows.
- Fluxbot air temperature = 44755.7 °C and RH = 25600.4 % are failed sensor reads (0xFFFF). They are treated as missing.
- The Fluxbot unit 100 pressure sensor reads ~22 hPa low, and unit 112's pressure sensor was reported faulty. Station pressure is taken as the per-stand median of the remaining sensors.
- Fluxbot units 24, 111, 114 and 13 have many closures in which CO2 falls: the lid did not seal, or did not vent between closures. These closures are excluded as chamber failures.
- The autochamber logger clock is EST. It matches the HF293 record exactly.
- HF001 `bar` is sea-level pressure, 3.9% above station pressure at the stands.

## Sources and credit

Fluxbot data: Forbes, Gewirtzman et al. (this study).

Autochamber raw data: Harvard Forest hemlock autochamber network (HF293; A. Finzi, A. Keiser, M.-A. Giasson, M. Nieland). Field operations and maintenance: M. Van Scoy.

HF001: Boose & VanScoy (2025), Harvard Forest Data Archive HF001 (v.34), https://doi.org/10.6073/pasta/7a7ffd2eaa2ea9965d701998d4e2b1f5.

HF293: Finzi, Keiser, Giasson & Nieland (2026), Harvard Forest Data Archive HF293 (v.10), https://doi.org/10.6073/pasta/a4ebf6b3eb19d832fcf09c81ec6dfa73.

## Laboratory test of the K30 PTFE envelope (raw/lab_ptfe_test_2023-09-22/)

A growth chamber test on 22 September 2023: ~90% RH, CO2 setpoint 900 ppm.

| File | Contents |
|---|---|
| `co2_coveredPTFE_k30_22Sept2023.txt` | PTFE-covered K30 (`HH:MM:SS, ppm`; 65535 = error) |
| `co2_uncoveredk30_22Sept2023.txt` | Uncovered K30 (same format) |
| `lgr_2023-09-22.csv.gz` | LGR analyzer CO2 and H2O, 1 Hz. The LGR clock runs 116 s fast: LGR 17:54:53 = real 17:52:57. |

Event log (lab notes):

| Time | Event |
|---|---|
| 13:36 | Door closed; CO2 injection off |
| 13:45 | CO2 on |
| 14:00 | Door opened for 1 min |
| 17:32 | Door opened; covered sensor sprayed with water (water beaded on the PTFE); door left open until CO2 plateaued, then closed for ~3 min |
| 17:43 | Door opened (CO2 fell to ~670 ppm); breath spike to ~900 ppm; door closed until plateau |
| 17:49 | SD cards collected |

## Laboratory test of K30 covers (raw/lab_cover_test_2023-12-13/)

A test in a CO2 rig (Raymond lab, Yale) on 13 December 2023. Three uncovered K30s (controls c1–c3) and two covered K30s (t1, t2) logged every ~6 s in one chamber next to an LGR analyzer. CO2 was varied with breath pulses and door openings.

| File | Contents |
|---|---|
| `k30_control_c1.txt`, `k30_control_c2.txt`, `k30_control_c3.txt` | Uncovered K30s (`HH:MM:SS, ppm`; 65535 = error). Originally `group1_control/co2 2.txt`, `co2 3.txt`, `co2.txt`. |
| `k30_test_t1.txt`, `k30_test_t2.txt` | Covered K30s. Originally `group2_test/co2 2.txt`, `co2.txt`. |
| `lgr_2023-12-13.csv.gz` | LGR CO2 (wet and dry) and H2O, 1 Hz, from `LGR3_raw/2023-12-13/micro_2023-12-13_f0000/f0001.txt` |

Each K30 file holds several logging sessions, each starting with `EXPERIMENT BEGIN`. Only the last session belongs to this run; it starts at 11:12:11 on all loggers. The LGR clock is 130 s ahead of the K30 loggers, and the lag is constant over the day.

Sequence, from J. Gewirtzman's messages of 13 Dec 2023:
1. dry PTFE envelope
2. wet PTFE envelope
3. dry 3D-printed bracket
4. wet 3D-printed bracket
5. bare K30s sprayed with water (they became erratic and stopped recording)

The notes give no times. In `19_cover_test_dec.R` the phase boundaries are set from the LGR H2O record (wetting at 16:40 and 17:43) and from the covered sensors stopping (18:06).
