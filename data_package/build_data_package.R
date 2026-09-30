# Build the data package from the original sources.
#
# This is the ONLY script that reads files outside the repository. Everything in
# manuscript_code/ reads data_package/ only. Run from the repository root:
#   FLUXBOT_DRIVE="<path to 'Fluxbot 2023' Drive folder>" Rscript data_package/build_data_package.R
#
# Sources
#  - Fluxbot sensor records: Google-Sheets exports (one tab per unit, one row per hourly
#    transmission holding arrays of device timestamps and readings), in
#    "old R scripts/fluxbot data 2023/fluxbot_{co2,temperature,humidity,pressure}.xlsx"
#    (identical, by md5, to the copies in the Drive folder "manuscripts/Fluxbot data, code").
#  - Autochamber raw CO2 (1 Hz, analyzer output per chamber): Drive
#    "manuscripts/autochamber data, code/site{1,2}_October_RawData.csv", written by
#    "AutoChamber Code_AS.R" from the Campbell loggers' .dat files.
#  - Fluxbot GPS positions: Drive "Harvard Forest 2023/fluxbot locations/Harvard Forest 2023.csv".
#  - Autochamber geometry: collar heights, lid and system volume from
#    "old R scripts/HFarray_fluxbotcalcs_final.Rmd.qmd" (lines 26-40).
#  - HF001 Fisher met station (15-min), Harvard Forest Data Archive.
#  - HF293-07 autochamber soil respiration and soil temperature as published by the
#    Harvard Forest team (used as an independent reference and for local soil temperature).

suppressPackageStartupMessages({ library(dplyr); library(readxl); library(stringr); library(readr) })

drive <- Sys.getenv("FLUXBOT_DRIVE", file.path(Sys.getenv("HOME"),
  "Library/CloudStorage/GoogleDrive-jonathan.gewirtzman@yale.edu/.shortcut-targets-by-id",
  "1xCZfEjQvIvJrJ6t9FWhHFmNa01CDMpU5/Fluxbot 2023"))
pkg <- "data_package"
for (d in c("raw", "metadata", "ancillary")) dir.create(file.path(pkg, d), showWarnings = FALSE, recursive = TRUE)

# ---- Fluxbot units ---------------------------------------------------------------------------
gps <- read_csv(file.path(drive, "Harvard Forest 2023/fluxbot locations/Harvard Forest 2023.csv"), show_col_types = FALSE) %>%
  mutate(stand_code = str_extract(Name, "healthy|unhealthy"), unit = str_extract(Name, "\\d+$"))
units <- gps %>% transmute(unit, stand_code, stand = if_else(stand_code == "healthy", "stand 1", "stand 2"),
                           site_name = if_else(stand_code == "healthy", "Bigelow Brook", "Hemlock tower"),
                           latitude = round(Latitude, 6), longitude = round(Longitude, 6),
                           gps_date = as.Date(Date), chamber_volume_cm3 = 768, collar_area_cm2 = 81,
                           collar_inner_diameter_cm = 10.2, collar_insertion_cm = 4, co2_sensor = "Senseair K30",
                           notes = if_else(unit == "112", "pressure sensor faulty; excluded from submitted analysis", NA_character_)) %>%
  arrange(stand_code, as.integer(unit))
write_csv(units, file.path(pkg, "metadata", "fluxbot_units.csv"))

# ---- Fluxbot sensor records --------------------------------------------------------------------
xdir <- file.path("old R scripts", "fluxbot data 2023")
parse_vec <- function(x) suppressWarnings(as.numeric(str_split(str_remove_all(x, "\\[|\\]"), ",")[[1]]))
read_var <- function(file, col, name) {
  f <- file.path(xdir, file); sh <- excel_sheets(f); sh <- sh[str_extract(sh, "\\d+$") %in% units$unit]
  bind_rows(lapply(sh, function(s) {
    x <- read_excel(f, sheet = s)
    bind_rows(lapply(seq_len(nrow(x)), function(i) {
      t <- parse_vec(x[["device timestamps"]][i]); v <- parse_vec(x[[col]][i]); n <- min(length(t), length(v))
      tibble(unix_time = t[seq_len(n)], value = v[seq_len(n)])
    })) %>% mutate(unit = str_extract(s, "\\d+$"))
  })) %>% distinct(unit, unix_time, .keep_all = TRUE) %>% rename(!!name := value)
}
fb <- read_var("fluxbot_co2.xlsx", "co2", "co2_ppm") %>%
  full_join(read_var("fluxbot_temperature.xlsx", "temprerature", "air_temp_c"), by = c("unit", "unix_time")) %>%
  full_join(read_var("fluxbot_humidity.xlsx", "humidity", "rh_pct"), by = c("unit", "unix_time")) %>%
  full_join(read_var("fluxbot_pressure.xlsx", "pressure", "pressure_hpa"), by = c("unit", "unix_time")) %>%
  filter(unix_time >= as.numeric(as.POSIXct("2023-09-25", tz = "UTC")),
         unix_time <  as.numeric(as.POSIXct("2023-11-06", tz = "UTC"))) %>%
  arrange(unit, unix_time) %>%
  mutate(datetime_utc = format(as.POSIXct(unix_time, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")) %>%
  select(unit, unix_time, datetime_utc, co2_ppm, air_temp_c, rh_pct, pressure_hpa)
write_csv(fb, file.path(pkg, "raw", "fluxbot_sensor_records_2023.csv.gz"))
message("Fluxbot records: ", nrow(fb))

# ---- Autochamber raw CO2 --------------------------------------------------------------------------
ac <- bind_rows(lapply(1:2, function(s)
  read_csv(file.path(drive, sprintf("manuscripts/autochamber data, code/site%d_October_RawData.csv", s)),
           col_types = cols(datetime = col_character(), chamber = col_integer(), CO2_ppm = col_double())))) %>%
  # midnight rows were written without a time component
  mutate(datetime = if_else(nchar(datetime) == 10, paste(datetime, "00:00:00"), datetime)) %>%
  rename(datetime_est = datetime, co2_ppm = CO2_ppm) %>% arrange(chamber, datetime_est)
write_csv(ac, file.path(pkg, "raw", "autochamber_co2_1hz_oct2023.csv.gz"))
message("Autochamber records: ", nrow(ac))

heights <- c(4.85, 4.9, 5.85, 4.45, 5.75, 5.15, 4.8, 4.3, 5.50, 4.3, 3.6, 4)
chambers <- tibble(chamber = 1:12, logger_site = rep(c("site1", "site2"), each = 6),
                   stand = rep(c("stand 2", "stand 1"), each = 6), stand_code = rep(c("unhealthy", "healthy"), each = 6),
                   site_name = rep(c("Hemlock tower", "Bigelow Brook"), each = 6),
                   analyzer = rep(c("LI-840", "LI-800"), each = 6),
                   collar_height_cm = heights, collar_area_cm2 = 646.328,
                   lid_volume_cm3 = 646.328 * 9.9, system_volume_cm3 = 274.14) %>%
  mutate(total_volume_cm3 = round(collar_height_cm * collar_area_cm2 + lid_volume_cm3 + system_volume_cm3, 3),
         slot_minute = (chamber - 1) %% 6 * 5)
write_csv(chambers, file.path(pkg, "metadata", "autochamber_chambers.csv"))

# ---- ancillary ----------------------------------------------------------------------------------------
met <- read_csv("https://harvardforest.fas.harvard.edu/data/p00/hf001/hf001-10-15min-m.csv", col_types = cols(.default = col_character())) %>%
  filter(substr(datetime, 1, 4) == "2023")
write_csv(met, file.path(pkg, "ancillary", "hf001-10-15min-m_2023.csv"))
hf293 <- read_csv("https://harvardforest.fas.harvard.edu/data/p29/hf293/hf293-07-soil-resp-2022-2023.csv", col_types = cols(.default = col_character())) %>%
  filter(year == 2023)
write_csv(hf293, file.path(pkg, "ancillary", "hf293-07-soil-resp-2023.csv"))
message("Done.")
