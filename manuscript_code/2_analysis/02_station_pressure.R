# A7: pressure used in the ideal-gas conversion. The submitted fluxes used HF001 `bar`,
# which is reduced to sea level (Oct 2023 median ~1016 hPa); the LPS22 sensors in the
# Fluxbot chambers record station pressure at the stands (~977 hPa). Flux scales linearly
# with pressure, so P_local / P_HF001 is the size of the bias in the submitted fluxes.
# The reprocessed fluxes (1_clean/01_fluxes.R) use station pressure.

source("R/setup.R")
suppressPackageStartupMessages(library(readr))

units <- read_csv(file.path(pkg, "metadata", "fluxbot_units.csv"), col_types = cols(unit = col_character()))
p <- read_csv(file.path(pkg, "raw", "fluxbot_sensor_records_2023.csv.gz"), col_types = cols(unit = col_character())) %>%
  left_join(units %>% select(unit, stand = stand_code), by = "unit") %>%
  transmute(bot = unit, stand, pressure = pressure_hpa,
            time = as.POSIXct(unix_time, origin = "1970-01-01", tz = "America/New_York")) %>%
  filter(time >= as.POSIXct("2023-10-01", tz = "America/New_York"),
         time <  analysis_end,
         pressure > 900, pressure < 1100)

# hourly median per bot, then per-stand array median across bots that pass QC
# (QC range 965-1050 hPa; units whose median offset from the stand median exceeds 5 hPa are excluded)
hourly <- p %>% mutate(hour_of_obs = floor_date(time, "hour")) %>%
  group_by(stand, bot, hour_of_obs) %>% summarise(p = median(pressure), .groups = "drop")
bot_offsets <- hourly %>% group_by(stand, hour_of_obs) %>% mutate(dev = p - median(p)) %>%
  group_by(stand, bot) %>% summarise(median_dev_hPa = median(dev), n_hours = n(), .groups = "drop")
write.csv(bot_offsets, file.path(out_dir, "pressure_bot_offsets.csv"), row.names = FALSE)

bad <- bot_offsets$bot[abs(bot_offsets$median_dev_hPa) > 5]
stand_p <- hourly %>% filter(p > 965, p < 1050, !bot %in% bad) %>%
  group_by(stand, hour_of_obs) %>% summarise(p_local = median(p), n_bots = n(), .groups = "drop")

met <- load_met() %>% mutate(hour_of_obs = floor_date(Time, "hour")) %>%
  group_by(hour_of_obs) %>% summarise(p_hf001 = mean(bar), .groups = "drop")
cmp <- inner_join(stand_p, met, by = "hour_of_obs") %>%
  mutate(ratio = p_local / p_hf001, diff = p_local - p_hf001)
write.csv(cmp, file.path(out_dir, "pressure_local_vs_hf001_hourly.csv"), row.names = FALSE)

s <- cmp %>% group_by(stand) %>%
  summarise(mean_local = mean(p_local), mean_hf = mean(p_hf001), mean_ratio = mean(ratio),
            sd_ratio = sd(ratio), r = cor(p_local, p_hf001), n = n())
print(s)
record("pressure_hf001_mean_hPa", mean(cmp$p_hf001), "A7")
record("pressure_local_mean_hPa", mean(cmp$p_local), "A7", "median of in-chamber LPS22, per stand-hour")
record("pressure_ratio_mean", mean(cmp$ratio), "A7", "flux correction factor local/HF001")
record("pressure_ratio_sd", sd(cmp$ratio), "A7")
record("pressure_ratio_min", min(cmp$ratio), "A7")
record("pressure_ratio_max", max(cmp$ratio), "A7")
record("pressure_local_vs_hf_r", cor(cmp$p_local, cmp$p_hf001), "A7")
for (st in s$stand) record(paste0("pressure_ratio_", st), s$mean_ratio[s$stand == st], "A7")
saveRDS(stand_p, file.path(out_dir, "stand_pressure_hourly.rds"))
print(write_numbers(), row.names = FALSE)
