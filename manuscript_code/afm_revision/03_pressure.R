# A7: sensitivity of fluxes to the pressure used in the ideal-gas conversion.
# Fluxes were computed with HF001 `bar` from the Fisher met station. That series
# is reduced to sea level (Oct 2023 median ~1016 hPa), whereas the in-chamber
# LPS22 sensors on each Fluxbot record station pressure at the stands (~980 hPa).
# Flux scales linearly with the pressure used, so the correction factor for each
# measurement is P_local / P_HF001.

source("afm_revision/00_prep.R")
suppressPackageStartupMessages({ library(readxl); library(stringr) })

hf_bots <- c(healthy = "100|13|22|114|108|101|112|111",
             unhealthy = "113|103|102|105|24|106|104|110")
f <- file.path("..", "old R scripts", "fluxbot data 2023", "fluxbot_pressure.xlsx")

parse_vec <- function(x) as.numeric(str_split(str_remove_all(x, "\\[|\\]"), ",")[[1]])
read_bot <- function(sheet) {
  x <- read_excel(f, sheet = sheet)
  bind_rows(lapply(seq_len(nrow(x)), function(i) {
    t <- parse_vec(x$`device timestamps`[i]); p <- parse_vec(x$pressure[i])
    n <- min(length(t), length(p))
    data.frame(unix = t[seq_len(n)], pressure = p[seq_len(n)])
  })) %>% mutate(bot = str_extract(sheet, "\\d+$"))
}
sheets <- excel_sheets(f)
bots <- unlist(str_split(hf_bots, "\\|"))
sheets <- sheets[str_extract(sheets, "\\d+$") %in% bots]
p <- bind_rows(lapply(sheets, read_bot)) %>%
  mutate(time = as.POSIXct(unix, origin = "1970-01-01", tz = "America/New_York"),
         stand = if_else(grepl(paste0("^(", hf_bots["healthy"], ")$"), bot), "healthy", "unhealthy")) %>%
  filter(time >= as.POSIXct("2023-10-01", tz = "America/New_York"),
         time <  as.POSIXct("2023-11-05", tz = "America/New_York"),
         pressure > 900, pressure < 1100)

# hourly median per bot, then per-stand array median across bots that pass QC
# (the .qmd's QC range 965-1050 hPa; bot 112 had a faulty pressure sensor)
hourly <- p %>% mutate(hour_of_obs = floor_date(time, "hour")) %>%
  group_by(stand, bot, hour_of_obs) %>% summarise(p = median(pressure), .groups = "drop")
bot_offsets <- hourly %>% group_by(stand, hour_of_obs) %>% mutate(dev = p - median(p)) %>%
  group_by(stand, bot) %>% summarise(median_dev_hPa = median(dev), n_hours = n(), .groups = "drop")
write.csv(bot_offsets, file.path(out_dir, "pressure_bot_offsets.csv"), row.names = FALSE)

stand_p <- hourly %>% filter(p > 965, p < 1050, bot != "112") %>%
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
print(write_numbers("numbers_A7_pressure.csv"), row.names = FALSE)
