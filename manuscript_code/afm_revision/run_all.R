# Run the full AFM-revision analysis from manuscript_code/:
#   Rscript afm_revision/run_all.R
# Each script runs in a fresh R session. Order matters: 03 (pressure) writes an
# input used by 02's sensitivity table; 02 writes the dataset used by 04-05.
scripts <- c("01_baseline.R", "03_pressure.R", "02_afm_analyses.R",
             "04_local_soil_temperature.R", "05_figures.R", "06_uptime.R")
for (s in scripts) {
  message("== ", s)
  status <- system2("Rscript", file.path("afm_revision", s), stdout = FALSE)
  if (status != 0) stop(s, " failed")
}
# collect every number into one file
files <- c("numbers_baseline.csv", "numbers_A7_pressure.csv", "numbers_for_text.csv", "numbers_A7_soiltemp.csv", "numbers_uptime.csv")
all <- do.call(rbind, lapply(file.path("outputs", "afm_revision", files), read.csv))
write.csv(all, file.path("outputs", "afm_revision", "numbers_all.csv"), row.names = FALSE)
message("Done: ", nrow(all), " numbers in outputs/afm_revision/numbers_all.csv")
