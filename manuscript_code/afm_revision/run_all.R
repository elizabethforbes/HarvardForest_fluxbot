# Run the full AFM-revision analysis from manuscript_code/:
#   Rscript afm_revision/run_all.R
# Each script runs in a fresh R session. Order matters: 10 computes all fluxes from
# data_package/; 02 writes the analysis dataset used by 04-09.
# Set SKIP_FLUXES=1 to reuse existing outputs/afm_revision/fluxes/*.csv.
scripts <- c("10_fluxes.R", "01_baseline.R", "03_pressure.R", "02_afm_analyses.R",
             "04_local_soil_temperature.R", "05_figures.R", "06_uptime.R", "07_agreement_metrics.R", "08_chamber_physics.R", "09_flux_model_matrix.R")
if (Sys.getenv("SKIP_FLUXES") == "1") scripts <- setdiff(scripts, "10_fluxes.R")
for (s in scripts) {
  message("== ", s)
  status <- system2("Rscript", file.path("afm_revision", s), stdout = FALSE)
  if (status != 0) stop(s, " failed")
}
# collect every number into one file
files <- c("numbers_baseline.csv", "numbers_A7_pressure.csv", "numbers_for_text.csv", "numbers_A7_soiltemp.csv", "numbers_uptime.csv", "numbers_agreement.csv", "numbers_chamber_physics.csv", "numbers_flux_model.csv")
all <- do.call(rbind, lapply(file.path("outputs", "afm_revision", files), read.csv))
write.csv(all, file.path("outputs", "afm_revision", "numbers_all.csv"), row.names = FALSE)
message("Done: ", nrow(all), " numbers in outputs/afm_revision/numbers_all.csv")
