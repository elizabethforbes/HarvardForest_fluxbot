# Reproduce every flux, dataset, number, figure and table in the paper from the raw data in
# ../data_package/. Run from manuscript_code/:
#   Rscript run_all.R                  # everything (~11 min)
#   SKIP_FLUXES=1 Rscript run_all.R    # reuse ../data_clean/fluxes/ (skips 1_clean/01 and 02)
# Each script runs in a fresh R session, in the order below; a script's console output goes to
# outputs/logs/. Stages:
#   1_clean      raw records -> per-closure fluxes -> QC'd closures and chamber-hour datasets (../data_clean/)
#   2_analysis   statistics; read ../data_clean/ (and, for sensor-level diagnostics, ../data_package/)
#   3_lab_tests  the two laboratory tests of wet K30 sensors (../data_package/raw/lab_*)
#   4_figures    main and SI figures (outputs/figures/)
#   5_tables     SI tables (outputs/si_tables/)
# Every number quoted in the paper is written to outputs/numbers/ by the script that computes it
# and collected here into outputs/numbers/numbers_all.csv (key, value, section, note, script).

stages <- c("1_clean", "2_analysis", "3_lab_tests", "4_figures", "5_tables")
scripts <- unlist(lapply(stages, function(s) file.path(s, sort(list.files(s, pattern = "\\.R$")))))
if (Sys.getenv("SKIP_FLUXES") == "1") scripts <- setdiff(scripts, c("1_clean/01_fluxes.R", "1_clean/02_fluxes_window56.R"))

dir.create(file.path("outputs", "logs"), showWarnings = FALSE, recursive = TRUE)
num_dir <- file.path("outputs", "numbers")
unlink(list.files(num_dir, full.names = TRUE))          # numbers are rewritten by this run only
t_start <- Sys.time()
for (s in scripts) {
  t0 <- Sys.time()
  log <- file.path("outputs", "logs", paste0(gsub("/", "__", sub("\\.R$", "", s)), ".log"))
  status <- system2("Rscript", s, stdout = log, stderr = log)
  message(sprintf("%-45s %6.1f s%s", s, as.numeric(difftime(Sys.time(), t0, units = "secs")), if (status != 0) "  FAILED" else ""))
  if (status != 0) stop(s, " failed; see ", log)
}

# collect every number, in run order
files <- file.path(num_dir, paste0(gsub("/", "__", sub("\\.R$", "", scripts)), ".csv"))
files <- files[file.exists(files)]
all <- do.call(rbind, lapply(files, read.csv))
if (anyDuplicated(all$key)) warning("duplicated number keys: ", paste(unique(all$key[duplicated(all$key)]), collapse = ", "))
write.csv(all, file.path(num_dir, "numbers_all.csv"), row.names = FALSE)
writeLines(capture.output(sessionInfo()), file.path("outputs", "logs", "sessionInfo.txt"))
message("Done in ", round(as.numeric(difftime(Sys.time(), t_start, units = "mins")), 1), " min: ", nrow(all),
        " numbers in outputs/numbers/numbers_all.csv")
