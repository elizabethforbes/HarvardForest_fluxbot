# Re-run the uptime / coverage numbers (manuscript "System uptime" section and
# Discussion 4.1) with the objects the sys_performance scripts expect from the .qmd.

source("afm_revision/00_prep.R")
HF_fluxestimates <- load_fluxbot() %>% rename(fluxL_umolm2sec = flux)
HFauto_fluxestimates <- load_autochamber() %>% rename(fluxL_umolm2sec = flux)
HF_fluxestimates_filtered <- apply_qc(load_fluxbot(), "iqr") %>% rename(fluxL_umolm2sec = flux)
HFauto_fluxestimates_filtered <- apply_qc(load_autochamber(), "iqr") %>% rename(fluxL_umolm2sec = flux)
out <- capture.output(source("sys_performance/system_performance.R", echo = FALSE))
writeLines(out, file.path(out_dir, "system_performance_log.txt"))
cat(out, sep = "\n")
