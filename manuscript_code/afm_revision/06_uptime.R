# Re-run the uptime / coverage numbers (manuscript "System uptime" section and
# Discussion 4.1) with the objects the sys_performance scripts expect from the .qmd.

source("afm_revision/00_prep.R")
HF_fluxestimates <- load_fluxbot() %>% rename(fluxL_umolm2sec = flux)
HFauto_fluxestimates <- load_autochamber() %>% rename(fluxL_umolm2sec = flux)
HF_fluxestimates_filtered <- apply_qc(load_fluxbot(), "fit") %>% rename(fluxL_umolm2sec = flux)
HFauto_fluxestimates_filtered <- apply_qc(load_autochamber(), "fit") %>% rename(fluxL_umolm2sec = flux)
out <- capture.output(source("sys_performance/system_performance.R", echo = FALSE))
writeLines(out, file.path(out_dir, "system_performance_log.txt"))
cat(out, sep = "\n")

# record the uptime numbers quoted in the text
for (v in c("fluxbot_collection_rate", "fluxbot_overall_success", "autochamber_collection_rate",
            "autochamber_overall_success", "fluxbot_collected", "fluxbot_retained",
            "autochamber_collected", "autochamber_retained"))
  if (exists(v)) record(v, get(v), "uptime")
if (exists("fluxbot_retained")) record("fluxbot_retention_rate", 100 * fluxbot_retained / fluxbot_collected, "uptime")
if (exists("autochamber_retained")) record("autochamber_retention_rate", 100 * autochamber_retained / autochamber_collected, "uptime")
# replicated coverage (>= 3 units per stand-hour), parsed from the log in stand order
cov <- as.numeric(sub(".*\\(([0-9.]+)% of total time\\).*", "\\1", grep("units: .*% of total time", out, value = TRUE)))
lab <- c("coverage3_fluxbot_stand1", "coverage3_fluxbot_stand2", "coverage3_autochamber_stand1", "coverage3_autochamber_stand2")
for (i in seq_along(cov)) record(lab[i], cov[i], "uptime", "hours with >= 3 units, % of 720")
write_numbers("numbers_uptime.csv")
