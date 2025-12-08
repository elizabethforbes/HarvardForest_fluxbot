# Complete statistics for both Fluxbot and Autochamber systems (October 2-31 only)
# Add this code at the end of your script

# =============================================================================
# FLUXBOT SYSTEM STATISTICS (October 2-31)
# =============================================================================

# 1. Filter fluxbot data to October 2-31
fluxbot_filtered <- HF_fluxestimates %>%
  mutate(date = as.Date(as.POSIXct(hour_of_obs))) %>%
  filter(date >= as.Date("2023-10-02") & date <= as.Date("2023-10-31"))

fluxbot_filtered_qc <- HF_fluxestimates_filtered %>%
  mutate(date = as.Date(as.POSIXct(hour_of_obs))) %>%
  filter(date >= as.Date("2023-10-02") & date <= as.Date("2023-10-31"))

fluxbot_units <- fluxbot_filtered %>%
  distinct(id) %>%
  nrow()

fluxbot_date_range <- data.frame(
  start_date = as.Date("2023-10-02"),
  end_date = as.Date("2023-10-31"),
  duration_days = 30  # October 2-31 = 30 days
)

# 2. Calculate potential fluxbot measurements (October 2-31)
fluxbot_hours_total <- fluxbot_date_range$duration_days * 24
fluxbot_potential_measurements <- fluxbot_units * fluxbot_hours_total

# 3. Fluxbot collected measurements (October 2-31)
fluxbot_collected <- nrow(fluxbot_filtered)

# 4. Fluxbot retained after QC (October 2-31)
fluxbot_retained <- nrow(fluxbot_filtered_qc)

# 5. Fluxbot collection and retention rates
fluxbot_collection_rate <- (fluxbot_collected / fluxbot_potential_measurements) * 100
fluxbot_retention_rate <- (fluxbot_retained / fluxbot_collected) * 100
fluxbot_overall_success <- (fluxbot_retained / fluxbot_potential_measurements) * 100

# =============================================================================
# AUTOCHAMBER SYSTEM STATISTICS (October 2-31)
# =============================================================================

# 1. Filter autochamber data to October 2-31
autochamber_filtered <- HFauto_fluxestimates %>%
  mutate(date = as.Date(as.POSIXct(hour_of_obs))) %>%
  filter(date >= as.Date("2023-10-02") & date <= as.Date("2023-10-31"))

autochamber_filtered_qc <- HFauto_fluxestimates_filtered %>%
  mutate(date = as.Date(as.POSIXct(hour_of_obs))) %>%
  filter(date >= as.Date("2023-10-02") & date <= as.Date("2023-10-31"))

autochamber_units <- autochamber_filtered %>%
  distinct(id) %>%
  nrow()

autochamber_date_range <- data.frame(
  start_date = as.Date("2023-10-02"),
  end_date = as.Date("2023-10-31"),
  duration_days = 30  # October 2-31 = 30 days
)

# 2. Calculate potential autochamber measurements (October 2-31)
autochamber_hours_total <- autochamber_date_range$duration_days * 24
autochamber_potential_measurements <- autochamber_units * autochamber_hours_total * 2  # 2 measurements per hour

# 3. Autochamber collected measurements (October 2-31)
autochamber_collected <- nrow(autochamber_filtered)

# 4. Autochamber retained after QC (October 2-31)
autochamber_retained <- nrow(autochamber_filtered_qc)

# 5. Autochamber collection and retention rates
autochamber_collection_rate <- (autochamber_collected / autochamber_potential_measurements) * 100
autochamber_retention_rate <- (autochamber_retained / autochamber_collected) * 100
autochamber_overall_success <- (autochamber_retained / autochamber_potential_measurements) * 100

# =============================================================================
# PRINT SUMMARY STATISTICS
# =============================================================================

cat("=============================================================================\n")
cat("FLUXBOT SYSTEM STATISTICS (October 2-31, 2023)\n")
cat("=============================================================================\n")
cat("Number of units:", fluxbot_units, "\n")
cat("Deployment period:", format(fluxbot_date_range$start_date, "%B %d"), "-", 
    format(fluxbot_date_range$end_date, "%B %d, %Y"), 
    "(", fluxbot_date_range$duration_days, "days)\n")
cat("Potential measurements:", format(fluxbot_potential_measurements, big.mark = ","), "\n")
cat("Collected measurements:", format(fluxbot_collected, big.mark = ","), "\n")
cat("Retained after QC:", format(fluxbot_retained, big.mark = ","), "\n")
cat("Collection rate:", round(fluxbot_collection_rate, 1), "%\n")
cat("QC retention rate:", round(fluxbot_retention_rate, 1), "%\n")
cat("Overall success rate:", round(fluxbot_overall_success, 1), "%\n")

cat("\n=============================================================================\n")
cat("AUTOCHAMBER SYSTEM STATISTICS (October 2-31, 2023)\n")
cat("=============================================================================\n")
cat("Number of units:", autochamber_units, "\n")
cat("Deployment period:", format(autochamber_date_range$start_date, "%B %d"), "-", 
    format(autochamber_date_range$end_date, "%B %d, %Y"), 
    "(", autochamber_date_range$duration_days, "days)\n")
cat("Potential measurements:", format(autochamber_potential_measurements, big.mark = ","), "\n")
cat("Collected measurements:", format(autochamber_collected, big.mark = ","), "\n")
cat("Retained after QC:", format(autochamber_retained, big.mark = ","), "\n")
cat("Collection rate:", round(autochamber_collection_rate, 1), "%\n")
cat("QC retention rate:", round(autochamber_retention_rate, 1), "%\n")
cat("Overall success rate:", round(autochamber_overall_success, 1), "%\n")

cat("\n=============================================================================\n")
cat("COMPARATIVE SUMMARY (October 2-31, 2023)\n")
cat("=============================================================================\n")
cat("System | Units | Days | Potential | Collected | Retained | Collection% | Retention% | Overall%\n")
cat("-------|-------|------|-----------|-----------|----------|-------------|------------|----------\n")
cat(sprintf("Fluxbot| %-5d | %-4d | %-9s | %-9s | %-8s | %-11.1f | %-10.1f | %.1f\n",
            fluxbot_units, fluxbot_date_range$duration_days, 
            format(fluxbot_potential_measurements, big.mark = ","),
            format(fluxbot_collected, big.mark = ","),
            format(fluxbot_retained, big.mark = ","),
            fluxbot_collection_rate, fluxbot_retention_rate, fluxbot_overall_success))
cat(sprintf("AutoCh | %-5d | %-4d | %-9s | %-9s | %-8s | %-11.1f | %-10.1f | %.1f\n",
            autochamber_units, autochamber_date_range$duration_days,
            format(autochamber_potential_measurements, big.mark = ","),
            format(autochamber_collected, big.mark = ","),
            format(autochamber_retained, big.mark = ","),
            autochamber_collection_rate, autochamber_retention_rate, autochamber_overall_success))

# =============================================================================
# RAW NUMBERS FOR PARAGRAPH WRITING
# =============================================================================

cat("\n=============================================================================\n")
cat("RAW NUMBERS FOR PARAGRAPH WRITING (October 2-31, 2023)\n")
cat("=============================================================================\n")

cat("\nFLUXBOT NUMBERS:\n")
cat("Units:", fluxbot_units, "\n")
cat("Days:", fluxbot_date_range$duration_days, "\n")
cat("Potential:", fluxbot_potential_measurements, "\n")
cat("Collected:", fluxbot_collected, "\n")
cat("Retained:", fluxbot_retained, "\n")
cat("Collection rate:", round(fluxbot_collection_rate, 1), "\n")
cat("Retention rate:", round(fluxbot_retention_rate, 1), "\n")
cat("Overall success:", round(fluxbot_overall_success, 1), "\n")

cat("\nAUTOCHAMBER NUMBERS:\n")
cat("Units:", autochamber_units, "\n")
cat("Days:", autochamber_date_range$duration_days, "\n")
cat("Potential:", autochamber_potential_measurements, "\n")
cat("Collected:", autochamber_collected, "\n")
cat("Retained:", autochamber_retained, "\n")
cat("Collection rate:", round(autochamber_collection_rate, 1), "\n")
cat("Retention rate:", round(autochamber_retention_rate, 1), "\n")
cat("Overall success:", round(autochamber_overall_success, 1), "\n")
























# Calculate percentage of time with at least 3 units running per plot
# Based on TOTAL potential deployment time (including downtime periods)
# Add this code at the end of your script

# =============================================================================
# SETUP: CREATE COMPLETE TIME SERIES FOR OCTOBER 2-31, 2023
# =============================================================================

# Create complete hourly time series for October 2-31
start_date <- as.POSIXct("2023-10-02 00:00:00", tz = "America/New_York")
end_date <- as.POSIXct("2023-10-31 23:00:00", tz = "America/New_York")
all_hours <- seq(from = start_date, to = end_date, by = "hour")

# Create complete time grid for both stands
complete_time_grid <- expand.grid(
  hour_of_obs = all_hours,
  stand = c("healthy", "unhealthy")
) %>%
  as.data.frame()

# =============================================================================
# FLUXBOT COVERAGE ANALYSIS
# =============================================================================

# Filter fluxbot data to October 2-31
fluxbot_oct <- HF_fluxestimates %>%
  mutate(
    date = as.Date(as.POSIXct(hour_of_obs)),
    hour_of_obs_rounded = as.POSIXct(round(as.numeric(as.POSIXct(hour_of_obs))/3600)*3600, 
                                     origin = "1970-01-01", tz = "America/New_York")
  ) %>%
  filter(date >= as.Date("2023-10-02") & date <= as.Date("2023-10-31"))

# Count active fluxbots per hour per stand
fluxbot_active_by_hour <- fluxbot_oct %>%
  group_by(hour_of_obs_rounded, stand) %>%
  summarise(n_active_units = n_distinct(id), .groups = 'drop') %>%
  rename(hour_of_obs = hour_of_obs_rounded)

# Merge with complete time grid to include hours with no data
fluxbot_complete_coverage <- complete_time_grid %>%
  left_join(fluxbot_active_by_hour, by = c("hour_of_obs", "stand")) %>%
  mutate(
    n_active_units = ifelse(is.na(n_active_units), 0, n_active_units),
    has_min_3_units = n_active_units >= 3
  )

# Calculate coverage statistics by stand
fluxbot_coverage_stats <- fluxbot_complete_coverage %>%
  group_by(stand) %>%
  summarise(
    total_possible_hours = n(),
    hours_with_data = sum(n_active_units > 0),
    hours_with_min_3 = sum(has_min_3_units),
    percent_coverage_min3 = round((hours_with_min_3 / total_possible_hours) * 100, 1),
    percent_uptime = round((hours_with_data / total_possible_hours) * 100, 1),
    mean_units_when_active = round(mean(n_active_units[n_active_units > 0], na.rm = TRUE), 1),
    max_units = max(n_active_units),
    .groups = 'drop'
  )

# =============================================================================
# AUTOCHAMBER COVERAGE ANALYSIS
# =============================================================================

# Filter autochamber data to October 2-31
autochamber_oct <- HFauto_fluxestimates %>%
  mutate(
    date = as.Date(as.POSIXct(hour_of_obs)),
    hour_of_obs_rounded = as.POSIXct(round(as.numeric(as.POSIXct(hour_of_obs))/3600)*3600, 
                                     origin = "1970-01-01", tz = "America/New_York")
  ) %>%
  filter(date >= as.Date("2023-10-02") & date <= as.Date("2023-10-31"))

# Count active autochambers per hour per stand
autochamber_active_by_hour <- autochamber_oct %>%
  group_by(hour_of_obs_rounded, stand) %>%
  summarise(n_active_units = n_distinct(id), .groups = 'drop') %>%
  rename(hour_of_obs = hour_of_obs_rounded)

# Merge with complete time grid to include hours with no data
autochamber_complete_coverage <- complete_time_grid %>%
  left_join(autochamber_active_by_hour, by = c("hour_of_obs", "stand")) %>%
  mutate(
    n_active_units = ifelse(is.na(n_active_units), 0, n_active_units),
    has_min_3_units = n_active_units >= 3
  )

# Calculate coverage statistics by stand
autochamber_coverage_stats <- autochamber_complete_coverage %>%
  group_by(stand) %>%
  summarise(
    total_possible_hours = n(),
    hours_with_data = sum(n_active_units > 0),
    hours_with_min_3 = sum(has_min_3_units),
    percent_coverage_min3 = round((hours_with_min_3 / total_possible_hours) * 100, 1),
    percent_uptime = round((hours_with_data / total_possible_hours) * 100, 1),
    mean_units_when_active = round(mean(n_active_units[n_active_units > 0], na.rm = TRUE), 1),
    max_units = max(n_active_units),
    .groups = 'drop'
  )

# =============================================================================
# PRINT COVERAGE ANALYSIS RESULTS
# =============================================================================

cat("=============================================================================\n")
cat("PLOT COVERAGE ANALYSIS: INCLUDING DOWNTIME (October 2-31, 2023)\n")
cat("=============================================================================\n")
cat("Total possible hours per stand:", length(all_hours), "\n\n")

cat("FLUXBOT COVERAGE BY STAND:\n")
cat("----------------------------\n")
for(i in 1:nrow(fluxbot_coverage_stats)) {
  stand_name <- fluxbot_coverage_stats$stand[i]
  stand_data <- fluxbot_coverage_stats[i, ]
  
  cat(sprintf("Stand: %s\n", stand_name))
  cat(sprintf("  Total possible hours: %d\n", stand_data$total_possible_hours))
  cat(sprintf("  Hours with any data: %d (%s%% uptime)\n", 
              stand_data$hours_with_data, stand_data$percent_uptime))
  cat(sprintf("  Hours with ≥3 units: %d (%s%% of total time)\n", 
              stand_data$hours_with_min_3, stand_data$percent_coverage_min3))
  cat(sprintf("  Mean units when active: %s\n", stand_data$mean_units_when_active))
  cat(sprintf("  Max units observed: %d\n\n", stand_data$max_units))
}

cat("AUTOCHAMBER COVERAGE BY STAND:\n")
cat("--------------------------------\n")
for(i in 1:nrow(autochamber_coverage_stats)) {
  stand_name <- autochamber_coverage_stats$stand[i]
  stand_data <- autochamber_coverage_stats[i, ]
  
  cat(sprintf("Stand: %s\n", stand_name))
  cat(sprintf("  Total possible hours: %d\n", stand_data$total_possible_hours))
  cat(sprintf("  Hours with any data: %d (%s%% uptime)\n", 
              stand_data$hours_with_data, stand_data$percent_uptime))
  cat(sprintf("  Hours with ≥3 units: %d (%s%% of total time)\n", 
              stand_data$hours_with_min_3, stand_data$percent_coverage_min3))
  cat(sprintf("  Mean units when active: %s\n", stand_data$mean_units_when_active))
  cat(sprintf("  Max units observed: %d\n\n", stand_data$max_units))
}

# =============================================================================
# COMPARATIVE SUMMARY TABLE
# =============================================================================

cat("=============================================================================\n")
cat("COMPARATIVE COVERAGE SUMMARY\n")
cat("=============================================================================\n")
cat("System      | Stand     | Uptime% | ≥3Units% | Mean Active | Max Units\n")
cat("------------|-----------|---------|----------|-------------|----------\n")

# Fluxbot results
for(i in 1:nrow(fluxbot_coverage_stats)) {
  stand_data <- fluxbot_coverage_stats[i, ]
  cat(sprintf("Fluxbot     | %-9s | %-7s | %-8s | %-11s | %d\n",
              stand_data$stand, 
              paste0(stand_data$percent_uptime, "%"),
              paste0(stand_data$percent_coverage_min3, "%"),
              stand_data$mean_units_when_active, 
              stand_data$max_units))
}

# Autochamber results
for(i in 1:nrow(autochamber_coverage_stats)) {
  stand_data <- autochamber_coverage_stats[i, ]
  cat(sprintf("Autochamber | %-9s | %-7s | %-8s | %-11s | %d\n",
              stand_data$stand, 
              paste0(stand_data$percent_uptime, "%"),
              paste0(stand_data$percent_coverage_min3, "%"),
              stand_data$mean_units_when_active, 
              stand_data$max_units))
}

# =============================================================================
# OVERALL SYSTEM PERFORMANCE
# =============================================================================

fluxbot_overall <- fluxbot_coverage_stats %>%
  summarise(
    overall_uptime = round(mean(percent_uptime), 1),
    overall_min3_coverage = round(mean(percent_coverage_min3), 1)
  )

autochamber_overall <- autochamber_coverage_stats %>%
  summarise(
    overall_uptime = round(mean(percent_uptime), 1),
    overall_min3_coverage = round(mean(percent_coverage_min3), 1)
  )

cat("\n=============================================================================\n")
cat("OVERALL SYSTEM PERFORMANCE\n")
cat("=============================================================================\n")
cat("Fluxbot System:\n")
cat("  Average uptime across plots:", fluxbot_overall$overall_uptime, "%\n")
cat("  Average ≥3 units coverage:", fluxbot_overall$overall_min3_coverage, "%\n")
cat("\nAutochamber System:\n")
cat("  Average uptime across plots:", autochamber_overall$overall_uptime, "%\n")
cat("  Average ≥3 units coverage:", autochamber_overall$overall_min3_coverage, "%\n")

# =============================================================================
# RAW NUMBERS FOR REPORTING
# =============================================================================

cat("\n=============================================================================\n")
cat("RAW NUMBERS FOR REPORTING\n")
cat("=============================================================================\n")

cat("\nFLUXBOT DETAILED STATS:\n")
print(fluxbot_coverage_stats)

cat("\nAUTOCHAMBER DETAILED STATS:\n")
print(autochamber_coverage_stats)