# ==============================================================================
# QUANTITATIVE BIAS ASSESSMENT FOR SUPPLEMENTARY INFORMATION
# ==============================================================================

library(dplyr)
library(ggplot2)

# Calculate comprehensive bias statistics
bias_assessment <- list()

# First, ensure concentration categories and bias metrics are calculated
all_flux_comparison <- all_flux_comparison %>%
  mutate(
    conc_category = case_when(
      is.na(starting_conc) ~ "No data",
      starting_conc < 450 ~ "Low (< 450 ppm)",
      starting_conc >= 450 & starting_conc < 600 ~ "Normal (450-600 ppm)", 
      starting_conc >= 600 & starting_conc < 850 ~ "Elevated (600-850 ppm)",
      starting_conc >= 850 ~ "High (≥ 850 ppm)"
    ),
    conc_category = factor(conc_category, 
                           levels = c("Low (< 450 ppm)", "Normal (450-600 ppm)", 
                                      "Elevated (600-850 ppm)", "High (≥ 850 ppm)", "No data")),
    
    # Calculate bias metrics if not already present
    flux_difference = fluxbot_flux - autochamber_flux,
    flux_ratio = fluxbot_flux / autochamber_flux,
    percent_error = ((fluxbot_flux - autochamber_flux) / autochamber_flux) * 100
  )

# ==============================================================================
# 1. OVERALL DATASET CHARACTERISTICS
# ==============================================================================

bias_assessment$dataset_summary <- data.frame(
  metric = c("Total observations", "Observations with concentration data", 
             "Percentage with concentration data", "Date range", "Measurement period"),
  value = c(
    nrow(all_flux_comparison),
    sum(!is.na(all_flux_comparison$starting_conc)),
    round(sum(!is.na(all_flux_comparison$starting_conc)) / nrow(all_flux_comparison) * 100, 1),
    paste(range(as.Date(all_flux_comparison$hour_of_obs), na.rm = TRUE), collapse = " to "),
    paste(round(as.numeric(diff(range(as.Date(all_flux_comparison$hour_of_obs), na.rm = TRUE)))), "days")
  )
)

print("=== DATASET SUMMARY ===")
print(bias_assessment$dataset_summary)

# ==============================================================================
# 2. CONCENTRATION DISTRIBUTION
# ==============================================================================

bias_assessment$concentration_stats <- all_flux_comparison %>%
  filter(!is.na(starting_conc)) %>%
  summarise(
    min_conc = min(starting_conc),
    q25_conc = quantile(starting_conc, 0.25),
    median_conc = median(starting_conc),
    q75_conc = quantile(starting_conc, 0.75),
    max_conc = max(starting_conc),
    mean_conc = mean(starting_conc),
    sd_conc = sd(starting_conc),
    .groups = 'drop'
  )

print("=== CONCENTRATION DISTRIBUTION ===")
print(bias_assessment$concentration_stats)

# Data distribution by concentration categories
bias_assessment$conc_category_distribution <- all_flux_comparison %>%
  filter(!is.na(starting_conc)) %>%
  group_by(conc_category) %>%
  summarise(
    n_obs = n(),
    percentage = round(n() / nrow(filter(all_flux_comparison, !is.na(starting_conc))) * 100, 1),
    .groups = 'drop'
  ) %>%
  arrange(conc_category)

print("=== CONCENTRATION CATEGORY DISTRIBUTION ===")
print(bias_assessment$conc_category_distribution)

# ==============================================================================
# 3. FLUX MEASUREMENT RANGES
# ==============================================================================

bias_assessment$flux_ranges <- all_flux_comparison %>%
  summarise(
    fluxbot_min = min(fluxbot_flux, na.rm = TRUE),
    fluxbot_max = max(fluxbot_flux, na.rm = TRUE),
    fluxbot_mean = mean(fluxbot_flux, na.rm = TRUE),
    fluxbot_sd = sd(fluxbot_flux, na.rm = TRUE),
    autochamber_min = min(autochamber_flux, na.rm = TRUE),
    autochamber_max = max(autochamber_flux, na.rm = TRUE),
    autochamber_mean = mean(autochamber_flux, na.rm = TRUE),
    autochamber_sd = sd(autochamber_flux, na.rm = TRUE),
    .groups = 'drop'
  )

print("=== FLUX MEASUREMENT RANGES ===")
print(bias_assessment$flux_ranges)

# ==============================================================================
# 4. BIAS STATISTICS BY CONCENTRATION CATEGORY
# ==============================================================================

bias_assessment$bias_by_category <- all_flux_comparison %>%
  filter(!is.na(starting_conc)) %>%
  group_by(conc_category) %>%
  summarise(
    n = n(),
    mean_bias = mean(flux_difference, na.rm = TRUE),
    median_bias = median(flux_difference, na.rm = TRUE),
    sd_bias = sd(flux_difference, na.rm = TRUE),
    q25_bias = quantile(flux_difference, 0.25, na.rm = TRUE),
    q75_bias = quantile(flux_difference, 0.75, na.rm = TRUE),
    rmse = sqrt(mean(flux_difference^2, na.rm = TRUE)),
    mean_abs_error = mean(abs(flux_difference), na.rm = TRUE),
    correlation = cor(fluxbot_flux, autochamber_flux, use = "complete.obs"),
    # Calculate bias as percentage of typical flux values
    mean_autochamber_flux = mean(autochamber_flux, na.rm = TRUE),
    bias_percentage = round((mean_bias / mean_autochamber_flux) * 100, 1),
    .groups = 'drop'
  )

print("=== BIAS STATISTICS BY CONCENTRATION CATEGORY ===")
print(bias_assessment$bias_by_category)

# ==============================================================================
# 5. OVERALL CORRELATION AND REGRESSION STATISTICS
# ==============================================================================

# Overall correlation
bias_assessment$overall_correlation <- cor.test(all_flux_comparison$starting_conc, 
                                                all_flux_comparison$flux_difference, 
                                                method = "spearman", use = "complete.obs")

# Linear regression
bias_assessment$regression_model <- lm(flux_difference ~ starting_conc, data = all_flux_comparison)
bias_assessment$regression_summary <- summary(bias_assessment$regression_model)

# Extract key regression statistics
bias_assessment$regression_stats <- data.frame(
  metric = c("Spearman correlation (rho)", "Spearman p-value", "Linear slope (bias per ppm)", 
             "Linear intercept", "R-squared", "Regression p-value", "Residual standard error"),
  value = c(
    round(bias_assessment$overall_correlation$estimate, 4),
    format(bias_assessment$overall_correlation$p.value, scientific = TRUE, digits = 3),
    format(coef(bias_assessment$regression_model)[2], scientific = TRUE, digits = 4),
    round(coef(bias_assessment$regression_model)[1], 4),
    round(bias_assessment$regression_summary$r.squared, 4),
    format(bias_assessment$regression_summary$coefficients[2,4], scientific = TRUE, digits = 3),
    round(bias_assessment$regression_summary$sigma, 4)
  )
)

print("=== OVERALL CORRELATION AND REGRESSION ===")
print(bias_assessment$regression_stats)

# ==============================================================================
# 6. PRACTICAL SIGNIFICANCE ASSESSMENT
# ==============================================================================

# Calculate practical significance metrics
typical_flux_range <- quantile(all_flux_comparison$autochamber_flux, c(0.25, 0.75), na.rm = TRUE)
typical_flux_iqr <- diff(typical_flux_range)

bias_assessment$practical_significance <- data.frame(
  metric = c("Overall mean bias", "Overall median bias", "Overall RMSE", 
             "Autochamber flux IQR", "Bias as % of flux IQR", 
             "Max bias (high conc)", "Max bias as % of typical flux",
             "Proportion of data with small bias", "Proportion with large bias"),
  value = c(
    round(mean(all_flux_comparison$flux_difference, na.rm = TRUE), 4),
    round(median(all_flux_comparison$flux_difference, na.rm = TRUE), 4),
    round(sqrt(mean(all_flux_comparison$flux_difference^2, na.rm = TRUE)), 4),
    round(typical_flux_iqr, 2),
    round((abs(mean(all_flux_comparison$flux_difference, na.rm = TRUE)) / typical_flux_iqr) * 100, 1),
    round(min(bias_assessment$bias_by_category$mean_bias), 2),
    round((abs(min(bias_assessment$bias_by_category$mean_bias)) / mean(typical_flux_range)) * 100, 1),
    round(sum(abs(all_flux_comparison$flux_difference) < 0.5, na.rm = TRUE) / 
            sum(!is.na(all_flux_comparison$flux_difference)) * 100, 1),
    round(sum(abs(all_flux_comparison$flux_difference) > 1.0, na.rm = TRUE) / 
            sum(!is.na(all_flux_comparison$flux_difference)) * 100, 1)
  )
)

print("=== PRACTICAL SIGNIFICANCE ASSESSMENT ===")
print(bias_assessment$practical_significance)

# ==============================================================================
# 7. HIGH CONCENTRATION BIAS ASSESSMENT
# ==============================================================================

# Focus on high concentration data (≥850 ppm)
high_conc_data <- filter(all_flux_comparison, starting_conc >= 850 & !is.na(starting_conc))

bias_assessment$high_concentration_impact <- data.frame(
  metric = c("High concentration observations", "Percentage of total data", 
             "Mean bias in high conc", "SD bias in high conc", "RMSE in high conc",
             "Correlation in high conc", "Mean autochamber flux (high conc)",
             "Bias as % of flux (high conc)"),
  value = c(
    nrow(high_conc_data),
    round(nrow(high_conc_data) / nrow(filter(all_flux_comparison, !is.na(starting_conc))) * 100, 1),
    round(mean(high_conc_data$flux_difference, na.rm = TRUE), 3),
    round(sd(high_conc_data$flux_difference, na.rm = TRUE), 3),
    round(sqrt(mean(high_conc_data$flux_difference^2, na.rm = TRUE)), 3),
    round(cor(high_conc_data$fluxbot_flux, high_conc_data$autochamber_flux, use = "complete.obs"), 3),
    round(mean(high_conc_data$autochamber_flux, na.rm = TRUE), 3),
    round((abs(mean(high_conc_data$flux_difference, na.rm = TRUE)) / 
             mean(high_conc_data$autochamber_flux, na.rm = TRUE)) * 100, 1)
  )
)

print("=== HIGH CONCENTRATION BIAS ASSESSMENT ===")
print(bias_assessment$high_concentration_impact)

# ==============================================================================
# 8. SUMMARY STATISTICS FOR MANUSCRIPT
# ==============================================================================

# Create a summary table for the manuscript
bias_assessment$manuscript_summary <- data.frame(
  Concentration_Range = c("Low (< 450 ppm)", "Normal (450-600 ppm)", 
                          "Elevated (600-850 ppm)", "High (≥ 850 ppm)", "Overall"),
  N_Observations = c(
    bias_assessment$bias_by_category$n,
    nrow(filter(all_flux_comparison, !is.na(starting_conc)))
  ),
  Percent_of_Data = c(
    bias_assessment$bias_by_category$n / nrow(filter(all_flux_comparison, !is.na(starting_conc))) * 100,
    100
  ),
  Mean_Bias = c(
    bias_assessment$bias_by_category$mean_bias,
    mean(all_flux_comparison$flux_difference, na.rm = TRUE)
  ),
  RMSE = c(
    bias_assessment$bias_by_category$rmse,
    sqrt(mean(all_flux_comparison$flux_difference^2, na.rm = TRUE))
  ),
  Correlation_r = c(
    bias_assessment$bias_by_category$correlation,
    cor(all_flux_comparison$fluxbot_flux, all_flux_comparison$autochamber_flux, use = "complete.obs")
  ),
  Bias_Percent_of_Flux = c(
    bias_assessment$bias_by_category$bias_percentage,
    round((abs(mean(all_flux_comparison$flux_difference, na.rm = TRUE)) / 
             mean(all_flux_comparison$autochamber_flux, na.rm = TRUE)) * 100, 1)
  )
)

# Round for presentation
bias_assessment$manuscript_summary <- bias_assessment$manuscript_summary %>%
  mutate(
    Percent_of_Data = round(Percent_of_Data, 1),
    Mean_Bias = round(Mean_Bias, 3),
    RMSE = round(RMSE, 3),
    Correlation_r = round(Correlation_r, 3)
  )

print("=== MANUSCRIPT SUMMARY TABLE ===")
print(bias_assessment$manuscript_summary)

# ==============================================================================
# 9. SAVE RESULTS
# ==============================================================================

# Save all results for manuscript use
save(bias_assessment, file = "fluxbot_bias_assessment_results.RData")

# Export summary table as CSV
write.csv(bias_assessment$manuscript_summary, "fluxbot_bias_summary_table.csv", row.names = FALSE)

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("All results saved to 'fluxbot_bias_assessment_results.RData'\n")
cat("Summary table exported to 'fluxbot_bias_summary_table.csv'\n")
cat("\nKey findings for SI section:\n")
cat("- Total observations:", nrow(all_flux_comparison), "\n")
cat("- Statistical significance: p =", format(bias_assessment$overall_correlation$p.value, scientific = TRUE), "\n")
cat("- Effect size (R²):", round(bias_assessment$regression_summary$r.squared, 4), "\n")
cat("- Practical significance: bias represents", 
    round((abs(mean(all_flux_comparison$flux_difference, na.rm = TRUE)) / 
             mean(all_flux_comparison$autochamber_flux, na.rm = TRUE)) * 100, 1), 
    "% of mean flux\n")
cat("- Most data (", round(sum(bias_assessment$bias_by_category$n[1:2]) / 
                             sum(bias_assessment$bias_by_category$n) * 100, 1), 
    "%) in low-normal range with minimal bias\n")