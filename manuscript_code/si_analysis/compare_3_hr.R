# Load required libraries
library(ggplot2)
library(dplyr)
library(viridis)
library(broom)
library(zoo)  # For rolling mean calculations

# ==============================================================================
# STEP 1: Calculate 3-hour rolling means
# ==============================================================================

# First, ensure data is sorted by time for proper rolling calculations
all_flux_comparison_sorted <- all_flux_comparison %>%
  arrange(hour_of_obs)

# Calculate 3-hour rolling means by chamber/stand
rolling_flux_data <- all_flux_comparison_sorted %>%
  group_by(stand) %>%
  arrange(hour_of_obs) %>%
  mutate(
    # 3-hour rolling means (center-aligned, so uses 1.5 hours before and after each point)
    fluxbot_flux_3h = rollapply(fluxbot_flux, width = 3, FUN = mean, fill = NA, align = "center"),
    autochamber_flux_3h = rollapply(autochamber_flux, width = 3, FUN = mean, fill = NA, align = "center"),
    starting_conc_3h = rollapply(starting_conc, width = 3, FUN = mean, fill = NA, align = "center", na.rm = TRUE),
    ending_conc_3h = rollapply(ending_conc, width = 3, FUN = mean, fill = NA, align = "center", na.rm = TRUE)
  ) %>%
  ungroup() %>%
  # Remove rows where rolling means couldn't be calculated
  filter(!is.na(fluxbot_flux_3h) & !is.na(autochamber_flux_3h) & !is.na(starting_conc_3h))

# Create concentration categories and bias metrics for rolling means
rolling_analysis_data <- rolling_flux_data %>%
  mutate(
    conc_category_3h = case_when(
      is.na(starting_conc_3h) ~ "No data",
      starting_conc_3h < 450 ~ "Low (< 450 ppm)",
      starting_conc_3h >= 450 & starting_conc_3h < 600 ~ "Normal (450-600 ppm)", 
      starting_conc_3h >= 600 & starting_conc_3h < 850 ~ "Elevated (600-850 ppm)",
      starting_conc_3h >= 850 ~ "High (≥ 850 ppm)"
    ),
    conc_category_3h = factor(conc_category_3h, 
                              levels = c("Low (< 450 ppm)", "Normal (450-600 ppm)", 
                                         "Elevated (600-850 ppm)", "High (≥ 850 ppm)", "No data")),
    
    # Calculate bias metrics for rolling means
    flux_difference_3h = fluxbot_flux_3h - autochamber_flux_3h,
    flux_ratio_3h = fluxbot_flux_3h / autochamber_flux_3h,
    percent_error_3h = ((fluxbot_flux_3h - autochamber_flux_3h) / autochamber_flux_3h) * 100
  )

cat("=== 3-HOUR ROLLING MEAN ANALYSIS ===\n")
cat("Original data points:", nrow(all_flux_comparison), "\n")
cat("Rolling mean data points:", nrow(rolling_analysis_data), "\n")
cat("Starting conc range (3h means):", range(rolling_analysis_data$starting_conc_3h, na.rm = TRUE), "ppm\n\n")

# ==============================================================================
# VISUALIZATION: Rolling Mean Analysis
# ==============================================================================

# Plot 1: Bias vs starting concentration (3-hour rolling means)
p1_rolling <- ggplot(rolling_analysis_data, aes(x = starting_conc_3h, y = flux_difference_3h)) +
  geom_point(alpha = 0.6, color = "darkblue", size = 1.5) +
  geom_smooth(method = "lm", color = "red", se = TRUE, size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(
    x = "3-Hour Mean Starting CO₂ Concentration (ppm)",
    y = "3-Hour Mean Flux Bias (Fluxbot - Autochamber)",
    title = "Fluxbot Bias vs Starting CO₂ (3-Hour Rolling Means)",
    subtitle = "Smoothed data removes short-term noise to reveal systematic patterns"
  ) +
  theme_classic()

print(p1_rolling)

# Plot 2: Box plots by concentration category (3-hour rolling means)
p2_rolling <- ggplot(filter(rolling_analysis_data, !is.na(starting_conc_3h)), 
                     aes(x = conc_category_3h, y = flux_difference_3h, fill = conc_category_3h)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(alpha = 0.3, width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_viridis_d(option = "plasma") +
  labs(
    x = "3-Hour Mean Starting CO₂ Category",
    y = "3-Hour Mean Flux Bias",
    title = "Distribution of Smoothed Flux Bias by Concentration Range"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

print(p2_rolling)

# ==============================================================================
# STATISTICAL ANALYSIS: Rolling Means
# ==============================================================================

# Test 1: Correlation between starting concentration and bias (rolling means)
bias_correlation_3h <- cor.test(rolling_analysis_data$starting_conc_3h, 
                                rolling_analysis_data$flux_difference_3h, 
                                method = "spearman", use = "complete.obs")

cat("=== 3-HOUR ROLLING MEAN CORRELATION ANALYSIS ===\n")
cat("Spearman correlation between 3h-mean starting CO₂ and flux bias:\n")
cat("rho =", round(bias_correlation_3h$estimate, 4), "\n")
cat("p-value =", format(bias_correlation_3h$p.value, scientific = TRUE), "\n")
if(bias_correlation_3h$p.value < 0.05) {
  cat("*** SIGNIFICANT correlation detected! ***\n\n")
} else {
  cat("No significant correlation.\n\n")
}

# Test 2: Linear regression (rolling means)
bias_model_3h <- lm(flux_difference_3h ~ starting_conc_3h, data = rolling_analysis_data)
bias_summary_3h <- summary(bias_model_3h)

cat("=== 3-HOUR ROLLING MEAN REGRESSION ANALYSIS ===\n")
cat("Linear model: 3h_Flux_Bias ~ 3h_Starting_CO2\n")
cat("Slope:", round(coef(bias_model_3h)[2], 6), "bias units per ppm CO₂\n")
cat("R²:", round(bias_summary_3h$r.squared, 4), "\n")
cat("p-value for slope:", format(bias_summary_3h$coefficients[2,4], scientific = TRUE), "\n")
if(bias_summary_3h$coefficients[2,4] < 0.05) {
  cat("*** SIGNIFICANT relationship detected! ***\n\n")
} else {
  cat("No significant relationship.\n\n")
}

# ==============================================================================
# PRECISION ANALYSIS: Rolling Means
# ==============================================================================

precision_analysis_3h <- rolling_analysis_data %>%
  filter(!is.na(starting_conc_3h)) %>%
  group_by(conc_category_3h) %>%
  summarise(
    n = n(),
    mean_bias_3h = mean(flux_difference_3h, na.rm = TRUE),
    sd_bias_3h = sd(flux_difference_3h, na.rm = TRUE),
    rmse_3h = sqrt(mean(flux_difference_3h^2, na.rm = TRUE)),
    mean_abs_error_3h = mean(abs(flux_difference_3h), na.rm = TRUE),
    correlation_3h = cor(fluxbot_flux_3h, autochamber_flux_3h, use = "complete.obs"),
    .groups = 'drop'
  )

cat("=== 3-HOUR ROLLING MEAN PRECISION BY CONCENTRATION RANGE ===\n")
print(precision_analysis_3h)
cat("\n")

# Plot 3: RMSE and correlation by concentration category (rolling means)
p3_rolling <- precision_analysis_3h %>%
  ggplot(aes(x = conc_category_3h)) +
  geom_col(aes(y = rmse_3h), fill = "coral", alpha = 0.7) +
  geom_text(aes(y = rmse_3h + 0.05, label = paste0("r = ", round(correlation_3h, 2)))) +
  labs(
    x = "3-Hour Mean Starting CO₂ Category",
    y = "Root Mean Square Error (3-Hour Means)",
    title = "Smoothed Measurement Precision vs Starting CO₂",
    subtitle = "Based on 3-hour rolling means - reduces noise effects"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p3_rolling)

# ==============================================================================
# COMPARISON: Original vs Rolling Mean Results
# ==============================================================================

# Compare correlations
cat("=== COMPARISON: ORIGINAL vs 3-HOUR ROLLING MEANS ===\n")
cat("Original data correlation (rho):", round(bias_correlation$estimate, 4), "\n")
cat("3-hour rolling mean correlation (rho):", round(bias_correlation_3h$estimate, 4), "\n")
cat("Original data R²:", round(bias_summary$r.squared, 4), "\n") 
cat("3-hour rolling mean R²:", round(bias_summary_3h$r.squared, 4), "\n")

# Calculate improvement in signal detection
if(abs(bias_correlation_3h$estimate) > abs(bias_correlation$estimate)) {
  cat("✅ Rolling means STRENGTHEN the correlation signal\n")
} else {
  cat("❌ Rolling means WEAKEN the correlation signal\n")
}

if(bias_summary_3h$r.squared > bias_summary$r.squared) {
  cat("✅ Rolling means IMPROVE the explanatory power (R²)\n")
} else {
  cat("❌ Rolling means REDUCE the explanatory power (R²)\n")
}

# Plot 4: Direct comparison plot
comparison_plot_data <- data.frame(
  analysis_type = rep(c("Original Data", "3-Hour Rolling Means"), each = 4),
  concentration = rep(c("Low", "Normal", "Elevated", "High"), 2),
  rmse = c(precision_analysis$rmse, precision_analysis_3h$rmse_3h),
  correlation = c(precision_analysis$correlation, precision_analysis_3h$correlation_3h)
)

p4_comparison <- comparison_plot_data %>%
  ggplot(aes(x = concentration, y = rmse, fill = analysis_type)) +
  geom_col(position = "dodge", alpha = 0.7) +
  geom_text(aes(label = paste0("r=", round(correlation, 2))), 
            position = position_dodge(width = 0.9), 
            vjust = -0.2, size = 3) +
  scale_fill_manual(values = c("Original Data" = "lightblue", "3-Hour Rolling Means" = "coral")) +
  labs(
    x = "Starting CO₂ Concentration Category",
    y = "Root Mean Square Error",
    title = "Comparison: Original vs 3-Hour Rolling Mean Analysis",
    subtitle = "Lower RMSE and higher correlations indicate better measurement precision",
    fill = "Analysis Type"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p4_comparison)

cat("\n=== SUMMARY ===\n")
cat("3-hour rolling means help by:\n")
cat("1. Reducing measurement noise and random errors\n")
cat("2. Revealing systematic bias patterns more clearly\n")
cat("3. Providing more robust statistical relationships\n")
cat("4. Better isolating true sensor interference from environmental variability\n")