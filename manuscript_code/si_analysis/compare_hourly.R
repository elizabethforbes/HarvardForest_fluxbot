# Load required libraries
library(ggplot2)
library(dplyr)
library(viridis)
library(broom)
library(corrplot)

# ==============================================================================
# APPROACH 1: Compare fluxbot accuracy vs autochamber across concentration ranges
# ==============================================================================

# Prepare the data with concentration ranges
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
    
    # Calculate bias metrics
    flux_difference = fluxbot_flux - autochamber_flux,  # fluxbot minus autochamber
    flux_ratio = fluxbot_flux / autochamber_flux,       # fluxbot relative to autochamber
    percent_error = ((fluxbot_flux - autochamber_flux) / autochamber_flux) * 100
  )

# Plot 1: Bias (difference) vs starting concentration
p1 <- ggplot(all_flux_comparison, aes(x = starting_conc, y = flux_difference)) +
  geom_point(alpha = 0.6, color = "darkblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(
    x = "Starting CO₂ Concentration (ppm)",
    y = "Flux Bias (Fluxbot - Autochamber)",
    title = "Fluxbot Bias vs Starting CO₂ Concentration",
    subtitle = "Positive values = fluxbot overestimates, Negative = underestimates"
  ) +
  theme_classic()

print(p1)

# Plot 2: Box plots of bias by concentration category
p2 <- ggplot(filter(all_flux_comparison, !is.na(starting_conc)), 
             aes(x = conc_category, y = flux_difference, fill = conc_category)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(alpha=0.1)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_viridis_d(option = "plasma") +
  labs(
    x = "Starting CO₂ Concentration Category",
    y = "Flux Bias (Fluxbot - Autochamber)",
    title = "Distribution of Fluxbot Bias by Concentration Range"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

print(p2)

# ==============================================================================
# APPROACH 2: Statistical tests for concentration-dependent bias
# ==============================================================================

# Test 1: Correlation between starting concentration and bias
bias_correlation <- cor.test(all_flux_comparison$starting_conc, 
                             all_flux_comparison$flux_difference, 
                             method = "spearman", use = "complete.obs")

cat("=== CORRELATION ANALYSIS ===\n")
cat("Spearman correlation between starting CO₂ and flux bias:\n")
cat("rho =", round(bias_correlation$estimate, 4), "\n")
cat("p-value =", format(bias_correlation$p.value, scientific = TRUE), "\n")
if(bias_correlation$p.value < 0.05) {
  cat("*** SIGNIFICANT correlation detected! ***\n\n")
} else {
  cat("No significant correlation.\n\n")
}

# Test 2: Linear regression of bias vs concentration
bias_model <- lm(flux_difference ~ starting_conc, data = all_flux_comparison)
bias_summary <- summary(bias_model)

cat("=== REGRESSION ANALYSIS ===\n")
cat("Linear model: Flux_Bias ~ Starting_CO2\n")
cat("Slope:", round(coef(bias_model)[2], 6), "bias units per ppm CO₂\n")
cat("R²:", round(bias_summary$r.squared, 4), "\n")
cat("p-value for slope:", format(bias_summary$coefficients[2,4], scientific = TRUE), "\n")
if(bias_summary$coefficients[2,4] < 0.05) {
  cat("*** SIGNIFICANT relationship detected! ***\n\n")
} else {
  cat("No significant relationship.\n\n")
}

# ==============================================================================
# APPROACH 3: Compare measurement precision across concentration ranges  
# ==============================================================================

precision_analysis <- all_flux_comparison %>%
  filter(!is.na(starting_conc)) %>%
  group_by(conc_category) %>%
  summarise(
    n = n(),
    mean_bias = mean(flux_difference, na.rm = TRUE),
    sd_bias = sd(flux_difference, na.rm = TRUE),
    rmse = sqrt(mean(flux_difference^2, na.rm = TRUE)),
    mean_abs_error = mean(abs(flux_difference), na.rm = TRUE),
    correlation = cor(fluxbot_flux, autochamber_flux, use = "complete.obs"),
    .groups = 'drop'
  )

cat("=== PRECISION BY CONCENTRATION RANGE ===\n")
print(precision_analysis)
cat("\n")

# Plot 3: RMSE and correlation by concentration category
p3 <- precision_analysis %>%
  ggplot(aes(x = conc_category)) +
  geom_col(aes(y = rmse), fill = "coral", alpha = 0.7) +
  geom_text(aes(y = rmse + 0.1, label = paste0("r = ", round(correlation, 2)))) +
  labs(
    x = "Starting CO₂ Concentration Category",
    y = "Root Mean Square Error",
    title = "Measurement Precision vs Starting CO₂ Concentration",
    subtitle = "Text shows correlation coefficient (r) between fluxbot and autochamber"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p3)

# ==============================================================================
# APPROACH 4: Test for non-linear sensor saturation effects
# ==============================================================================

# Plot 4: Fluxbot vs Autochamber colored by concentration with saturation curve
p4 <- all_flux_comparison %>%
  filter(!is.na(starting_conc)) %>%
  ggplot(aes(x = autochamber_flux, y = fluxbot_flux, color = starting_conc)) +
  geom_point(alpha = 0.7, size = 2) +
  scale_color_viridis_c(name = "Starting CO₂\n(ppm)", option = "plasma") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  geom_smooth(method = "lm", se = TRUE, color = "blue", alpha = 0.3) +
  geom_smooth(method = "loess", se = FALSE, color = "red", linetype = "dotted", alpha = 0.8) +
  labs(
    x = "Autochamber Flux (μmol m⁻² s⁻¹)",
    y = "Fluxbot Flux (μmol m⁻² s⁻¹)", 
    title = "Sensor Response: Linear vs Non-linear Trends",
    subtitle = "Dashed = 1:1 line, Blue = linear fit, Red dotted = LOESS (non-linear)"
  ) +
  theme_classic() +
  coord_fixed()

print(p4)

# ==============================================================================
# APPROACH 5: Threshold analysis - where does bias start?
# ==============================================================================

# Calculate moving average bias across concentration gradient
conc_range <- seq(400, 1200, by = 25)
threshold_analysis <- data.frame()

for(conc_threshold in conc_range) {
  high_conc <- filter(all_flux_comparison, starting_conc >= conc_threshold & !is.na(starting_conc))
  low_conc <- filter(all_flux_comparison, starting_conc < conc_threshold & !is.na(starting_conc))
  
  if(nrow(high_conc) > 10 & nrow(low_conc) > 10) {
    t_test <- t.test(high_conc$flux_difference, low_conc$flux_difference)
    
    threshold_analysis <- rbind(threshold_analysis, data.frame(
      threshold = conc_threshold,
      high_bias = mean(high_conc$flux_difference),
      low_bias = mean(low_conc$flux_difference),
      p_value = t_test$p.value,
      n_high = nrow(high_conc),
      n_low = nrow(low_conc)
    ))
  }
}

# Plot 5: Bias difference across concentration thresholds
p5 <- threshold_analysis %>%
  mutate(significant = p_value < 0.05) %>%
  ggplot(aes(x = threshold, y = high_bias - low_bias, color = significant)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("FALSE" = "gray", "TRUE" = "red"),
                     name = "Significant\n(p < 0.05)") +
  labs(
    x = "CO₂ Concentration Threshold (ppm)",
    y = "Bias Difference (High - Low concentration groups)",
    title = "Threshold Analysis: Where Does Sensor Interference Begin?",
    subtitle = "Red points indicate significant bias differences"
  ) +
  theme_classic()

print(p5)

# Summary of key findings
cat("=== SUMMARY ===\n")
cat("Key metrics to interpret:\n")
cat("1. Correlation test: Does bias increase with CO₂ concentration?\n")
cat("2. Regression: How much bias per ppm increase?\n") 
cat("3. RMSE by category: Does precision degrade at high concentrations?\n")
cat("4. Threshold analysis: At what concentration does bias become significant?\n\n")

cat("If sensor interference exists, you should see:\n")
cat("- Positive correlation between starting CO₂ and bias\n")
cat("- Higher RMSE at high concentrations\n") 
cat("- Significant bias differences above certain thresholds\n")
cat("- Non-linear patterns in the fluxbot vs autochamber relationship\n")
