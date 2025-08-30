# ==============================================================================
# FLUXBOT BIAS ASSESSMENT WITH PROJECT FILTERING APPLIED
# Applies the same IQR filtering and negative flux removal as the main project
# ==============================================================================

library(dplyr)
library(ggplot2)

# Load the filter_iqr function
source("manuscript_code/filter_iqr.R")

# Check that required original datasets exist
required_datasets <- c("HF_fluxestimates", "HFauto_fluxestimates")
missing_datasets <- required_datasets[!sapply(required_datasets, exists)]

if(length(missing_datasets) > 0) {
  stop("Missing required datasets: ", paste(missing_datasets, collapse = ", "), 
       "\nPlease ensure HF_fluxestimates and HFauto_fluxestimates are loaded")
}

cat("=== APPLYING PROJECT FILTERING TO BIAS ASSESSMENT ===\n")
cat("Original fluxbot observations:", nrow(HF_fluxestimates), "\n")
cat("Original autochamber observations:", nrow(HFauto_fluxestimates), "\n")

# ==============================================================================
# APPLY SAME FILTERING AS PROJECT WORKFLOW
# ==============================================================================

#### FLUXBOTS: Apply negative flux removal and IQR filtering
HF_fluxestimates_subset <- HF_fluxestimates[HF_fluxestimates$fluxL_umolm2sec >= 0, ]
HF_fluxestimates_filtered_bias <- HF_fluxestimates_subset %>% 
  filter_iqr("fluxL_umolm2sec")

cat("Fluxbot observations after filtering:", nrow(HF_fluxestimates_filtered_bias), "\n")

#### AUTOCHAMBERS: Apply negative flux removal and IQR filtering  
HFauto_fluxestimates_subset <- HFauto_fluxestimates[HFauto_fluxestimates$fluxL_umolm2sec >= 0, ]
HFauto_fluxestimates_filtered_bias <- HFauto_fluxestimates_subset %>% 
  filter_iqr("fluxL_umolm2sec")

cat("Autochamber observations after filtering:", nrow(HFauto_fluxestimates_filtered_bias), "\n")

# Average autochamber data by hour and stand (matching project workflow)
HFauto_fluxestimates_filtered_avg <- HFauto_fluxestimates_filtered_bias %>%
  group_by(hour_of_obs, stand) %>%
  summarise(fluxL_umolm2sec = mean(fluxL_umolm2sec, na.rm = TRUE),
            .groups = 'drop') %>%
  mutate(method = "autochamber")

# Prepare fluxbot data
HF_fluxestimates_filtered_bias <- HF_fluxestimates_filtered_bias %>%
  mutate(method = "fluxbot")

# Combine datasets (matching project workflow)
merged_data_bias <- rbind(
  HF_fluxestimates_filtered_bias %>% 
    select(id, hour_of_obs, date, hour, fluxL_umolm2sec, stand, method, starting_concen, ending_concen),
  HFauto_fluxestimates_filtered_avg %>% 
    select(hour_of_obs, fluxL_umolm2sec, stand, method) %>%
    mutate(id = paste0("autochamber_avg"), date = as.Date(hour_of_obs), 
           hour = hour(hour_of_obs), starting_concen = NA, ending_concen = NA)
)

cat("Combined filtered dataset:", nrow(merged_data_bias), "observations\n\n")

# ==============================================================================
# CREATE FLUX COMPARISON DATASET
# ==============================================================================

# Extract fluxbot data with concentration info
fluxbot_data_filtered <- merged_data_bias %>%
  filter(method == "fluxbot") %>%
  select(hour_of_obs, stand, fluxL_umolm2sec, starting_concen, ending_concen) %>%
  rename(fluxbot_flux = fluxL_umolm2sec, starting_conc = starting_concen, ending_conc = ending_concen)

# Extract autochamber data (already averaged)
autochamber_data_filtered <- merged_data_bias %>%
  filter(method == "autochamber") %>%
  select(hour_of_obs, stand, fluxL_umolm2sec) %>%
  rename(autochamber_flux = fluxL_umolm2sec)

# Merge fluxbot and autochamber data
all_flux_comparison_filtered <- fluxbot_data_filtered %>%
  left_join(autochamber_data_filtered, by = c("hour_of_obs", "stand")) %>%
  filter(!is.na(fluxbot_flux) & !is.na(autochamber_flux))

# Calculate concentration categories and bias metrics
all_flux_comparison_filtered <- all_flux_comparison_filtered %>%
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
    flux_difference = fluxbot_flux - autochamber_flux,
    flux_ratio = fluxbot_flux / autochamber_flux,
    percent_error = ((fluxbot_flux - autochamber_flux) / autochamber_flux) * 100
  )

cat("Final comparison dataset:", nrow(all_flux_comparison_filtered), "observations\n")
cat("Observations with concentration data:", sum(!is.na(all_flux_comparison_filtered$starting_conc)), "\n\n")

# ==============================================================================
# QUANTITATIVE BIAS ASSESSMENT
# ==============================================================================

bias_assessment_filtered <- list()

# 1. Dataset characteristics
bias_assessment_filtered$dataset_summary <- data.frame(
  metric = c("Total observations (filtered)", "Observations with concentration data", 
             "Percentage with concentration data", "Date range", "Measurement period"),
  value = c(
    nrow(all_flux_comparison_filtered),
    sum(!is.na(all_flux_comparison_filtered$starting_conc)),
    round(sum(!is.na(all_flux_comparison_filtered$starting_conc)) / nrow(all_flux_comparison_filtered) * 100, 1),
    paste(range(as.Date(all_flux_comparison_filtered$hour_of_obs), na.rm = TRUE), collapse = " to "),
    paste(round(as.numeric(diff(range(as.Date(all_flux_comparison_filtered$hour_of_obs), na.rm = TRUE)))), "days")
  )
)

# 2. Concentration distribution
bias_assessment_filtered$concentration_stats <- all_flux_comparison_filtered %>%
  filter(!is.na(starting_conc)) %>%
  summarise(
    min_conc = min(starting_conc),
    q25_conc = quantile(starting_conc, 0.25),
    median_conc = median(starting_conc),
    q75_conc = quantile(starting_conc, 0.75),
    max_conc = max(starting_conc),
    mean_conc = mean(starting_conc),
    sd_conc = sd(starting_conc)
  )

bias_assessment_filtered$conc_category_distribution <- all_flux_comparison_filtered %>%
  filter(!is.na(starting_conc)) %>%
  group_by(conc_category) %>%
  summarise(
    n_obs = n(),
    percentage = round(n() / nrow(filter(all_flux_comparison_filtered, !is.na(starting_conc))) * 100, 1),
    .groups = 'drop'
  )

# 3. Flux ranges
bias_assessment_filtered$flux_ranges <- all_flux_comparison_filtered %>%
  summarise(
    fluxbot_min = min(fluxbot_flux, na.rm = TRUE),
    fluxbot_max = max(fluxbot_flux, na.rm = TRUE),
    fluxbot_mean = mean(fluxbot_flux, na.rm = TRUE),
    fluxbot_sd = sd(fluxbot_flux, na.rm = TRUE),
    autochamber_min = min(autochamber_flux, na.rm = TRUE),
    autochamber_max = max(autochamber_flux, na.rm = TRUE),
    autochamber_mean = mean(autochamber_flux, na.rm = TRUE),
    autochamber_sd = sd(autochamber_flux, na.rm = TRUE)
  )

# 4. Bias statistics by category
bias_assessment_filtered$bias_by_category <- all_flux_comparison_filtered %>%
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
    mean_autochamber_flux = mean(autochamber_flux, na.rm = TRUE),
    bias_percentage = round((mean_bias / mean_autochamber_flux) * 100, 1),
    .groups = 'drop'
  )

# 5. Overall correlation and regression
bias_assessment_filtered$overall_correlation <- cor.test(all_flux_comparison_filtered$starting_conc, 
                                                         all_flux_comparison_filtered$flux_difference, 
                                                         method = "spearman", use = "complete.obs")

bias_assessment_filtered$regression_model <- lm(flux_difference ~ starting_conc, data = all_flux_comparison_filtered)
bias_assessment_filtered$regression_summary <- summary(bias_assessment_filtered$regression_model)

bias_assessment_filtered$regression_stats <- data.frame(
  metric = c("Spearman correlation (rho)", "Spearman p-value", "Linear slope (bias per ppm)", 
             "Linear intercept", "R-squared", "Regression p-value", "Residual standard error"),
  value = c(
    round(bias_assessment_filtered$overall_correlation$estimate, 4),
    format(bias_assessment_filtered$overall_correlation$p.value, scientific = TRUE, digits = 3),
    format(coef(bias_assessment_filtered$regression_model)[2], scientific = TRUE, digits = 4),
    round(coef(bias_assessment_filtered$regression_model)[1], 4),
    round(bias_assessment_filtered$regression_summary$r.squared, 4),
    format(bias_assessment_filtered$regression_summary$coefficients[2,4], scientific = TRUE, digits = 3),
    round(bias_assessment_filtered$regression_summary$sigma, 4)
  )
)

# 6. Practical significance
typical_flux_range_filtered <- quantile(all_flux_comparison_filtered$autochamber_flux, c(0.25, 0.75), na.rm = TRUE)
typical_flux_iqr_filtered <- diff(typical_flux_range_filtered)

bias_assessment_filtered$practical_significance <- data.frame(
  metric = c("Overall mean bias", "Overall median bias", "Overall RMSE", 
             "Autochamber flux IQR", "Bias as % of flux IQR", 
             "Max bias (high conc)", "Max bias as % of typical flux",
             "Proportion of data with small bias", "Proportion with large bias"),
  value = c(
    round(mean(all_flux_comparison_filtered$flux_difference, na.rm = TRUE), 4),
    round(median(all_flux_comparison_filtered$flux_difference, na.rm = TRUE), 4),
    round(sqrt(mean(all_flux_comparison_filtered$flux_difference^2, na.rm = TRUE)), 4),
    round(typical_flux_iqr_filtered, 2),
    round((abs(mean(all_flux_comparison_filtered$flux_difference, na.rm = TRUE)) / typical_flux_iqr_filtered) * 100, 1),
    round(min(bias_assessment_filtered$bias_by_category$mean_bias), 2),
    round((abs(min(bias_assessment_filtered$bias_by_category$mean_bias)) / mean(typical_flux_range_filtered)) * 100, 1),
    round(sum(abs(all_flux_comparison_filtered$flux_difference) < 0.5, na.rm = TRUE) / 
            sum(!is.na(all_flux_comparison_filtered$flux_difference)) * 100, 1),
    round(sum(abs(all_flux_comparison_filtered$flux_difference) > 1.0, na.rm = TRUE) / 
            sum(!is.na(all_flux_comparison_filtered$flux_difference)) * 100, 1)
  )
)

# 7. High concentration assessment
high_conc_data_filtered <- filter(all_flux_comparison_filtered, starting_conc >= 850 & !is.na(starting_conc))

bias_assessment_filtered$high_concentration_impact <- data.frame(
  metric = c("High concentration observations", "Percentage of total data", 
             "Mean bias in high conc", "SD bias in high conc", "RMSE in high conc",
             "Correlation in high conc", "Mean autochamber flux (high conc)",
             "Bias as % of flux (high conc)"),
  value = c(
    nrow(high_conc_data_filtered),
    round(nrow(high_conc_data_filtered) / nrow(filter(all_flux_comparison_filtered, !is.na(starting_conc))) * 100, 1),
    round(mean(high_conc_data_filtered$flux_difference, na.rm = TRUE), 3),
    round(sd(high_conc_data_filtered$flux_difference, na.rm = TRUE), 3),
    round(sqrt(mean(high_conc_data_filtered$flux_difference^2, na.rm = TRUE)), 3),
    round(cor(high_conc_data_filtered$fluxbot_flux, high_conc_data_filtered$autochamber_flux, use = "complete.obs"), 3),
    round(mean(high_conc_data_filtered$autochamber_flux, na.rm = TRUE), 3),
    round((abs(mean(high_conc_data_filtered$flux_difference, na.rm = TRUE)) / 
             mean(high_conc_data_filtered$autochamber_flux, na.rm = TRUE)) * 100, 1)
  )
)

# 8. Manuscript summary table
bias_assessment_filtered$manuscript_summary <- data.frame(
  Concentration_Range = c("Low (< 450 ppm)", "Normal (450-600 ppm)", 
                          "Elevated (600-850 ppm)", "High (≥ 850 ppm)", "Overall"),
  N_Observations = c(
    bias_assessment_filtered$bias_by_category$n,
    nrow(filter(all_flux_comparison_filtered, !is.na(starting_conc)))
  ),
  Percent_of_Data = c(
    bias_assessment_filtered$bias_by_category$n / nrow(filter(all_flux_comparison_filtered, !is.na(starting_conc))) * 100,
    100
  ),
  Mean_Bias = c(
    bias_assessment_filtered$bias_by_category$mean_bias,
    mean(all_flux_comparison_filtered$flux_difference, na.rm = TRUE)
  ),
  RMSE = c(
    bias_assessment_filtered$bias_by_category$rmse,
    sqrt(mean(all_flux_comparison_filtered$flux_difference^2, na.rm = TRUE))
  ),
  Correlation_r = c(
    bias_assessment_filtered$bias_by_category$correlation,
    cor(all_flux_comparison_filtered$fluxbot_flux, all_flux_comparison_filtered$autochamber_flux, use = "complete.obs")
  ),
  Bias_Percent_of_Flux = c(
    bias_assessment_filtered$bias_by_category$bias_percentage,
    round((abs(mean(all_flux_comparison_filtered$flux_difference, na.rm = TRUE)) / 
             mean(all_flux_comparison_filtered$autochamber_flux, na.rm = TRUE)) * 100, 1)
  )
) %>%
  mutate(
    Percent_of_Data = round(Percent_of_Data, 1),
    Mean_Bias = round(Mean_Bias, 3),
    RMSE = round(RMSE, 3),
    Correlation_r = round(Correlation_r, 3)
  )

# ==============================================================================
# PRINT RESULTS
# ==============================================================================

print("=== FILTERED BIAS ASSESSMENT RESULTS ===")
print("Dataset Summary:")
print(bias_assessment_filtered$dataset_summary)

print("\nConcentration Distribution:")
print(bias_assessment_filtered$concentration_stats)
print(bias_assessment_filtered$conc_category_distribution)

print("\nBias Statistics by Category:")
print(bias_assessment_filtered$bias_by_category)

print("\nOverall Correlation and Regression:")
print(bias_assessment_filtered$regression_stats)

print("\nPractical Significance:")
print(bias_assessment_filtered$practical_significance)

print("\nHigh Concentration Impact:")
print(bias_assessment_filtered$high_concentration_impact)

print("\nManuscript Summary Table:")
print(bias_assessment_filtered$manuscript_summary)

# ==============================================================================
# VISUALIZATION: FILTERED DATA PLOTS
# ==============================================================================

library(viridis)
library(ggpointdensity)

cat("Creating visualizations for filtered data...\n")

# Plot 1: Distribution of bias by concentration category (box plots)
p1_boxplot <- all_flux_comparison_filtered %>% 
  filter(!is.na(starting_conc)) %>%
  ggplot(aes(x = conc_category, y = flux_difference, fill = conc_category)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(alpha = 0.15, width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_viridis_d(option = "plasma") +
  labs(
    x = "Starting CO₂ Concentration Category",
    y = "Flux Bias (Fluxbot - Autochamber)",
    title = "Distribution of Fluxbot Bias by Concentration Range (Filtered Data)",
    subtitle = "Negative values indicate fluxbot underestimation"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

print(p1_boxplot)
ggsave("fluxbot_bias_boxplots_FILTERED.png", p1_boxplot, width = 10, height = 6, dpi = 300)

# Plot 2: RMSE and correlation by concentration category (bar plots)
p2_precision <- bias_assessment_filtered$bias_by_category %>%
  ggplot(aes(x = conc_category)) +
  geom_col(aes(y = rmse), fill = "coral", alpha = 0.7) +
  geom_text(aes(y = rmse + 0.05, label = paste0("r = ", round(correlation, 2))), size = 3.5) +
  labs(
    x = "Starting CO₂ Concentration Category",
    y = "Root Mean Square Error",
    title = "Measurement Precision vs Starting CO₂ Concentration (Filtered Data)",
    subtitle = "Text shows correlation coefficient (r) between fluxbot and autochamber"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p2_precision)
ggsave("fluxbot_precision_barplot_FILTERED.png", p2_precision, width = 10, height = 6, dpi = 300)

# Plot 3: Bias vs starting concentration with point density coloring
p3_density <- all_flux_comparison_filtered %>%
  filter(!is.na(starting_conc)) %>%
  ggplot(aes(x = starting_conc, y = flux_difference)) +
  geom_pointdensity(size = 1.5, alpha = 0.7) +
  scale_color_viridis_c(name = "Observation\nDensity", option = "plasma") +
  geom_smooth(method = "lm", color = "red", se = TRUE, size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(
    x = expression("Starting CO"[2] ~ "Concentration (ppm)"),
    y = "Flux Bias (Fluxbot - Autochamber)",
    #title = "Fluxbot Bias vs Starting CO₂ Concentration (Filtered Data)",
    #subtitle = "Points colored by local data density"
  ) +
  theme_classic()

print(p3_density)
ggsave("fluxbot_bias_density_scatter_FILTERED.png", p3_density, width = 12, height = 8, dpi = 300)

# Plot 4: Faceted by concentration ranges with detailed patterns
facet_data_filtered <- all_flux_comparison_filtered %>%
  filter(!is.na(starting_conc)) %>%
  mutate(
    conc_range = case_when(
      starting_conc < 500 ~ "300-500 ppm\n(Low)",
      starting_conc >= 500 & starting_conc < 700 ~ "500-700 ppm\n(Normal)",
      starting_conc >= 700 & starting_conc < 900 ~ "700-900 ppm\n(Elevated)",
      starting_conc >= 900 ~ "900+ ppm\n(High)"
    ),
    conc_range = factor(conc_range, levels = c("300-500 ppm\n(Low)", "500-700 ppm\n(Normal)", 
                                               "700-900 ppm\n(Elevated)", "900+ ppm\n(High)"))
  )

p4_facet <- facet_data_filtered %>%
  ggplot(aes(x = starting_conc, y = flux_difference)) +
  geom_point(alpha = 0.6, color = "darkblue", size = 1.2) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  facet_wrap(~conc_range, scales = "free_x", ncol = 2) +
  labs(
    x = expression("Starting CO"[2] ~ "Concentration (ppm)"),
    y = "Flux Bias (Fluxbot - Autochamber)",
    #title = "Data Density: Faceted by Concentration Range (Filtered Data)",
    #subtitle = "Each panel shows detailed patterns within concentration ranges"
  ) +
  theme_classic() +
  theme(strip.text = element_text(size = 10))

print(p4_facet)
ggsave("fluxbot_bias_faceted_ranges_FILTERED.png", p4_facet, width = 12, height = 10, dpi = 300)

# Plot 5: Fluxbot vs Autochamber comparison colored by starting concentration
p5_comparison <- all_flux_comparison_filtered %>%
  filter(!is.na(starting_conc)) %>%
  ggplot(aes(x = autochamber_flux, y = fluxbot_flux, color = starting_conc)) +
  geom_point(size = 2, alpha = 0.7) +
  scale_color_viridis_c(name = "Starting CO₂\n(ppm)", option = "plasma") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", size = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", alpha = 0.3) +
  labs(
    x = expression(paste("Autochamber flux estimate, ", mu, "mol ", CO[2], " ", m^-2, s^-1)),
    y = expression(paste("Fluxbot flux estimate, ", mu, "mol ", CO[2], " ", m^-2, s^-1)),
    title = "Fluxbot vs Autochamber Flux Estimates (Filtered Data)",
    subtitle = "Points colored by starting CO₂ concentration"
  ) +
  theme_classic() +
  coord_fixed() +
  xlim(0, 6) + ylim(0, 6)

print(p5_comparison)
ggsave("fluxbot_vs_autochamber_FILTERED.png", p5_comparison, width = 10, height = 8, dpi = 300)

cat("All filtered data visualizations saved as PNG files\n")

# ==============================================================================
# SAVE RESULTS
# ==============================================================================

save(bias_assessment_filtered, file = "fluxbot_bias_assessment_FILTERED_results.RData")
write.csv(bias_assessment_filtered$manuscript_summary, "fluxbot_bias_summary_FILTERED_table.csv", row.names = FALSE)

# Store filtered dataset for 3-hour rolling mean analysis
save(all_flux_comparison_filtered, file = "all_flux_comparison_filtered.RData")

cat("\n=== FILTERED ANALYSIS COMPLETE ===\n")
cat("Results saved to 'fluxbot_bias_assessment_FILTERED_results.RData'\n")
cat("Summary table exported to 'fluxbot_bias_summary_FILTERED_table.csv'\n")
cat("Filtered dataset saved for 3-hour rolling mean analysis\n")
cat("Visualizations saved as PNG files with '_FILTERED' suffix\n")

cat("\nKey findings for SI section:\n")
cat("- Total observations (filtered):", nrow(all_flux_comparison_filtered), "\n")
cat("- Statistical significance: p =", format(bias_assessment_filtered$overall_correlation$p.value, scientific = TRUE), "\n")
cat("- Effect size (R²):", round(bias_assessment_filtered$regression_summary$r.squared, 4), "\n")
cat("- Practical significance: bias represents", 
    round((abs(mean(all_flux_comparison_filtered$flux_difference, na.rm = TRUE)) / 
             mean(all_flux_comparison_filtered$autochamber_flux, na.rm = TRUE)) * 100, 1), 
    "% of mean flux\n")
cat("- Most data (", round(sum(bias_assessment_filtered$bias_by_category$n[1:2]) / 
                             sum(bias_assessment_filtered$bias_by_category$n) * 100, 1), 
    "%) in low-normal range with minimal bias\n")