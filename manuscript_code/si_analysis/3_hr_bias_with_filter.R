# ==============================================================================
# 3-HOUR ROLLING MEAN BIAS ASSESSMENT WITH PROJECT FILTERING
# Run this AFTER running the filtered bias assessment
# ==============================================================================

library(dplyr)
library(ggplot2)
library(zoo)  # For rolling mean calculations

# Load the filter_iqr function
#source("filter_iqr.R")

# Check that filtered dataset exists or create it
if(!exists("all_flux_comparison_filtered")) {
  if(file.exists("all_flux_comparison_filtered.RData")) {
    load("all_flux_comparison_filtered.RData")
    cat("Loaded filtered dataset from file\n")
  } else {
    # If not available, need to run the filtering first
    if(!exists("HF_fluxestimates") || !exists("HFauto_fluxestimates")) {
      stop("Need to run filtered bias assessment first or ensure HF_fluxestimates and HFauto_fluxestimates are loaded")
    }
    
    # Apply same filtering as in the first script
    cat("Creating filtered dataset...\n")
    
    #### FLUXBOTS: Apply negative flux removal and IQR filtering
    HF_fluxestimates_subset <- HF_fluxestimates[HF_fluxestimates$fluxL_umolm2sec >= 0, ]
    HF_fluxestimates_filtered_bias <- HF_fluxestimates_subset %>% 
      filter_iqr("fluxL_umolm2sec")
    
    #### AUTOCHAMBERS: Apply negative flux removal and IQR filtering  
    HFauto_fluxestimates_subset <- HFauto_fluxestimates[HFauto_fluxestimates$fluxL_umolm2sec >= 0, ]
    HFauto_fluxestimates_filtered_bias <- HFauto_fluxestimates_subset %>% 
      filter_iqr("fluxL_umolm2sec")
    
    # Average autochamber data by hour and stand
    HFauto_fluxestimates_filtered_avg <- HFauto_fluxestimates_filtered_bias %>%
      group_by(hour_of_obs, stand) %>%
      summarise(fluxL_umolm2sec = mean(fluxL_umolm2sec, na.rm = TRUE),
                .groups = 'drop') %>%
      mutate(method = "autochamber")
    
    # Extract fluxbot data with concentration info
    fluxbot_data_filtered <- HF_fluxestimates_filtered_bias %>%
      mutate(method = "fluxbot") %>%
      select(hour_of_obs, stand, fluxL_umolm2sec, starting_concen, ending_concen) %>%
      rename(fluxbot_flux = fluxL_umolm2sec, starting_conc = starting_concen, ending_conc = ending_concen)
    
    # Extract autochamber data (already averaged)
    autochamber_data_filtered <- HFauto_fluxestimates_filtered_avg %>%
      select(hour_of_obs, stand, fluxL_umolm2sec) %>%
      rename(autochamber_flux = fluxL_umolm2sec)
    
    # Merge fluxbot and autochamber data
    all_flux_comparison_filtered <- fluxbot_data_filtered %>%
      left_join(autochamber_data_filtered, by = c("hour_of_obs", "stand")) %>%
      filter(!is.na(fluxbot_flux) & !is.na(autochamber_flux))
    
    cat("Created filtered comparison dataset:", nrow(all_flux_comparison_filtered), "observations\n")
  }
}

cat("=== 3-HOUR ROLLING MEAN BIAS ASSESSMENT ===\n")
cat("Starting with filtered dataset:", nrow(all_flux_comparison_filtered), "observations\n\n")

# ==============================================================================
# CALCULATE 3-HOUR ROLLING MEANS
# ==============================================================================

# Sort data by time for proper rolling calculations
all_flux_comparison_sorted <- all_flux_comparison_filtered %>%
  arrange(hour_of_obs)

# Calculate 3-hour rolling means by stand
rolling_flux_data_filtered <- all_flux_comparison_sorted %>%
  group_by(stand) %>%
  arrange(hour_of_obs) %>%
  mutate(
    # 3-hour rolling means (center-aligned)
    fluxbot_flux_3h = rollapply(fluxbot_flux, width = 3, FUN = mean, fill = NA, align = "center"),
    autochamber_flux_3h = rollapply(autochamber_flux, width = 3, FUN = mean, fill = NA, align = "center"),
    starting_conc_3h = rollapply(starting_conc, width = 3, FUN = mean, fill = NA, align = "center", na.rm = TRUE),
    ending_conc_3h = rollapply(ending_conc, width = 3, FUN = mean, fill = NA, align = "center", na.rm = TRUE)
  ) %>%
  ungroup() %>%
  # Remove rows where rolling means couldn't be calculated
  filter(!is.na(fluxbot_flux_3h) & !is.na(autochamber_flux_3h) & !is.na(starting_conc_3h))

# Create concentration categories and bias metrics for rolling means
rolling_analysis_data_filtered <- rolling_flux_data_filtered %>%
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

cat("Rolling mean dataset created:", nrow(rolling_analysis_data_filtered), "observations\n")
cat("Starting concentration range (3h means):", range(rolling_analysis_data_filtered$starting_conc_3h, na.rm = TRUE), "ppm\n\n")

# ==============================================================================
# QUANTITATIVE BIAS ASSESSMENT FOR 3-HOUR ROLLING MEANS
# ==============================================================================

bias_assessment_3h_filtered <- list()

# 1. Dataset characteristics (3h rolling means)
bias_assessment_3h_filtered$dataset_summary <- data.frame(
  metric = c("Total observations (3h rolling, filtered)", "Observations with concentration data", 
             "Percentage with concentration data", "Date range", "Measurement period"),
  value = c(
    nrow(rolling_analysis_data_filtered),
    sum(!is.na(rolling_analysis_data_filtered$starting_conc_3h)),
    round(sum(!is.na(rolling_analysis_data_filtered$starting_conc_3h)) / nrow(rolling_analysis_data_filtered) * 100, 1),
    paste(range(as.Date(rolling_analysis_data_filtered$hour_of_obs), na.rm = TRUE), collapse = " to "),
    paste(round(as.numeric(diff(range(as.Date(rolling_analysis_data_filtered$hour_of_obs), na.rm = TRUE)))), "days")
  )
)

# 2. Concentration distribution (3h rolling means)
bias_assessment_3h_filtered$concentration_stats <- rolling_analysis_data_filtered %>%
  filter(!is.na(starting_conc_3h)) %>%
  summarise(
    min_conc = min(starting_conc_3h),
    q25_conc = quantile(starting_conc_3h, 0.25),
    median_conc = median(starting_conc_3h),
    q75_conc = quantile(starting_conc_3h, 0.75),
    max_conc = max(starting_conc_3h),
    mean_conc = mean(starting_conc_3h),
    sd_conc = sd(starting_conc_3h)
  )

bias_assessment_3h_filtered$conc_category_distribution <- rolling_analysis_data_filtered %>%
  filter(!is.na(starting_conc_3h)) %>%
  group_by(conc_category_3h) %>%
  summarise(
    n_obs = n(),
    percentage = round(n() / nrow(filter(rolling_analysis_data_filtered, !is.na(starting_conc_3h))) * 100, 1),
    .groups = 'drop'
  )

# 3. Flux ranges (3h rolling means)
bias_assessment_3h_filtered$flux_ranges <- rolling_analysis_data_filtered %>%
  summarise(
    fluxbot_min = min(fluxbot_flux_3h, na.rm = TRUE),
    fluxbot_max = max(fluxbot_flux_3h, na.rm = TRUE),
    fluxbot_mean = mean(fluxbot_flux_3h, na.rm = TRUE),
    fluxbot_sd = sd(fluxbot_flux_3h, na.rm = TRUE),
    autochamber_min = min(autochamber_flux_3h, na.rm = TRUE),
    autochamber_max = max(autochamber_flux_3h, na.rm = TRUE),
    autochamber_mean = mean(autochamber_flux_3h, na.rm = TRUE),
    autochamber_sd = sd(autochamber_flux_3h, na.rm = TRUE)
  )

# 4. Bias statistics by category (3h rolling means)
bias_assessment_3h_filtered$bias_by_category <- rolling_analysis_data_filtered %>%
  filter(!is.na(starting_conc_3h)) %>%
  group_by(conc_category_3h) %>%
  summarise(
    n = n(),
    mean_bias = mean(flux_difference_3h, na.rm = TRUE),
    median_bias = median(flux_difference_3h, na.rm = TRUE),
    sd_bias = sd(flux_difference_3h, na.rm = TRUE),
    q25_bias = quantile(flux_difference_3h, 0.25, na.rm = TRUE),
    q75_bias = quantile(flux_difference_3h, 0.75, na.rm = TRUE),
    rmse = sqrt(mean(flux_difference_3h^2, na.rm = TRUE)),
    mean_abs_error = mean(abs(flux_difference_3h), na.rm = TRUE),
    correlation = cor(fluxbot_flux_3h, autochamber_flux_3h, use = "complete.obs"),
    mean_autochamber_flux = mean(autochamber_flux_3h, na.rm = TRUE),
    bias_percentage = round((mean_bias / mean_autochamber_flux) * 100, 1),
    .groups = 'drop'
  )

# 5. Overall correlation and regression (3h rolling means)
bias_assessment_3h_filtered$overall_correlation <- cor.test(rolling_analysis_data_filtered$starting_conc_3h, 
                                                            rolling_analysis_data_filtered$flux_difference_3h, 
                                                            method = "spearman", use = "complete.obs")

bias_assessment_3h_filtered$regression_model <- lm(flux_difference_3h ~ starting_conc_3h, data = rolling_analysis_data_filtered)
bias_assessment_3h_filtered$regression_summary <- summary(bias_assessment_3h_filtered$regression_model)

bias_assessment_3h_filtered$regression_stats <- data.frame(
  metric = c("Spearman correlation (rho)", "Spearman p-value", "Linear slope (bias per ppm)", 
             "Linear intercept", "R-squared", "Regression p-value", "Residual standard error"),
  value = c(
    round(bias_assessment_3h_filtered$overall_correlation$estimate, 4),
    format(bias_assessment_3h_filtered$overall_correlation$p.value, scientific = TRUE, digits = 3),
    format(coef(bias_assessment_3h_filtered$regression_model)[2], scientific = TRUE, digits = 4),
    round(coef(bias_assessment_3h_filtered$regression_model)[1], 4),
    round(bias_assessment_3h_filtered$regression_summary$r.squared, 4),
    format(bias_assessment_3h_filtered$regression_summary$coefficients[2,4], scientific = TRUE, digits = 3),
    round(bias_assessment_3h_filtered$regression_summary$sigma, 4)
  )
)

# 6. Practical significance (3h rolling means)
typical_flux_range_3h <- quantile(rolling_analysis_data_filtered$autochamber_flux_3h, c(0.25, 0.75), na.rm = TRUE)
typical_flux_iqr_3h <- diff(typical_flux_range_3h)

bias_assessment_3h_filtered$practical_significance <- data.frame(
  metric = c("Overall mean bias", "Overall median bias", "Overall RMSE", 
             "Autochamber flux IQR", "Bias as % of flux IQR", 
             "Max bias (high conc)", "Max bias as % of typical flux",
             "Proportion of data with small bias", "Proportion with large bias"),
  value = c(
    round(mean(rolling_analysis_data_filtered$flux_difference_3h, na.rm = TRUE), 4),
    round(median(rolling_analysis_data_filtered$flux_difference_3h, na.rm = TRUE), 4),
    round(sqrt(mean(rolling_analysis_data_filtered$flux_difference_3h^2, na.rm = TRUE)), 4),
    round(typical_flux_iqr_3h, 2),
    round((abs(mean(rolling_analysis_data_filtered$flux_difference_3h, na.rm = TRUE)) / typical_flux_iqr_3h) * 100, 1),
    round(min(bias_assessment_3h_filtered$bias_by_category$mean_bias), 2),
    round((abs(min(bias_assessment_3h_filtered$bias_by_category$mean_bias)) / mean(typical_flux_range_3h)) * 100, 1),
    round(sum(abs(rolling_analysis_data_filtered$flux_difference_3h) < 0.5, na.rm = TRUE) / 
            sum(!is.na(rolling_analysis_data_filtered$flux_difference_3h)) * 100, 1),
    round(sum(abs(rolling_analysis_data_filtered$flux_difference_3h) > 1.0, na.rm = TRUE) / 
            sum(!is.na(rolling_analysis_data_filtered$flux_difference_3h)) * 100, 1)
  )
)

# 7. High concentration assessment (3h rolling means)
high_conc_data_3h <- filter(rolling_analysis_data_filtered, starting_conc_3h >= 850 & !is.na(starting_conc_3h))

bias_assessment_3h_filtered$high_concentration_impact <- data.frame(
  metric = c("High concentration observations", "Percentage of total data", 
             "Mean bias in high conc", "SD bias in high conc", "RMSE in high conc",
             "Correlation in high conc", "Mean autochamber flux (high conc)",
             "Bias as % of flux (high conc)"),
  value = c(
    nrow(high_conc_data_3h),
    round(nrow(high_conc_data_3h) / nrow(filter(rolling_analysis_data_filtered, !is.na(starting_conc_3h))) * 100, 1),
    round(mean(high_conc_data_3h$flux_difference_3h, na.rm = TRUE), 3),
    round(sd(high_conc_data_3h$flux_difference_3h, na.rm = TRUE), 3),
    round(sqrt(mean(high_conc_data_3h$flux_difference_3h^2, na.rm = TRUE)), 3),
    round(cor(high_conc_data_3h$fluxbot_flux_3h, high_conc_data_3h$autochamber_flux_3h, use = "complete.obs"), 3),
    round(mean(high_conc_data_3h$autochamber_flux_3h, na.rm = TRUE), 3),
    round((abs(mean(high_conc_data_3h$flux_difference_3h, na.rm = TRUE)) / 
             mean(high_conc_data_3h$autochamber_flux_3h, na.rm = TRUE)) * 100, 1)
  )
)

# 8. Manuscript summary table (3h rolling means)
bias_assessment_3h_filtered$manuscript_summary <- data.frame(
  Concentration_Range = c("Low (< 450 ppm)", "Normal (450-600 ppm)", 
                          "Elevated (600-850 ppm)", "High (≥ 850 ppm)", "Overall"),
  N_Observations = c(
    bias_assessment_3h_filtered$bias_by_category$n,
    nrow(filter(rolling_analysis_data_filtered, !is.na(starting_conc_3h)))
  ),
  Percent_of_Data = c(
    bias_assessment_3h_filtered$bias_by_category$n / nrow(filter(rolling_analysis_data_filtered, !is.na(starting_conc_3h))) * 100,
    100
  ),
  Mean_Bias = c(
    bias_assessment_3h_filtered$bias_by_category$mean_bias,
    mean(rolling_analysis_data_filtered$flux_difference_3h, na.rm = TRUE)
  ),
  RMSE = c(
    bias_assessment_3h_filtered$bias_by_category$rmse,
    sqrt(mean(rolling_analysis_data_filtered$flux_difference_3h^2, na.rm = TRUE))
  ),
  Correlation_r = c(
    bias_assessment_3h_filtered$bias_by_category$correlation,
    cor(rolling_analysis_data_filtered$fluxbot_flux_3h, rolling_analysis_data_filtered$autochamber_flux_3h, use = "complete.obs")
  ),
  Bias_Percent_of_Flux = c(
    bias_assessment_3h_filtered$bias_by_category$bias_percentage,
    round((abs(mean(rolling_analysis_data_filtered$flux_difference_3h, na.rm = TRUE)) / 
             mean(rolling_analysis_data_filtered$autochamber_flux_3h, na.rm = TRUE)) * 100, 1)
  )
) %>%
  mutate(
    Percent_of_Data = round(Percent_of_Data, 1),
    Mean_Bias = round(Mean_Bias, 3),
    RMSE = round(RMSE, 3),
    Correlation_r = round(Correlation_r, 3)
  )

# ==============================================================================
# COMPARISON WITH NON-ROLLING DATA
# ==============================================================================

# Load the non-rolling filtered results for comparison if available
if(exists("bias_assessment_filtered")) {
  bias_assessment_3h_filtered$comparison_with_original <- data.frame(
    metric = c("Original correlation (rho)", "3h rolling correlation (rho)",
               "Original R-squared", "3h rolling R-squared",
               "Original overall bias", "3h rolling overall bias",
               "Original overall RMSE", "3h rolling overall RMSE"),
    value = c(
      round(bias_assessment_filtered$overall_correlation$estimate, 4),
      round(bias_assessment_3h_filtered$overall_correlation$estimate, 4),
      round(bias_assessment_filtered$regression_summary$r.squared, 4),
      round(bias_assessment_3h_filtered$regression_summary$r.squared, 4),
      round(mean(all_flux_comparison_filtered$flux_difference, na.rm = TRUE), 4),
      round(mean(rolling_analysis_data_filtered$flux_difference_3h, na.rm = TRUE), 4),
      round(sqrt(mean(all_flux_comparison_filtered$flux_difference^2, na.rm = TRUE)), 4),
      round(sqrt(mean(rolling_analysis_data_filtered$flux_difference_3h^2, na.rm = TRUE)), 4)
    )
  )
  
  cat("Comparison with original filtered data available\n")
}

# ==============================================================================
# PRINT RESULTS
# ==============================================================================

print("=== 3-HOUR ROLLING MEAN BIAS ASSESSMENT RESULTS ===")
print("Dataset Summary:")
print(bias_assessment_3h_filtered$dataset_summary)

print("\nConcentration Distribution (3h rolling means):")
print(bias_assessment_3h_filtered$concentration_stats)
print(bias_assessment_3h_filtered$conc_category_distribution)

print("\nBias Statistics by Category (3h rolling means):")
print(bias_assessment_3h_filtered$bias_by_category)

print("\nOverall Correlation and Regression (3h rolling means):")
print(bias_assessment_3h_filtered$regression_stats)

print("\nPractical Significance (3h rolling means):")
print(bias_assessment_3h_filtered$practical_significance)

print("\nHigh Concentration Impact (3h rolling means):")
print(bias_assessment_3h_filtered$high_concentration_impact)

print("\nManuscript Summary Table (3h rolling means):")
print(bias_assessment_3h_filtered$manuscript_summary)

if(exists("bias_assessment_3h_filtered$comparison_with_original")) {
  print("\nComparison with Original (non-rolling) Data:")
  print(bias_assessment_3h_filtered$comparison_with_original)
}

# ==============================================================================
# VISUALIZATION: 3-HOUR ROLLING MEAN PLOTS
# ==============================================================================

library(viridis)
library(ggpointdensity)

cat("Creating visualizations for 3-hour rolling mean data...\n")

# Plot 1: Distribution of bias by concentration category (box plots) - 3h rolling means
p1_boxplot_3h <- rolling_analysis_data_filtered %>% 
  filter(!is.na(starting_conc_3h)) %>%
  ggplot(aes(x = conc_category_3h, y = flux_difference_3h, fill = conc_category_3h)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(alpha = 0.15, width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_viridis_d(option = "plasma") +
  labs(
    #x = "3-Hour Mean Starting CO₂ Concentration Category",
    x = expression("3-Hour Mean Starting CO"[2] ~ "Concentration Category"),
    y = "3-Hour Mean Flux Bias (Fluxbot - Autochamber)",
    title = "Distribution of Smoothed Fluxbot Bias by Concentration Range",
    subtitle = "3-hour rolling means - reduces short-term noise effects"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

print(p1_boxplot_3h)
ggsave("fluxbot_bias_boxplots_3H_ROLLING.png", p1_boxplot_3h, width = 10, height = 6, dpi = 300)

# Plot 2: RMSE and correlation by concentration category (bar plots) - 3h rolling means
p2_precision_3h <- bias_assessment_3h_filtered$bias_by_category %>%
  ggplot(aes(x = conc_category_3h)) +
  geom_col(aes(y = rmse), fill = "lightblue", alpha = 0.7) +
  geom_text(aes(y = rmse + 0.03, label = paste0("r = ", round(correlation, 2))), size = 3.5) +
  labs(
    x = "3-Hour Mean Starting CO₂ Concentration Category",
    y = "Root Mean Square Error (3-Hour Means)",
    title = "Smoothed Measurement Precision vs Starting CO₂",
    subtitle = "Based on 3-hour rolling means - text shows correlation coefficients"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p2_precision_3h)
ggsave("fluxbot_precision_barplot_3H_ROLLING.png", p2_precision_3h, width = 10, height = 6, dpi = 300)

# Plot 3: Bias vs starting concentration with point density coloring - 3h rolling means
p3_density_3h <- rolling_analysis_data_filtered %>%
  filter(!is.na(starting_conc_3h)) %>%
  ggplot(aes(x = starting_conc_3h, y = flux_difference_3h)) +
  geom_pointdensity(size = 1.5, alpha = 0.7) +
  scale_color_viridis_c(name = "Observation\nDensity", option = "plasma") +
  geom_smooth(method = "lm", color = "red", se = TRUE, size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(
    #x = "3-Hour Mean Starting CO₂ Concentration (ppm)",
    x = expression("3-Hour Mean Starting CO"[2] ~ "Concentration (ppm)"),
    y = "3-Hour Mean Flux Bias (Fluxbot - Autochamber)",
    #title = "Smoothed Fluxbot Bias vs Starting CO₂ Concentration",
    #subtitle = "3-hour rolling means - points colored by local data density"
  ) +
  theme_classic()

print(p3_density_3h)
ggsave("fluxbot_bias_density_scatter_3H_ROLLING.png", p3_density_3h, width = 12, height = 8, dpi = 300)

# Plot 4: Faceted by concentration ranges - 3h rolling means
facet_data_3h <- rolling_analysis_data_filtered %>%
  filter(!is.na(starting_conc_3h)) %>%
  mutate(
    conc_range_3h = case_when(
      starting_conc_3h < 500 ~ "400-500 ppm\n(Low)",
      starting_conc_3h >= 500 & starting_conc_3h < 700 ~ "500-700 ppm\n(Normal)",
      starting_conc_3h >= 700 & starting_conc_3h < 900 ~ "700-900 ppm\n(Elevated)",
      starting_conc_3h >= 900 ~ "900+ ppm\n(High)"
    ),
    conc_range_3h = factor(conc_range_3h, levels = c("400-500 ppm\n(Low)", "500-700 ppm\n(Normal)", 
                                                     "700-900 ppm\n(Elevated)", "900+ ppm\n(High)"))
  )

p4_facet_3h <- facet_data_3h %>%
  ggplot(aes(x = starting_conc_3h, y = flux_difference_3h)) +
  geom_point(alpha = 0.6, color = "darkblue", size = 1.2) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  facet_wrap(~conc_range_3h, scales = "free_x", ncol = 2) +
  labs(
    #x = "3-Hour Mean Starting CO₂ Concentration (ppm)",
    x = expression("3-Hour Mean Starting CO"[2] ~ "Concentration (ppm)"),
    y = "3-Hour Mean Flux Bias (Fluxbot - Autochamber)",
    #title = "Smoothed Data Density: Faceted by Concentration Range",
    #subtitle = "3-hour rolling means - each panel shows patterns within concentration ranges"
  ) +
  theme_classic() +
  theme(strip.text = element_text(size = 10))

print(p4_facet_3h)
ggsave("fluxbot_bias_faceted_ranges_3H_ROLLING.png", p4_facet_3h, width = 12, height = 10, dpi = 300)

# Plot 5: Fluxbot vs Autochamber comparison colored by starting concentration - 3h rolling means
p5_comparison_3h <- rolling_analysis_data_filtered %>%
  filter(!is.na(starting_conc_3h)) %>%
  ggplot(aes(x = autochamber_flux_3h, y = fluxbot_flux_3h, color = starting_conc_3h)) +
  geom_point(size = 2, alpha = 0.7) +
  scale_color_viridis_c(name = "Starting CO₂\n(ppm)", option = "plasma") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", size = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", alpha = 0.3) +
  labs(
    x = expression(paste("3h Mean Autochamber flux, ", mu, "mol ", CO[2], " ", m^-2, s^-1)),
    y = expression(paste("3h Mean Fluxbot flux, ", mu, "mol ", CO[2], " ", m^-2, s^-1)),
    title = "Smoothed Fluxbot vs Autochamber Flux Estimates",
    subtitle = "3-hour rolling means - points colored by starting CO₂ concentration"
  ) +
  theme_classic() +
  coord_fixed() +
  xlim(0, 6) + ylim(0, 6)

print(p5_comparison_3h)
ggsave("fluxbot_vs_autochamber_3H_ROLLING.png", p5_comparison_3h, width = 10, height = 8, dpi = 300)

# Plot 6: Direct comparison between original and rolling mean results
if(exists("bias_assessment_filtered")) {
  comparison_data <- data.frame(
    analysis_type = rep(c("Original Data", "3-Hour Rolling Means"), each = 4),
    concentration = rep(c("Low", "Normal", "Elevated", "High"), 2),
    rmse = c(bias_assessment_filtered$bias_by_category$rmse, bias_assessment_3h_filtered$bias_by_category$rmse),
    correlation = c(bias_assessment_filtered$bias_by_category$correlation, bias_assessment_3h_filtered$bias_by_category$correlation)
  )
  
  p6_comparison <- comparison_data %>%
    ggplot(aes(x = concentration, y = rmse, fill = analysis_type)) +
    geom_col(position = "dodge", alpha = 0.7) +
    geom_text(aes(label = paste0("r=", round(correlation, 2))), 
              position = position_dodge(width = 0.9), 
              vjust = -0.2, size = 3) +
    scale_fill_manual(values = c("Original Data" = "coral", "3-Hour Rolling Means" = "lightblue")) +
    labs(
      x = "Starting CO₂ Concentration Category",
      y = "Root Mean Square Error",
      title = "Comparison: Original vs 3-Hour Rolling Mean Analysis",
      subtitle = "Lower RMSE and higher correlations indicate better measurement precision",
      fill = "Analysis Type"
    ) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p6_comparison)
  ggsave("comparison_original_vs_rolling_means.png", p6_comparison, width = 12, height = 8, dpi = 300)
  
  cat("Comparison plot created\n")
}

cat("All 3-hour rolling mean visualizations saved as PNG files\n")

# ==============================================================================
# SAVE RESULTS
# ==============================================================================

save(bias_assessment_3h_filtered, file = "fluxbot_bias_assessment_3H_ROLLING_FILTERED_results.RData")
write.csv(bias_assessment_3h_filtered$manuscript_summary, "fluxbot_bias_summary_3H_ROLLING_FILTERED_table.csv", row.names = FALSE)
save(rolling_analysis_data_filtered, file = "rolling_analysis_data_filtered.RData")

cat("\n=== 3-HOUR ROLLING MEAN ANALYSIS COMPLETE ===\n")
cat("Results saved to 'fluxbot_bias_assessment_3H_ROLLING_FILTERED_results.RData'\n")
cat("Summary table exported to 'fluxbot_bias_summary_3H_ROLLING_FILTERED_table.csv'\n")
cat("Rolling mean dataset saved to 'rolling_analysis_data_filtered.RData'\n")
cat("Visualizations saved as PNG files with '_3H_ROLLING' suffix\n")

cat("\nKey findings comparison:\n")
if(exists("bias_assessment_filtered")) {
  cat("- Original data observations:", nrow(all_flux_comparison_filtered), "\n")
  cat("- 3h rolling data observations:", nrow(rolling_analysis_data_filtered), "\n")
  cat("- Original correlation (rho):", round(bias_assessment_filtered$overall_correlation$estimate, 4), "\n")
  cat("- 3h rolling correlation (rho):", round(bias_assessment_3h_filtered$overall_correlation$estimate, 4), "\n")
  cat("- Original R²:", round(bias_assessment_filtered$regression_summary$r.squared, 4), "\n")
  cat("- 3h rolling R²:", round(bias_assessment_3h_filtered$regression_summary$r.squared, 4), "\n")
  
  # Determine if rolling means strengthen or weaken the signal
  if(abs(bias_assessment_3h_filtered$overall_correlation$estimate) > abs(bias_assessment_filtered$overall_correlation$estimate)) {
    cat("✅ Rolling means STRENGTHEN the correlation signal\n")
  } else {
    cat("❌ Rolling means WEAKEN the correlation signal\n")
  }
  
  if(bias_assessment_3h_filtered$regression_summary$r.squared > bias_assessment_filtered$regression_summary$r.squared) {
    cat("✅ Rolling means IMPROVE the explanatory power (R²)\n")
  } else {
    cat("❌ Rolling means REDUCE the explanatory power (R²)\n")
  }
}