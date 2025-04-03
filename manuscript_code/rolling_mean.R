library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)  # For rollapply function
library(lubridate)
library(epiR)  # For Lin's CCC calculation

# Step 1: Filter the dataset for hours where at least 5 autochambers and 3 fluxbots are functioning within each stand
filtered_data <- merged_data %>%
  group_by(hour_of_obs, stand) %>%
  filter(
    sum(method == "autochamber" & !is.na(fluxL_umolm2sec)) >= 5 &
      sum(method == "fluxbot" & !is.na(fluxL_umolm2sec)) >= 3
  ) %>%
  ungroup()

# Step 2: Calculate the mean flux for each hour, stand, and method
mean_data <- filtered_data %>%
  group_by(hour_of_obs, stand, method) %>%
  summarize(mean_flux = mean(fluxL_umolm2sec, na.rm = TRUE)) %>%
  ungroup()

# Step 3: Pivot data so that we have autochamber and fluxbot as columns
pivoted_data <- mean_data %>%
  pivot_wider(names_from = method, values_from = mean_flux)

pivoted_data <- pivoted_data %>%
  filter(autochamber > 1 & (!is.na(fluxbot) | !is.na(autochamber)))

# Step 4: Bin autochamber and fluxbot data into hourly intervals for comparison
pivoted_data <- pivoted_data %>%
  mutate(hour_bin = floor_date(hour_of_obs, "hour")) %>%
  group_by(hour_bin) %>%
  summarize(
    autochamber_mean = mean(autochamber, na.rm = TRUE),
    fluxbot_mean = mean(fluxbot, na.rm = TRUE)
  ) %>%
  filter(!is.na(autochamber_mean) & !is.na(fluxbot_mean))  # Ensure both are non-NA

# Step 5: Calculate 3-hour rolling means for both autochamber and fluxbot
pivoted_data <- pivoted_data %>%
  arrange(hour_bin) %>%  # Ensure data is sorted by time
  mutate(
    autochamber_rolling_mean = rollapply(autochamber_mean, width = 3, FUN = mean, fill = NA, align = "right"),
    fluxbot_rolling_mean = rollapply(fluxbot_mean, width = 3, FUN = mean, fill = NA, align = "right")
  ) %>%
  filter(!is.na(autochamber_rolling_mean) & !is.na(fluxbot_rolling_mean))

# Fit the linear model to the full data
model <- lm(fluxbot_rolling_mean ~ autochamber_rolling_mean, data = pivoted_data)
# Extract the slope, intercept, and R-squared
slope <- coef(model)[2]
intercept <- coef(model)[1]
r_squared <- summary(model)$r.squared

# Step 6: Calculate Lin's Concordance Correlation Coefficient (CCC)
ccc_result <- epi.ccc(pivoted_data$autochamber_rolling_mean, pivoted_data$fluxbot_rolling_mean)
ccc_value <- ccc_result$rho.c$est  # Extract the CCC estimate

# Create the equation text including R² and CCC
equation_label <- paste0("y = ", round(intercept, 2), " + ", round(slope, 2), "x,  R² = ", round(r_squared, 2), 
                         ", CCC = ", round(ccc_value, 2))

# Step 7: Plot the comparison of the two systems with rolling means
ggplot(pivoted_data, aes(x = autochamber_rolling_mean, y = fluxbot_rolling_mean)) +
  geom_point(alpha=0.3) +
  labs(x = "Autochamber Mean Flux (3-Hour Rolling)", y = "Fluxbot Mean Flux (3-Hour Rolling)") +
  theme_classic() +
  geom_smooth(method = "lm", color = "#5A82C9") +  # Linear regression line
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") + # Add a 1:1 line
  annotate("text", x = 4.5, y = Inf, label = equation_label, 
           hjust = 1, vjust = 1.5, size = 5, color = "#4C72B0") + # Add the equation, R-squared, and CCC
  xlim(1, 5) + ylim(1, 5)

# Step 8: Print the linear model and CCC results
cat("Slope:", slope, "\nR²:", r_squared, "\nLin's CCC:", round(ccc_value, 2), "\n")
