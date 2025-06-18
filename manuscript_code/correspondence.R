library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)  # For rollapply function

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


# Step 1: Bin autochamber and fluxbot data into hourly intervals for comparison
pivoted_data <- pivoted_data %>%
  mutate(hour_bin = floor_date(hour_of_obs, "hour")) %>%
  group_by(hour_bin) %>%
  summarize(
    autochamber_mean = mean(autochamber, na.rm = TRUE),
    fluxbot_mean = mean(fluxbot, na.rm = TRUE)
  ) %>%
  filter(!is.na(autochamber_mean) & !is.na(fluxbot_mean))  # Ensure both are non-NA

# Step 2: Plot the comparison of the two systems
ggplot(pivoted_data, aes(x = autochamber_mean, y = fluxbot_mean)) +
  geom_point() +
  labs(x = "Autochamber Mean Flux (Hourly)", y = "Fluxbot Mean Flux (Hourly)")+
  theme_classic() +
  geom_smooth(method = "lm", color="#4C72B0") +  # Linear regression line
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") 



# Step 1: Fit the linear regression model
model <- lm(fluxbot_mean ~ autochamber_mean, data = pivoted_data)

# Step 2: Get the summary of the linear model
summary(model)

# Step 3: Test if the slope is significantly different from 1
# Extract the estimated slope and standard error
slope <- coef(model)["autochamber_mean"]
se_slope <- summary(model)$coefficients["autochamber_mean", "Std. Error"]

# Perform a t-test to see if the slope is different from 1
t_value <- (slope - 1) / se_slope
df <- summary(model)$df[2]  # Degrees of freedom
p_value <- 2 * pt(abs(t_value), df = df, lower.tail = FALSE)

# Print the results
cat("Slope:", slope, "\nStandard Error:", se_slope, "\nT-value:", t_value, "\nP-value:", p_value)

