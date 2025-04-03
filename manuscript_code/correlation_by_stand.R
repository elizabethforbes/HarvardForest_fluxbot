library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)  # For rollapply function
library(lubridate)

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

# Step 3: Pivot data so that we have autochamber and fluxbot as columns, grouped by stand
pivoted_data <- mean_data %>%
  pivot_wider(names_from = method, values_from = mean_flux)

pivoted_data <- pivoted_data %>%
  filter(autochamber > 1 & (!is.na(fluxbot) | !is.na(autochamber)))

# Step 4: Bin autochamber and fluxbot data into hourly intervals for comparison, grouped by stand
pivoted_data <- pivoted_data %>%
  mutate(hour_bin = floor_date(hour_of_obs, "hour")) %>%
  group_by(hour_bin, stand) %>%
  summarize(
    autochamber_mean = mean(autochamber, na.rm = TRUE),
    fluxbot_mean = mean(fluxbot, na.rm = TRUE)
  ) %>%
  filter(!is.na(autochamber_mean) & !is.na(fluxbot_mean))  # Ensure both are non-NA

# Step 5: Calculate 3-hour rolling means for both autochamber and fluxbot, grouped by stand
pivoted_data <- pivoted_data %>%
  arrange(hour_bin) %>%
  group_by(stand) %>%
  mutate(
    autochamber_rolling_mean = rollapply(autochamber_mean, width = 3, FUN = mean, fill = NA, align = "right"),
    fluxbot_rolling_mean = rollapply(fluxbot_mean, width = 3, FUN = mean, fill = NA, align = "right")
  ) %>%
  filter(!is.na(autochamber_rolling_mean) & !is.na(fluxbot_rolling_mean))

# Step 6: Fit linear models for each stand and perform t-tests to check if the slope is different from 1
model_results <- pivoted_data %>%
  group_by(stand) %>%
  do({
    model <- lm(fluxbot_rolling_mean ~ autochamber_rolling_mean, data = .)
    slope <- coef(model)[2]
    intercept <- coef(model)[1]
    r_squared <- summary(model)$r.squared
    
    # Test if the slope is different from 1
    se_slope <- summary(model)$coefficients["autochamber_rolling_mean", "Std. Error"]
    t_value <- (slope - 1) / se_slope
    df <- summary(model)$df[2]
    p_value <- 2 * pt(abs(t_value), df = df, lower.tail = FALSE)
    
    data.frame(
      stand = unique(.$stand),
      intercept = intercept,
      slope = slope,
      r_squared = r_squared,
      t_value = t_value,
      p_value = p_value
    )
  })

# View the model results
print(model_results)

# Step 7: Plot the comparison of the two systems with rolling means for each stand
ggplot(pivoted_data, aes(x = autochamber_rolling_mean, y = fluxbot_rolling_mean)) +
  geom_point() +
  labs(x = "Autochamber Mean Flux (3-Hour Rolling)", y = "Fluxbot Mean Flux (3-Hour Rolling)",
       title = "Autochamber vs Fluxbot Mean Flux by Stand") +
  theme_classic() +
  geom_smooth(method = "lm") +  # Linear regression line
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")  # Add a 1:1 line
  #facet_wrap(~stand, scales = "free")  # Separate plots for each stand
