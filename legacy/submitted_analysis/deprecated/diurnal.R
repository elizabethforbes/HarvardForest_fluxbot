# Step 1: Extract the smoothed values using the `predict` method after fitting the smoothing model
library(broom)  # For tidying model outputs

# Step 1: Fit a smooth spline and extract fitted values using predict
smooth_data <- merged_data_with_met_filter %>%
  mutate(hour = hour(ymd_hms(hour_of_obs))) %>%
  group_by(method, hour) %>%
  summarise(mean_flux = mean(fluxL_umolm2sec, na.rm = TRUE), .groups = 'drop') %>%
  group_by(method) %>%
  do({
    # Fit the smooth spline for each method
    fit <- smooth.spline(.$hour, .$mean_flux, spar = 0.7)
    
    # Predict the smoothed values
    predictions <- predict(fit, .$hour)$y
    
    # Return a dataframe with the hour and smoothed values
    data.frame(hour = .$hour, smoothed_flux = predictions)
  }) %>%
  ungroup()

# Step 2: Now you can calculate the amplitude or further use the smoothed values as needed
# Example: Calculate amplitude for each method
amplitude_data <- smooth_data %>%
  group_by(method) %>%
  summarise(
    max_smooth = max(smoothed_flux, na.rm = TRUE),
    min_smooth = min(smoothed_flux, na.rm = TRUE),
    amplitude = max_smooth - min_smooth  # Amplitude per method
  )

# Step 3: Plot the original figure with the smooth line
merged_data_with_met_filter %>%
  mutate(hour = hour(ymd_hms(hour_of_obs))) %>%
  ggplot(aes(x = hour, y = fluxL_umolm2sec)) +
  stat_summary(fun = mean, geom = "point") +          # Line for mean
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +  # Error bars for SE
  geom_line(data = smooth_data, aes(x = hour, y = smoothed_flux, color = method), size = 1) +
  labs(x = "Hour of the day", 
       y = expression(paste("Flux estimate, ", mu, "mol m"^-2, "s"^-1))) +
  facet_wrap(~method, nrow=2) +
  theme_classic()





# Step 1: Calculate the mean flux for each method separately
mean_flux_per_method <- merged_data_with_met_filter %>%
  group_by(method) %>%
  summarise(mean_flux = mean(fluxL_umolm2sec, na.rm = TRUE))

# Step 2: Plot the smooth line, method-specific mean line, and amplitude
merged_data_with_met_filter %>%
  mutate(hour = hour(ymd_hms(hour_of_obs))) %>%
  ggplot(aes(x = hour, y = fluxL_umolm2sec, color=method)) +
  
  
  # Add a horizontal mean line specific to each method using geom_hline and data from `mean_flux_per_method`
  geom_hline(data = mean_flux_per_method, aes(yintercept = mean_flux), 
             linetype = "dashed", color = "black", size = 1) +
  
  # Smooth line with confidence interval (amplitude) per method
  geom_smooth(method = "loess", se = TRUE) +
  
  # Points and error bars for mean flux at each hour
  stat_summary(fun = mean, geom = "point") +          # Points for mean
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, color = "black") +  # Error bars for SE

  
  # Labels and facets
  labs(x = "Hour of Day", 
       y = expression(paste(CO[2], " Flux, ", mu, "mol ", m^-2, s^-1)))+
       facet_wrap(~method, nrow = 2) +
  theme_classic() +
  scale_fill_manual(values = c("#4C9F70", "#B0B0B0"))+
  scale_color_manual(values = c("#4C9F70", "#B0B0B0"))+
  theme(legend.position = "none")


# Optional: Print the mean flux per method for reference
print(mean_flux_per_method)




# Step 1: Calculate mean and standard error for each hour and method
summary_table <- merged_data_with_met_filter %>%
  mutate(hour = hour(ymd_hms(hour_of_obs))) %>%
  group_by(method, hour) %>%
  summarise(
    mean_flux = mean(fluxL_umolm2sec, na.rm = TRUE),
    se_flux = sd(fluxL_umolm2sec, na.rm = TRUE) / sqrt(n()),  # Standard error calculation
    .groups = 'drop'
  )

# Display the table
print(summary_table)