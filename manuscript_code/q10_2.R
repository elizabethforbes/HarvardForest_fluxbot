library(ggplot2)
library(dplyr)

# Function to fit the model, create predictions, and calculate SE manually
fit_nls_and_predict_with_se <- function(data) {
  tryCatch({
    # Fit the non-linear model
    nls_model <- nls(fluxL_umolm2sec ~ a * exp(b * soiltemp_10cm_HF), 
                     data = data, 
                     start = list(a = 1, b = 0.1))
    
    # Create predictions for the given sequence of soil temperatures
    predictions <- predict(nls_model, newdata = data.frame(soiltemp_10cm_HF = soiltemp_seq))
    
    # Get the covariance matrix of the parameter estimates
    cov_matrix <- vcov(nls_model)
    
    # Get the parameter estimates
    params <- coef(nls_model)
    
    # Calculate the Jacobian matrix of the predictions
    jacobian <- cbind(exp(params["b"] * soiltemp_seq), 
                      params["a"] * soiltemp_seq * exp(params["b"] * soiltemp_seq))
    
    # Calculate standard errors of the predictions using the covariance matrix
    se <- sqrt(rowSums((jacobian %*% cov_matrix) * jacobian))
    
    # Return the predictions and SE
    prediction_df <- data.frame(
      soiltemp_10cm_HF = soiltemp_seq,
      fluxL_umolm2sec = predictions,
      se = se
    )
    
    return(prediction_df)
  }, error = function(e) {
    # Return NA if the model fails
    return(data.frame(soiltemp_10cm_HF = soiltemp_seq, 
                      fluxL_umolm2sec = NA, 
                      se = NA))  
  })
}

# Function to calculate Q10 from the nls model
fit_nls_and_calculate_Q10 <- function(data) {
  tryCatch({
    # Fit the non-linear model
    nls_model <- nls(fluxL_umolm2sec ~ a * exp(b * soiltemp_10cm_HF), 
                     data = data, 
                     start = list(a = 1, b = 0.1))
    
    # Calculate Q10 as exp(10 * b)
    Q10 <- exp(10 * coef(nls_model)["b"])
    
    return(Q10)
  }, error = function(e) {
    # Return NA if the model fails
    return(NA)
  })
}

# Filter the data to remove NAs in soiltemp and fluxL_umolm2sec,
# ensure fluxL_umolm2sec > 0.5, and keep only the middle 95% of flux values per method
merged_data_with_met_filter <- merged_data_with_met %>%
  filter(
    !is.na(soiltemp_10cm_HF) & 
      !is.na(fluxL_umolm2sec) & 
      fluxL_umolm2sec > 0.5
  ) %>%
  group_by(method) %>%
  # Calculate the 2.5th and 97.5th percentiles for fluxL_umolm2sec
  mutate(
    lower_bound = quantile(fluxL_umolm2sec, 0, na.rm = TRUE),
    upper_bound = quantile(fluxL_umolm2sec, 1, na.rm = TRUE)
  ) %>%
  # Filter to keep only the flux values within the 2.5th and 97.5th percentiles
  filter(
    fluxL_umolm2sec >= lower_bound & 
      fluxL_umolm2sec <= upper_bound
  ) %>%
  # Remove the temporary percentile columns
  select(-lower_bound, -upper_bound) %>%
  ungroup()

# Generate a sequence of soil temperatures for predictions
soiltemp_seq <- seq(
  min(merged_data_with_met_filter$soiltemp_10cm_HF, na.rm = TRUE), 
  max(merged_data_with_met_filter$soiltemp_10cm_HF, na.rm = TRUE), 
  length.out = 100
)

# Apply the function across each method to get predictions with SE
predictions <- merged_data_with_met_filter %>%
  group_by(method) %>%
  do(fit_nls_and_predict_with_se(.)) %>%
  ungroup()

# Remove any rows with NA values in the predictions
predictions <- predictions %>% filter(!is.na(fluxL_umolm2sec))

# Calculate Q10 values for each method and create label expressions
Q10_results <- merged_data_with_met_filter %>%
  group_by(method) %>%
  summarize(
    Q10 = fit_nls_and_calculate_Q10(cur_data()),
    min_temp = min(soiltemp_10cm_HF, na.rm = TRUE),
    max_temp = max(soiltemp_10cm_HF, na.rm = TRUE),
    min_flux = min(fluxL_umolm2sec, na.rm = TRUE),
    max_flux = max(fluxL_umolm2sec, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    label_x = 12.5,  
    label_y = 5,  
    label_expr = paste0("Q[10] == ", round(Q10, 2))
  )

# Print Q10 values
print(Q10_results)

# Plot the fitted curves and annotate Q10 values with facet_wrap
ggplot() +
  # Add the original flux data points
  geom_point(data = merged_data_with_met_filter, 
             aes(x = soiltemp_10cm_HF, y = fluxL_umolm2sec, color=method, fill=method), 
             alpha = 0.1) +
  
  # Add the fitted lines (model predictions)
  geom_line(data = predictions, 
            aes(x = soiltemp_10cm_HF, y = fluxL_umolm2sec), 
            size = 1) +
  
  # Add SE shading
  geom_ribbon(data = predictions, 
              aes(x = soiltemp_10cm_HF, ymin = fluxL_umolm2sec - se, ymax = fluxL_umolm2sec + se), 
              alpha = 0.2) +
  
  # Add text for Q10 values with subscript
  geom_text(data = Q10_results, 
            aes(x = label_x, y = label_y, label = label_expr), 
            size = 4, color = "black", fontface = "bold", parse = TRUE) +
  
  # Facet the plot by method
  facet_wrap(~ method) +
  
  theme_classic() +
  labs(
    x = "Soil Temperature (10cm depth)", 
    y = "Flux (umol/m²/sec)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(face = "bold")  # Bold facet labels
  ) +
  scale_color_manual(values = c("#4C9F70", "#B0B0B0")) +
  scale_fill_manual(values = c("#4C9F70", "#B0B0B0"))+
  theme(legend.position = "none")+
  xlab(expression(paste(CO[2], " Flux, ", mu, "mol ", m^-2, s^-1)))+
  ylab(expression(paste("Soil Temperature(", degree, "C)")))

