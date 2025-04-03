library(dplyr)

# Function to fit the model and extract Q10
fit_nls_and_calculate_Q10 <- function(data) {
  tryCatch({
    nls_model <- nls(fluxL_umolm2sec ~ a * exp(b * soiltemp_10cm_HF), 
                     data = data, 
                     start = list(a = 1, b = 0.1))
    b_estimate <- coef(nls_model)["b"]
    Q10 <- exp(10 * b_estimate)
    return(Q10)
  }, error = function(e) {
    return(NA)  # Return NA if the model fails to converge
  })
}



# Apply the function across all combinations of stand and method
Q10_results <- merged_data_with_met_filter %>%
  group_by(method) %>%
  summarize(Q10 = fit_nls_and_calculate_Q10(cur_data()), .groups = 'drop')

# Display the results
print(Q10_results)
