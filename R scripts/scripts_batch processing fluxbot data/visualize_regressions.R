visualize_regressions <- function(results_df, time_series_df) {
  # Plot original data
  plot(time_series_df$Time, time_series_df$co2, type = "l", 
       xlab = "Time", ylab = "CO2 Concentration (ppm)", 
       main = "CO2 Concentration Over Time")
  
  # Plot linear regression lines
  lines(results_df$start_timestamp, 
        predict(lm(linear_beta ~ as.numeric(difftime(start_timestamp, 
                                                     min(start_timestamp), 
                                                     units = "secs")), 
                   data = results_df)), col = "red")
  
  # Plot quadratic regression lines
  lines(results_df$start_timestamp, 
        predict(lm(quadratic_beta ~ as.numeric(difftime(start_timestamp, 
                                                        min(start_timestamp), 
                                                        units = "secs")) +
                     I(as.numeric(difftime(start_timestamp, 
                                           min(start_timestamp), 
                                           units = "secs"))^2), 
                   data = results_df)), col = "blue")
  
  # Add legend
  legend("topright", legend = c("Original Data", "Linear Regression", "Quadratic Regression"),
         col = c("black", "red", "blue"), lty = 1)
}
