filter_iqr <- function(data, column) {
  Q1 <- quantile(data[[column]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[column]], 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  
  data %>%
    filter(data[[column]] >= lower_bound & data[[column]] <= upper_bound)
}

# Example usage:
HF_fluxestimates_filtered <- filter_iqr(HF_fluxestimates, "final_flux_umolm2sec")
