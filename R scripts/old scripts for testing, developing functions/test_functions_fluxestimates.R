# library(dplyr)
# library(lubridate)

extract_intervals_iterative <- function(time_series) {
  result <- data.frame()  # Initialize an empty data frame to store the results
  
  time_series %>%
    filter(minute(as.POSIXct(timestamp)) >= 56) -> result
  
  
  return(result)
  
}
##############################################################################
# library(dplyr)
# library(lubridate)
# library(broom)  # For tidy function

extract_intervals_and_regression <- function(time_series) {
  intervals_result <- data.frame()  # Initialize an empty data frame for intervals
  regression_result <- data.frame()  # Initialize an empty data frame for regression coefficients
  
  time_series %>%
    # group_by(RoundedTime = round(as.POSIXct(timestamp), "min")) %>%
    filter(minute(as.POSIXct(timestamp)) >= 56) %>%
    # ungroup() %>%
    do({
      interval_data <- .
      regression_coefficients <- lm(co2 ~ as.numeric(timestamp), data = interval_data) %>% tidy()
      
      intervals_result <<- bind_rows(intervals_result, interval_data)
      regression_result <<- bind_rows(regression_result, regression_coefficients)
    })
  
  return(list(intervals = intervals_result, regression = regression_result))
}
##############################################################################

# library(dplyr)
# library(lubridate)
# library(broom)  # For tidy function

extract_intervals_and_regression <- function(time_series) {
  intervals_result <- data.frame()  # Initialize an empty data frame for intervals
  regression_result <- list()  # Initialize a list for regression coefficients
  
  time_series <- time_series %>%
    arrange(timestamp)  # Sort data by timestamp
  
  unique_intervals <- time_series %>%
    filter(minute(as.POSIXct(timestamp)) >= 56) %>%
    select(timestamp) %>%
    unique()
  
  for (interval in unique_intervals$timestamp) {
    print(paste("Processing interval ", interval))
    interval_data <- time_series %>%
      filter(as.POSIXct(timestamp) >= interval,
             as.POSIXct(timestamp) < interval + minutes(4))
    
    if (nrow(interval_data) > 0) {
      min_timestamp <- min(interval_data$timestamp)
      
      model_data <- data.frame(
        co2 = interval_data$co2,
        timestamp_adjusted = as.numeric(interval_data$timestamp) - as.numeric(min_timestamp)
      )
      
      regression_coefficients <- lm(co2 ~ timestamp_adjusted, data = model_data) %>%
        tidy()
      
      intervals_result <- bind_rows(intervals_result, interval_data)
      regression_result[[as.character(interval)]] <- regression_coefficients
    }
  }
  
  return(list(intervals = intervals_result, regression = regression_result))
}
##############################################################################
# 
# library(dplyr)
# library(lubridate)
# library(broom)  # For tidy function

extract_intervals_and_regression <- function(time_series) {
  intervals_result <- data.frame()  # Initialize an empty data frame for intervals
  regression_result <- list()  # Initialize a list for regression coefficients
  
  unique_intervals <- time_series %>%
    filter(minute(as.POSIXct(timestamp)) >= 56) %>%
    select(timestamp) %>%
    unique()
  
  for (i in unique_intervals$timestamp) {
    # print(paste("Processing interval:", interval))
    
    interval_data <- time_series %>%
      filter(as.POSIXct(timestamp) >= i,
             as.POSIXct(timestamp) < i + minutes(4))
    
    if (nrow(interval_data) > 0) {
      min_timestamp <- min(interval_data$timestamp)
      
      model_data <- data.frame(
        co2 = interval_data$co2,
        timestamp_adjusted = as.numeric(interval_data$timestamp) - as.numeric(min_timestamp)
      )
      
      regression_coefficients <- lm(co2 ~ timestamp_adjusted, data = model_data) %>%
        tidy()
      
      intervals_result <- bind_rows(intervals_result, interval_data)
      regression_result[[as.character(interval)]] <- regression_coefficients
    }
  }
  
  return(list(intervals = intervals_result, regression = regression_result))
}




