generate_variable_interval_df <- function(start_datetime, end_datetime, interval_length, 
                                 start_minute = 0, start_second = 0, 
                                 end_minute = 0, end_second = 0, 
                                 interval_type = "half-hourly") {
  
  # Ensure start_datetime and end_datetime are in POSIXct format
  start_datetime <- as.POSIXct(start_datetime, tz = "America/New_York")
  end_datetime <- as.POSIXct(end_datetime, tz = "America/New_York")
  
  # Initialize empty vectors to store start and end timestamps
  start_timestamps <- c()
  end_timestamps <- c()
  
  # Strip seconds from the start datetime to avoid the added seconds issue
  start_datetime <- as.POSIXct(format(start_datetime, "%Y-%m-%d %H:%M:%S"), tz = "America/New_York")
  
  # Initialize the current time (starting at the first interval's start time)
  current_time <- start_datetime + as.difftime(start_minute * 60 + start_second, units = "secs")
  
  # Loop to generate intervals based on interval type
  while (current_time < end_datetime) {
    
    # Calculate the start interval
    start_interval <- current_time
    
    # Calculate the end interval by adding the specified interval length (in seconds) and any additional end minute/second
    end_interval <- start_interval + as.difftime(interval_length * 60, units = "secs")
    end_interval <- end_interval + as.difftime(end_minute * 60 + end_second, units = "secs")
    
    # Ensure that the end_interval doesn't exceed the end_datetime
    end_interval <- min(end_interval, end_datetime)
    
    # Append the start and end intervals to the respective lists
    start_timestamps <- c(start_timestamps, start_interval)
    end_timestamps <- c(end_timestamps, end_interval)
    
    # Move the current time forward for the next interval
    # If half-hourly intervals, move forward 30 minutes, respecting the start minute/second
    if (interval_type == "half-hourly") {
      current_time <- start_interval + as.difftime(30 * 60, units = "secs")  # Move 30 minutes ahead
    } else {
      current_time <- start_interval + as.difftime(60 * 60, units = "secs")  # Move 60 minutes ahead
    }
  }
  
  # Create a DataFrame with start and end timestamps in POSIXct format
  interval_df <- data.frame(
    Start = as.POSIXct(start_timestamps, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York"),
    End = as.POSIXct(end_timestamps, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York")
  )
  
  return(interval_df)
}
