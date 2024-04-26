# library(dplyr)
# library(tidyr)
# library(lubridate)
# 
# # calls for time series df, identified start minute, and identified end minute: see example below
# extract_intervals <- function(df, start_minute, end_minute, timezone = "America/New_York") {
#   
#   # Extract start times
#   start <- df %>% 
#     filter(minute(Time) == start_minute, second(Time) == 0) %>% 
#     select(Time) %>% 
#     mutate(date = date(Time),
#            hour = hour(Time),
#            minute = minute(Time),
#            sec = second(Time))
#   
#   # Extract end times
#   end <- df %>% 
#     filter(minute(Time) == end_minute, second(Time) == 59) %>% 
#     select(Time) %>% 
#     mutate(date = date(Time),
#            hour = hour(Time),
#            minute = minute(Time),
#            sec = second(Time))
#   
#   # Combine start and end
#   startend <- rbind(start, end)
#   
#   # Remove duplicate entries
#   startend <- startend %>% 
#     select(Time, minute) %>% 
#     unique()
#   
#   # Add row numbers
#   startend$row <- seq_len(nrow(startend))
#   
#   # Pivot wider
#   startend <- pivot_wider(data = startend,
#                           names_from = minute,
#                           values_from = Time)
#   
#   # Remove row column
#   startend$row <- NULL
#   
#   # Extract and clean start and end columns
#   startend_start <- na.omit(startend[[as.character(start_minute)]])
#   startend_end <- na.omit(startend[[as.character(end_minute)]])
#   
#   # Combine start and end columns
#   startend <- cbind(startend_start, startend_end)
#   
#   # Convert to data frame
#   startend <- as_data_frame(startend) 
#   
#   # Rename columns
#   colnames(startend) <- c("Start", "End")
#   
#   # Convert to datetime with specified timezone
#   startend <- startend %>% 
#     mutate(Start = as_datetime(Start, tz = timezone),
#            End = as_datetime(End, tz = timezone))
#   
#   return(startend)
# }
# 
# # Example usage:
# # Replace 'your_data_frame' with the actual name of your data frame, and input the 'start minute' and 'end minute' for desired interval lengths
# # result <- extract_intervals(your_data_frame, 56, 59)
# 
library(dplyr)

# Assuming 'df1' and 'df2' are your data frames
# Replace 'your_dataframe1.csv' and 'your_dataframe2.csv' with your actual files or use your data frames directly
# df1 <- read.csv('your_dataframe1.csv')
# df2 <- read.csv('your_dataframe2.csv')

# Assuming 'start' and 'end' are in character format, convert them to POSIXct in df1
# df1$start <- as.POSIXct(df1$start, format = "%Y-%m-%d %H:%M:%S")
# df1$end <- as.POSIXct(df1$end, format = "%Y-%m-%d %H:%M:%S")

# Assuming 'timestamp' is in character format, convert it to POSIXct in df2
# df2$timestamp <- as.POSIXct(df2$timestamp, format = "%Y-%m-%d %H:%M:%S")

##############################################################################################################

generate_interval_df <- function(start_datetime, end_datetime, interval_length, start_minute = 0, start_second = 0, end_minute = 0, end_second = 0) {
  # Initialize empty vectors to store start and end timestamps
  start_timestamps <- c()
  end_timestamps <- c()
  
  # Loop through each hour and generate start and end timestamps
  current_hour <- trunc(start_datetime, "hour")
  while (current_hour < end_datetime) {
    start_interval <- current_hour + as.numeric(start_minute) * 60 + as.numeric(start_second)
    end_interval <- current_hour + as.numeric(interval_length) + as.numeric(end_minute) * 60 + as.numeric(end_second)
    
    # Ensure end interval does not exceed end_datetime
    end_interval <- min(end_interval, end_datetime)
    
    # Append start and end timestamps to vectors
    start_timestamps <- c(start_timestamps, start_interval)
    end_timestamps <- c(end_timestamps, end_interval)
    
    # Move to the next hour
    current_hour <- current_hour + as.difftime(1, units = "hours")
  }
  
  # Create DataFrame with start and end timestamps in POSIXct format
  interval_df <- data.frame(Start = as.POSIXct(start_timestamps, 
                                               format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York"), 
                            End = as.POSIXct(end_timestamps,
                                             format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York"))
  
  return(interval_df)
}

# Example usage:
# start_datetime <- as.POSIXct("2023-07-18 00:00:00")
# end_datetime <- as.POSIXct("2023-07-19 00:00:00")
# interval_length <- as.difftime(1, units = "hours")  # 1 hour interval
# start_minute <- 56
# start_second <- 0
# end_minute <- 59
# end_second <- 59
# interval_df <- generate_interval_df(start_datetime, end_datetime, interval_length, start_minute, start_second, end_minute, end_second)


