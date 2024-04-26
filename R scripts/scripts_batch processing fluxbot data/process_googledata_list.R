# each fluxbot sensor will return a df with two columns: one a 2min interval of timestamp and the 
# other 2min of data inside of square brackets per cell.

# this function processes a list of dataframes and transposes the within-cell
# data into long-form. It also converts the UNIX timestamps to POSIXct-form
# timestamps for easier analysis.

library(dplyr)
library(tidyr)

process_data <- function(df_list, col1, col2) {
  longdat_list <- list()  # Initialize the list
  
  for (i in seq_along(df_list)) {
    df <- df_list[[i]]
    
    # Check if col1 and col2 exist in the data frame
    if (!(col1 %in% names(df) && col2 %in% names(df))) {
      warning("Column names not found in the data frame.")
      next
    }
    
    # Extract values from lists in col1 and col2
    df <- df %>%
      mutate(UNIX = gsub("\\[|\\]", "", df[[col1]]),
             co2 = gsub("\\[|\\]", "", df[[col2]])) %>%
      separate_rows(UNIX, co2, sep = ",") %>%
      select(UNIX, co2)
    
    # Store the processed dataframe in the list with the name of the input df
    longdat_list[[paste0("rawdata_", names(df_list)[i])]] <- df
  }
  
  # Return the results in a list of data frames
  return(longdat_list)
}

# Example usage:
# processed_data <- process_data(df_list, col1 = "device timestamps", col2 = "co2")







