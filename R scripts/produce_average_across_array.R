# function to come up with a universal pressure dataset to use for estimating fluxes
# performs quality control check on each time point shared across a list of dfs with pressure data
# for those that pass the quality control, the mean is calculated and stored

merge_and_average_with_qc <- function(dataframes, qc_min, qc_max) {
  # Check if the input is a list of dataframes
  if (!is.list(dataframes)) {
    stop("Input must be a list of dataframes")
  }
  
  # Check if the list is not empty
  if (length(dataframes) == 0) {
    stop("Input list is empty")
  }
  
  # Extract unique timestamps from all dataframes
  all_timestamps <- unique(unlist(lapply(dataframes, function(df) df$Time)))
  
  # Create an empty dataframe to store the merged and averaged data
  merged_df <- data.frame(Time = all_timestamps,
                          )
  
  # Loop through each dataframe and calculate the average of data columns
  for (i in seq_along(dataframes)) {
    df <- dataframes[[i]]
    # Apply quality control checks
    qc_mask <- df[["pressure"]] >= qc_min & df[["pressure"]] <= qc_max
    
    # Calculate the average of data column
    avg_data <- aggregate(df[qc_mask, "pressure"], by = list(Time = df[qc_mask, "Time"]), FUN = mean)
    
    # Merge averaged data with existing merged dataframe
    merged_df <- merge(merged_df, avg_data, by = "Time", all.x = TRUE)
    
    # Rename the merged column
    colnames(merged_df)[ncol(merged_df)] <- paste0("avg_pressure", names(dataframes)[i])
  }
  
  # Check for timestamps with no data passing QC
  no_data_passing_qc <- setdiff(all_timestamps, merged_df$Time)
  if (length(no_data_passing_qc) > 0) {
    warning("Timestamps with no data passing quality control:", no_data_passing_qc)
  }
  
  return(merged_df)
}

