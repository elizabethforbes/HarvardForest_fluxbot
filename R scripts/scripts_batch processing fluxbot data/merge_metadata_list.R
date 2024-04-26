merge_by_column <- function(df1, df2, col) {
  # Check for duplicate values in the common column
  if (any(duplicated(df1[[col]])) || any(duplicated(df2[[col]]))) {
    warning("Duplicate values detected in the common column. Duplicates will be removed before merging.")
  }
  
  # Remove duplicates if present
  df1 <- df1[!duplicated(df1[[col]]), ]
  df2 <- df2[!duplicated(df2[[col]]), ]
  
  # Merge data frames by the common column
  merged_df <- merge(df1, df2, by = col, all = TRUE)
  return(merged_df)
}

# list of dfs, that uses previous function:
merge_lists_by_column <- function(list_a, list_b, list_c, list_d, col) {
  merged_results <- list() # list to store final merged data frames
  
  # Track the shared names of the sublists
  sublist_names <- names(list_a)
  
  # Iterate over each sublist name
  for (name in sublist_names) {
    # Extract corresponding lists from each input list
    sublist_a <- list_a[[name]]
    sublist_b <- list_b[[name]]
    sublist_c <- list_c[[name]]
    sublist_d <- list_d[[name]]
    
    # Merge data frames from different sublists by the common column
    merged_df <- sublist_a # Initialize merged_df with sublist_a
    if (!is.null(sublist_b)) merged_df <- merge_by_column(merged_df, sublist_b, col)
    if (!is.null(sublist_c)) merged_df <- merge_by_column(merged_df, sublist_c, col)
    if (!is.null(sublist_d)) merged_df <- merge_by_column(merged_df, sublist_d, col)
    
    # Name the merged list by the shared name of each sublist
    merged_results[[name]] <- merged_df
  }
  
  return(merged_results)
}



