# Build function for data conversion from raw form to tidy form: each sensor 
# will return a df with two columns, one a 2min interval of timestamp and the 
# other 2min of data inside of square brackets per cell.

# this function processes individual dataframes and transposes the within-cell
# data into long-form. It also converts the UNIX timestamps to POSIXct-form
# timestamps for easier analysis.

# it doesn't convert the data column to numeric easily for some reason, and I
# am not sure why; however, that's ok for now.
process_data <- function(df, col1, col2) {
  df %>%
    separate_rows(c(!!rlang::sym(col1), !!rlang::sym(col2))) %>%
    select({{col1}}, {{col2}}) %>%
    rename(UNIX = {{col1}}) %>%
    mutate(
      UNIX = as.numeric(UNIX),
      timestamp = as.POSIXct(UNIX, origin = "1970-01-01"),
      {{col2}} := as.numeric({{col2}})
  )
}

# # Example usage
# result_df <- process_data(co2_fluxbot_20, "device timestamps", "co2")
# class(result_df$timestamp)
# class(result_df$co2)
