# Build function for data conversion from raw form to tidy form: each sensor 
# will return a df with two columns, one a 1min interval of timestamp and the 
# other 1min of data inside of square brackets per cell. However, this function 
# will work for whatever time interval you're working with as it just 
# iteratively adds one second to each unique timestamp until it reaches 
# a new one.

# name function:
clean_raw_data <- function(df, var){
  df %>% 
    separate_longer_delim({{var}}, delim = "[") %>% 
    separate_longer_delim({{var}}, delim = "]") %>% 
    separate_longer_delim({{var}}, delim = ",") %>% 
    filter({{var}} != "") %>% 
    group_by(Timestamp) %>% 
    mutate(Timestamp_new = make.index.unique(Timestamp, 1)) # add 1s to each row (change interval for different sampling timelines)
}
