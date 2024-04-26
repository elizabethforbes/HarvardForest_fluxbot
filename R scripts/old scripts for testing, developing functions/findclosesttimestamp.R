find_closest_timestamp <- function(target, timestamps) {
  closest_index <- which.min(abs(timestamps - target))
  return(timestamps[closest_index])
}