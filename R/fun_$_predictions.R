
# fun_$_predictions

# ----< Function: generate_date_time >---
generate_date_time <- function(start_date_time
                               , stop_date_time
                               , time_step = 1
                               , time_units = "days"
                               , tz = "EST") {

  # Ensure the start and stop date_times are in POSIXct format with the specified timezone
  start_date <- as.POSIXct(start_date_time, tz = tz)
  stop_date <- as.POSIXct(stop_date_time, tz = tz)

  # Generate the sequence of dates based on the specified units
  if (time_units %in% c("day", "days")) {
    time_step <- time_step * 86400 # Convert days to seconds
  } else if (time_units %in% c("hour", "hours")) {
    time_step <- time_step * 3600 # Convert hours to seconds
  } else if (time_units %in% c("second", "seconds")) {
    time_step <- time_step
  } else {
    stop("function generate_date_time > Unrecognized time_units")
  }

  date_time <- seq(from = start_date, to = stop_date, by = time_step)

  return(date_time)
}
