
# fun_$_predictions

#' Generate Date-Time Sequence
#'
#' This function generates a sequence of date-time values based on the specified start and stop times,
#' time step, and time units.
#'
#' @param start_date_time POSIXct compatible string or object specifying the start of the date-time sequence.
#' @param stop_date_time POSIXct compatible string or object specifying the end of the date-time sequence.
#' @param time_step Numeric value indicating the step size between date-time values. Default is 1.
#' @param time_units Character string specifying the units for the time step. Options include "days", "hours", or "seconds". Default is "days".
#' @param tz Character string specifying the time zone. Default is "EST".
#'
#' @return A vector of date-time values in POSIXct format.
#' @details The function ensures that both the start and stop date-times are in POSIXct format. It then generates
#' a sequence of date-time values according to the specified time step and units.
#'
#' @examples
#' # Generate date-time sequence every 2 days
#' generate_date_time("2024-01-01 00:00", "2024-01-10 00:00", time_step = 2, time_units = "days", tz = "EST")
#'
#' # Generate date-time sequence every 3 hours
#' generate_date_time("2024-01-01 00:00", "2024-01-02 00:00", time_step = 3, time_units = "hours", tz = "EST")
#'
#' @export
generate_date_time <- function(start_date_time,
                               stop_date_time,
                               time_step = 1,
                               time_units = "days",
                               tz = "EST") {

  # Ensure the start and stop date-times are in POSIXct format with the specified timezone
  start_date <- as.POSIXct(start_date_time, tz = tz)
  stop_date <- as.POSIXct(stop_date_time, tz = tz)

  # QC: Check that stop_date_time is later than start_date_time
  if (stop_date <= start_date) {
    stop("generate_date_time > 'stop_date_time' must be later than 'start_date_time'")
  }

  # QC: Check that time_units is valid
  valid_units <- c("days", "day", "hours", "hour", "seconds", "second")
  if (!(time_units %in% valid_units)) {
    stop("generate_date_time > 'time_units' must be one of 'days', 'hours', or 'seconds'")
  }

  # QC: Check that time_step is positive
  if (time_step <= 0) {
    stop("generate_date_time > 'time_step' must be a positive number")
  }


  # Convert time step to seconds based on units
  if (time_units %in% c("day", "days")) {
    time_step <- time_step * 86400 # Convert days to seconds
  } else if (time_units %in% c("hour", "hours")) {
    time_step <- time_step * 3600 # Convert hours to seconds
  } else if (time_units %in% c("second", "seconds")) {
    time_step <- time_step # Keep in seconds
  } else {
    stop("generate_date_time > Unrecognized time_units")
  }

  # Generate the sequence of date-time values
  date_time <- seq(from = start_date, to = stop_date, by = time_step)

  return(date_time)
}

