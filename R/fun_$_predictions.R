
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
generate_date_time_seq <- function(start_date_time,
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




#' Replicate Rows by Sequence
#'
#' This function replicates rows of a data frame based on a sequence generated
#' from a specified column. The function takes a data frame and for each row,
#' generates a sequence of values and replicates the row accordingly.
#'
#' @param data A data frame or tibble containing the data to be replicated.
#' @param start A numeric value specifying the starting point for the sequence. Default is 1.
#' @param step A numeric value specifying the increment for the sequence. Default is 1.
#' @param replicate_col A character string specifying the name of the column
#'   used to determine the number of replications (sequence end). Default is
#'   "depth_b".
#' @param output_col A character string specifying the name of the column where
#'   the generated sequence will be stored. Default is "depth".
#'
#' @return A tibble containing the replicated rows, with a new column for the
#'   sequence values.
#' @details For each row in the input data, the function generates a sequence
#'   starting from `start` to the value found in `replicate_col`, with
#'   increments of `step`. The row is then replicated for each value in the
#'   sequence, and a new column is created to store the sequence values.
#'
#' @examples
#' # Example data
#' data <- tibble::tibble(
#'   id = 1:2,
#'   depth_b = c(2, 3)
#' )
#'
#' # Replicate rows based on the 'depth_b' column
#' replicate_rows_by_seq(data, start = 0.5, step = 0.5, replicate_col = "depth_b", output_col = "depth")
#'
#' @export
replicate_rows_by_seq <- function(data, start = 1, step = 1, replicate_col = "depth_b", output_col = "depth") {

  # Function to replicate a single row based on the replicate_col column
  replicate_row <- function(row) {

    stop_value <- row[[replicate_col]]
    seq_values <- seq(from = start, to = stop_value, by = step)

    # Replicate row element-wise
    replicated_rows <- as_tibble(lapply(row, rep, times = length(seq_values)))
    replicated_rows[[output_col]] <- seq_values  # Add sequence column

    return(replicated_rows)
  }

  # Apply the function to each row of the data frame
  replicated_list <- Map(replicate_row, split(data, seq(nrow(data))))

  # Combine the replicated rows into one tibble without row names
  replicated_data <- do.call(rbind, replicated_list)

  return(replicated_data)
}
