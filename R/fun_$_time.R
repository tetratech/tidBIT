

#' Compute decimal date
#'
#' This function computes the decimal representation of a date-time object.
#' The decimal date is calculated as the year plus the day of the year divided by 366.
#' This function can handle both `POSIXct`/`POSIXlt` and `Date` types.
#'
#' @param date_time A `POSIXct`, `POSIXlt`, or `Date` object representing the date-time.
#' @return A numeric value representing the decimal date.
#' @importFrom lubridate year yday
#' @examples
#' # For POSIXct date-time
#' date_time <- lubridate::ymd_hms("2024-07-30 11:15:30")
#' decimal_date(date_time)
#'
#' # For Date
#' date <- as.Date("2024-07-30")
#' decimal_date(date)
#'
#' @seealso \code{\link{decimal_hours}},
#'   \code{\link{leap_yday}}
#'
#' @export
#' @keywords internal
decimal_date <- function(date_time) {
  if (inherits(date_time, "Date")) {
    year(date_time) + yday(date_time) / 366
  } else {
    year(date_time) + leap_yday(date_time) / 366
  }
}

#' Compute decimal time
#'
#' This function computes the decimal representation of the time of day.
#' The decimal time is calculated as the hour plus the minute divided by 60 and the second divided by 3600.
#' This function requires the input to be of type `POSIXct` or `POSIXlt`.
#'
#' @param date_time A `POSIXct` or `POSIXlt` object representing the date-time.
#' @return A numeric value representing the decimal time.
#' @importFrom lubridate hour minute second
#' @examples
#' date_time <- lubridate::ymd_hms("2024-07-30 11:15:30")
#' decimal_hours(date_time)
#'
#' @seealso \code{\link{decimal_date}},
#'   \code{\link{leap_yday}}
#'
#' @export
#' @keywords internal
decimal_hours <- function(date_time) {
  if (inherits(date_time, c("POSIXct", "POSIXlt"))) {
    hour(date_time) + minute(date_time) / 60 + second(date_time) / 3600
  } else {
    stop("decimal_hours function requires a POSIXct or POSIXlt datetime field")
  }
}



#' @title Compute Day of Year Adjusted for Leap Year and Start Date
#'
#' @description Calculates the day of the year (DOY) for a given date, adjusting
#'  as if every year is a leap year. This adjustment ensures consistency in day
#'  numbering across years, particularly useful for analyses where maintaining a
#'  consistent temporal scale is important, such as in time series modeling or
#'  seasonal studies. The function allows adjusting for different start dates.
#'
#' @param date_chk A date or datetime object, or a vector of dates, for which the adjusted day of the
#' year is calculated.
#' @param start_MM_DD A character string specifying the start date in "MM-DD" format.
#' Default is "10-01" (water year starting Oct 1).
#'
#' @return An integer vector representing the day of the year, adjusted for leap years
#' and the specified start date.
#'
#' @examples
#' date_example <- as.Date(
#'   c("1999-01-01", "1999-01-02", "1999-02-28", NA,
#'     "1999-03-01", "1999-03-30", "1999-03-31", "1999-04-01",
#'     "1999-04-02", "1999-09-29", "1999-09-30", "1999-10-01",
#'     "1999-10-02", "1999-12-30", "1999-12-31",
#'     "2000-01-01", "2000-01-02", "2000-02-28", "2000-02-29",
#'     "2000-03-01", "2000-03-30", "2000-03-31", "2000-04-01",
#'     "2000-04-02", "2000-09-29", "2000-09-30", "2000-10-01",
#'     "2000-10-02", "2000-12-30", "2000-12-31",
#'     "2001-01-01", "2001-01-02", "2001-02-28", NA,
#'     "2001-03-01", "2001-03-30", "2001-03-31", "2001-04-01",
#'     "2001-04-02", "2001-09-29", "2001-09-30", "2001-10-01",
#'     "2001-10-02", "2001-12-30", "2001-12-31"))
#'
#' x <- leap_yday(date_example)
#' matrix(x, ncol = 3)
#'
#' x <- leap_yday(date_example, start_MM_DD = "10-01")
#' matrix(x, ncol = 3)
#'
#' x <- leap_yday(date_example, start_MM_DD = "04-01")
#' matrix(x, ncol = 3)
#'
#' @importFrom lubridate ymd
#' @importFrom dplyr tibble left_join mutate select slice row_number n
#' @importFrom magrittr %>%
#'
#' @seealso \code{\link{decimal_date}},
#'   \code{\link{decimal_hours}}
#'
#' @export
#' @keywords internal
#'
leap_yday <- function(date_chk, start_MM_DD = "01-01") {

  # Function to validate "MM-DD" format
  validate_mm_dd <- function(start_MM_DD) {
    grepl("^\\d{2}-\\d{2}$", start_MM_DD) && !is.na(ymd(paste("2000", start_MM_DD, sep = "-")))
  }

  # Function to check if the "MM-DD" format is valid and stop if not
  check_mm_dd_format <- function(start_MM_DD) {
    if (!validate_mm_dd(start_MM_DD)) {
      stop(paste("Invalid 'MM-DD' date format:", start_MM_DD, "- Please provide a date in 'MM-DD' format."))
    }
  }

  check_mm_dd_format(start_MM_DD) # Validate start_MM_DD format

  # Create a sequence of dates from 1/1/2000 to 12/31/2000
  date_sequence <- tibble(base_date = seq(from = as.Date("2000-01-01"), to = as.Date("2000-12-31"), by = "day")) %>%
    mutate(mm_dd = format(.data$base_date, "%m-%d"))

  # Find the index corresponding to start_MM_DD
  idx <- which(date_sequence$mm_dd == start_MM_DD)

  # Reorder the rows so that rows from index `idx` to the end move to the top
  if (idx != 1) {
    date_sequence <- date_sequence %>%
      slice(c(idx:n(), 1:(idx - 1)))
  }

  # Number the reordered data to create a day of year look up table
  date_sequence <- date_sequence %>%
    mutate(leap_yday = row_number())

  # Merge with dates
  data <- left_join(tibble(date_chk) %>%
                      mutate(mm_dd = format(date_chk, "%m-%d")),
                    date_sequence, by = "mm_dd")

  return(data$leap_yday)
}## FUN ~ leap_yday
