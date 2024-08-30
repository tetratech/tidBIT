
# fun_$_time.R - helper functions to supplement commonly used lubridate functions

#' Check and Validate MM-DD Date Format
#'
#' This function checks if a given date string is in the "MM-DD" format and
#' stops execution with an error message if the format is invalid.
#'
#' The function first validates whether the date string matches the "MM-DD"
#' format using a regular expression. It then checks if the date can be
#' successfully parsed with a dummy year ("2000") using the `ymd` function from
#' the `lubridate` package. If the date is invalid, an error message is displayed.
#'
#' @param start_MM_DD A character string representing a date in "MM-DD" format.
#'
#' @return The function returns `NULL` invisibly if the date format is valid.
#' It stops execution with an error message if the date format is invalid.
#'
#' @importFrom lubridate ymd
#'
#' @examples
#' check_mm_dd_format("12-31")  # Valid date format
#' # check_mm_dd_format("02-30")  # Invalid date format
#' # check_mm_dd_format("2020-12-31")  # Invalid date format
#'
#' @seealso \code{\link{date_time_to_years_decimal}},
#'   \code{\link{date_time_to_hours_decimal}},
#'   \code{\link{leap_yday}}
#'
#' @export
#' @keywords internal
check_mm_dd_format <- function(start_MM_DD) {
  if (!grepl("^\\d{2}-\\d{2}$", start_MM_DD) || is.na(ymd(paste("2000", start_MM_DD, sep = "-")))) {
    stop(paste("Invalid 'MM-DD' date format:", start_MM_DD, "- Please provide a date in 'MM-DD' format."))
  }
}


#' Compute decimal date
#'
#' This function computes the decimal representation of a date-time object.
#' The decimal date is calculated as the year plus the day of the year divided by 366.
#' This function can handle both `POSIXct`/`POSIXlt` and `Date` types.
#'
#' @param date_chk A `POSIXct`, `POSIXlt`, or `Date` object representing the date-time.
#'
#' @return A numeric value representing the decimal date.
#'
#' @importFrom lubridate year yday date
#'
#' @examples
#' # For POSIXct date-time
#' date_chk <- lubridate::ymd_hms("2019-07-30 11:15:30", "2020-07-30 11:15:30", "2021-07-30 11:15:30")
#' date_time_to_years_decimal(date_chk)
#' date_time_to_years_decimal(date_chk, start_MM_DD = "07-01")
#' date_time_to_years_decimal(date_chk, start_MM_DD = "10-01")
#'
#' # For Date
#' date_chk <- lubridate::ymd("2019-07-30", "2020-07-30", "2021-07-30")
#' date_time_to_years_decimal(date_chk)
#' date_time_to_years_decimal(date_chk, start_MM_DD = "07-01")
#' date_time_to_years_decimal(date_chk, start_MM_DD = "10-01")
#'
#' @seealso \code{\link{date_time_to_hours_decimal}},
#'   \code{\link{check_mm_dd_format}},
#'   \code{\link{leap_yday}}
#'
#' @export
#' @keywords internal
date_time_to_years_decimal <- function(date_chk, start_MM_DD = "01-01") {

  check_mm_dd_format(start_MM_DD) # Validate start_MM_DD format

  if (inherits(date_chk, "Date") || inherits(date_chk, "POSIXt")) {
    year_dec <- year(date_chk) + leap_yday(date_chk, start_MM_DD) / 366
  } else {
    stop("date_time_to_years_decimal function requires a Date field")
  }

  year_adj <- date(date_chk) < ymd(paste(year(date_chk), start_MM_DD, sep = "-"))

  year_dec <- year_dec - as.numeric(year_adj)

  return(year_dec)

}

#' Compute decimal time
#'
#' This function computes the decimal representation of the time of day.
#' The decimal time is calculated as the hour plus the minute divided by 60 and the second divided by 3600.
#' This function requires the input to be of type `POSIXct` or `POSIXlt`.
#'
#' @param date_chk A `POSIXct` or `POSIXlt` object representing the date-time.
#'
#' @return A numeric value representing the decimal time.
#'
#' @importFrom lubridate hour minute second
#'
#' @examples
#' date_chk <- lubridate::ymd_hms("2019-07-30 11:15:00", "2020-07-30 11:15:00", "2021-07-30 11:15:00")
#' date_time_to_hours_decimal(date_chk)
#'
#' @seealso \code{\link{date_time_to_years_decimal}},
#'   \code{\link{check_mm_dd_format}},
#'   \code{\link{leap_yday}}
#'
#' @export
#' @keywords internal
date_time_to_hours_decimal <- function(date_chk) {

  if (inherits(date_chk, c("POSIXct", "POSIXlt"))) {
    hour(date_chk) + minute(date_chk) / 60 + second(date_chk) / 3600
  } else {
    stop("date_time_to_hours_decimal function requires a POSIXct or POSIXlt datetime field")
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
#' @importFrom lubridate ymd date
#' @importFrom dplyr tibble left_join mutate select slice row_number n
#' @importFrom dplyr %>%
#'
#' @seealso \code{\link{date_time_to_years_decimal}},
#'   \code{\link{date_time_to_hours_decimal}},
#'   \code{\link{check_mm_dd_format}}
#'
#' @export
#'
leap_yday <- function(date_chk, start_MM_DD = "01-01") {

  check_mm_dd_format(start_MM_DD) # Validate start_MM_DD format

  # Create a sequence of dates from 1/1/2000 to 12/31/2000
  date_sequence <- tibble(base_date = seq(from = date("2000-01-01"), to = date("2000-12-31"), by = "day")) %>%
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
