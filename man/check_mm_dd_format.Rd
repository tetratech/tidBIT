% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fun_$_time.R
\name{check_mm_dd_format}
\alias{check_mm_dd_format}
\title{Check and Validate MM-DD Date Format}
\usage{
check_mm_dd_format(start_MM_DD)
}
\arguments{
\item{start_MM_DD}{A character string representing a date in "MM-DD" format.}
}
\value{
The function returns `NULL` invisibly if the date format is valid.
It stops execution with an error message if the date format is invalid.
}
\description{
This function checks if a given date string is in the "MM-DD" format and
stops execution with an error message if the format is invalid.
}
\details{
The function first validates whether the date string matches the "MM-DD"
format using a regular expression. It then checks if the date can be
successfully parsed with a dummy year ("2000") using the `ymd` function from
the `lubridate` package. If the date is invalid, an error message is displayed.
}
\examples{
check_mm_dd_format("12-31")  # Valid date format
# check_mm_dd_format("02-30")  # Invalid date format
# check_mm_dd_format("2020-12-31")  # Invalid date format

}
\seealso{
\code{\link{date_time_to_years_decimal}},
  \code{\link{date_time_to_hours_decimal}},
  \code{\link{leap_yday}}
}
\keyword{internal}
