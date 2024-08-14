


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
#' @title Compute Five-Number Summary as a Character String
#'
#' @description This function computes the five-number summary of a numeric vector or a single-column data frame and returns it as a character string. The summary includes the minimum, first quartile, median, third quartile, and maximum.
#'
#' @param x A numeric vector or a single-column data frame for which the five-number summary is to be computed.
#' @param sig.fig An integer specifying the number of significant figures to use in the summary. Default is 4.
#'
#' @return A character string containing the five-number summary, with values separated by commas.
#'
#' @examples
#' # Example usage
#' data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
#' fivenum_char(data)
#'
#' # Example usage with a single-column data frame
#' df <- data.frame(data)
#' fivenum_char(df)
#'
#' @export
#' @keywords internal
#'
#' @seealso \code{\link{fivenum_charp}}, \code{\link[stats]{fivenum}}, \code{\link[base]{signif}}
#'
fivenum_char <- function(x, sig.fig = 4) {
  # Handle single-column data frame by extracting the column as a vector
  if (is.data.frame(x) && ncol(x) == 1) {
    x <- x[[1]]
  }

  # Compute and return the five-number summary
  return(paste(signif(fivenum(x), sig.fig), collapse = ", "))
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
#' @title Compute Five-Number Summary with Observation Count
#'
#' @description This function computes the five-number summary of a numeric vector or a single-column data frame and appends the count of non-`NA` observations. The result is returned as a formatted character string, with an option to include or exclude parentheses around the output.
#'
#' @param x A numeric vector or a single-column data frame for which the five-number summary and observation count are to be computed.
#' @param parens_inc A logical value indicating whether to include parentheses around the output. Default is `FALSE`.
#' @param sig.fig An integer specifying the number of significant figures to use in the five-number summary. Default is 4.
#'
#' @return A character string in the format "Obs.: n | 5-Num Summ.: min, Q1, median, Q3, max", with optional parentheses around the string if `parens_inc` is set to `TRUE`.
#'
#' @examples
#' # Example usage
#' data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, NA)
#' fivenum_charp(data)
#' fivenum_charp(data, TRUE)
#'
#' # Example usage with a single-column data frame
#' df <- data.frame(data)
#' fivenum_charp(df)
#'
#' @export
#' @keywords internal
#'
#' @seealso \code{\link{fivenum_char}}, \code{\link[stats]{fivenum}}, \code{\link[base]{signif}}
#'
fivenum_charp <- function(x, parens_inc = FALSE, sig.fig = 4) {
  # Handle single-column data frame by extracting the column as a vector
  if (is.data.frame(x) && ncol(x) == 1) {
    x <- x[[1]]
  }

  # Create the result string with observation count and five-number summary
  result <- paste0("Obs.: ", sum(!is.na(x)), " | 5-Num Summ.: ", fivenum_char(x, sig.fig))

  # Optionally include parentheses around the result
  if (parens_inc) {
    result <- paste0("(", result, ")")
  }

  return(result)
}


