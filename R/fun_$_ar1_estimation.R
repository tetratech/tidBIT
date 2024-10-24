
# fun_$_correlation.R - functions to compute AR(1) coefficients and make corresponding plots

#' Estimate AR(1) Model Parameter
#'
#' An AR(1) model is fit using up to three different approaches sequentially. If
#' an approach fails (produces an error or warning), the next approach is
#' attempted. Approach 1 uses the `arima` function with default parameters and
#' is computationally faster than Approach 2 but sometimes fails to converge.
#' Approach 2 uses the `arima` function with alternative method, optimization,
#' and iteration settings and based on testing found to be
#' less prone to convergence issues. Approach 3 uses the method of moments but
#' is considered less accurate than the other two approaches but does not rely
#' on computing initial values. See \code{\link{ar1_default}}, \code{\link{ar1_alter}},
#' and \code{\link{ar1_moment}} for further details.
#'
#' @param y A numeric vector representing the time series data.
#' @return A list containing the following elements:
#' \describe{
#'   \item{phi_1}{The estimated AR(1) parameter.}
#'   \item{standard_error}{The standard error of the estimated AR(1) parameter.}
#'   \item{white_noise_sd}{The standard deviation of the white noise.}
#'   \item{t_value}{The t-value of the estimated AR(1) parameter.}
#'   \item{p_value}{The p-value of the estimated AR(1) parameter.}
#'   \item{approach}{The method used to estimate the AR(1) parameter ("default", "alternative", "moments", or "No method worked").}
#'   \item{arima_results}{Results from arima function call when either the "default" or "alternative" method were.}
#'   \item{message}{Any warning or error messages encountered during the estimation process.}
#' }
#' @examples
#' set.seed(123)
#' y    <- arima.sim(n = 40, list(ar = 0.40))
#' y[8] <- NA
#' plot_acf_pacf(y)
#' estimate_ar1(y)
#'
#'
#' @importFrom stats arima pt sd
#'
#' @seealso \code{\link{plot_acf_pacf}},
#'   \code{\link{ar1_default}},
#'   \code{\link{ar1_alter}},
#'   \code{\link{ar1_moment}}
#'
#' @export
#'
estimate_ar1 <- function(y) {

  # Function to check if a value is neither NA nor NaN
  is_not_na_and_not_nan <- function(x) {
    if (!is.list(x)) {
      !is.na(x) & !is.nan(x)
    } else if (is.list(x)) {
      TRUE
    }
  }

  # Initialize result list with NA values and empty message
  result <- list(
    phi_1 = NA,
    standard_error = NA,
    white_noise_sd = NA,
    t_value = NA,
    p_value = NA,
    approach = NA,
    arima_results = "",
    message = ""
  )

  # Try default ARIMA method
  {
    try_default <- tryCatch({
      result <- ar1_default(y)
      result$message = ""
      result
    }, warning = function(w) {
      # Capture warning message
      result$approach <- "default"
      result$message  <- paste("ar1_default msg:", conditionMessage(w))
      result
    }, error = function(e) {
      # Capture error message
      result$approach <- "default"
      result$message  <- paste("ar1_default msg:", conditionMessage(e))
      result
    })
  }

  # If the default method succeeds without producing NA or NaN, return the result
  if (all(sapply(try_default, is_not_na_and_not_nan))) {
    return(try_default)
  }

  # Store the message from the default method
  message <- try_default$message

  # Try alternative ARIMA method
  {
    try_alternative <- tryCatch({
      result <- ar1_alter(y)
      result$message <- message
      result
    }, warning = function(w) {
      # Append warning message
      result$approach <- "alternative"
      result$message <- paste(message, "|", "ar1_alter msg:", conditionMessage(w))
      result
    }, error = function(e) {
      # Append error message
      result$approach <- "alternative"
      result$message <- paste(message, "|", "ar1_alter msg:", conditionMessage(e))
      result
    })
  }

  # If the alternative method succeeds without producing NA or NaN, return the result
  if (all(sapply(try_alternative, is_not_na_and_not_nan))) {
    return(try_alternative)
  }

  # Store the message from the alternative method
  message <- try_alternative$message

  # Try method of moments
  {
    try_moments <- tryCatch({
      result <- ar1_moment(y)
      result$message <- message
      result
    }, warning = function(w) {
      # Append warning message
      result$approach <- "moments"
      result$message <- paste(message, "|", "ar1_moment msg:", conditionMessage(w))
      result
    }, error = function(e) {
      # Append error message
      result$approach <- "moments"
      result$message <- paste(message, "|", "ar1_moment msg:", conditionMessage(e))
      result
    })
  }

  # If the method of moments succeeds without producing NA or NaN, return the result
  if (all(sapply(try_moments, is_not_na_and_not_nan))) {
    return(try_moments)
  }

  # Store the message from the method of moments
  message <- try_moments$message

  # All methods failed, update message and return result
  try_moments$approach <- "No method worked"

  return(try_moments)
}

#' Default ARIMA Application for AR(1) Model
#'
#' Fit an AR(1) model to the time series data using the default settings of the
#' `arima` function. The `arima` function call uses order = c(1, 0, 0)
#' indicating an AR(1) model with no differencing and no moving average
#' component. Default methods "CSS-ML" (Conditional Sum of Squares followed by
#' Maximum Likelihood), default optimization "BFGS"
#' (Broyden-Fletcher-Goldfarb-Shanno algorithm), and default iterations are
#' used.
#'
#' @param y A numeric vector representing the time series data.
#' @return A list containing the estimated AR(1) parameter (phi_1), standard
#'   error, white noise standard deviation, t-value, p-value, and the approach
#'   used.
#' @examples
#' set.seed(123)
#' y    <- arima.sim(n = 40, list(ar = 0.40))
#' y[8] <- NA
#' ar1_default(y)
#'
#' @importFrom stats arima pt sd
#'
#' @seealso \code{\link{estimate_ar1}},
#'   \code{\link{plot_acf_pacf}},
#'   \code{\link{ar1_alter}},
#'   \code{\link{ar1_moment}}
#'
#' @export
#' @keywords internal
#'
ar1_default <- function(y) {

  arima_results  <- arima(y, order = c(1, 0, 0))

  phi_1          <- arima_results$coef[1]
  standard_error <- sqrt(arima_results$var.coef[1, 1])
  white_noise_sd <- sqrt(arima_results$sigma2)
  t_value        <- phi_1 / standard_error
  valid_indices  <- which(!is.na(y[-length(y)]) & !is.na(y[-1]))
  n              <- length(valid_indices)
  p_value        <- 2 * (1 - pt(abs(t_value), df = n - 2))

  return(list(
    phi_1 = unname(phi_1),
    standard_error = unname(standard_error),
    white_noise_sd = unname(white_noise_sd),
    t_value = unname(t_value),
    p_value = unname(p_value),
    approach = "default",
    arima_results = arima_results
  ))
}


#' Alternative ARIMA Application for AR(1) Model
#'
#' Fit an AR(1) model to the time series data using the `arima` function with
#' alternative parameters. The `arima` function call uses order = c(1, 0, 0)
#' indicating an AR(1) model with no differencing and no moving average
#' component. An alternative method “ML" (Maximum Likelihood) and alternative
#' optimization "CG" (Conjugate Gradient algorithm) arguments are used. The
#' maximum number of iterations is increased to 200 (i.e., `optim.control =
#' list(maxit = 200)`).
#'
#' @param y A numeric vector representing the time series data.
#' @return A list containing the estimated AR(1) parameter (phi_1), standard
#'   error, white noise standard deviation, t-value, p-value, and the approach
#'   used.
#' @examples
#' set.seed(123)
#' y    <- arima.sim(n = 40, list(ar = 0.40))
#' y[8] <- NA
#' ar1_alter(y)
#'
#' @importFrom stats arima pt sd
#'
#' @seealso \code{\link{estimate_ar1}},
#'   \code{\link{plot_acf_pacf}},
#'   \code{\link{ar1_default}},
#'   \code{\link{ar1_moment}}
#'
#' @export
#' @keywords internal
#'
ar1_alter <- function(y) {
  arima_results <- arima(y, order = c(1, 0, 0),
                         method = "ML",
                         optim.method = "CG",
                         optim.control = list(maxit = 200))

  phi_1          <- arima_results$coef[1]
  standard_error <- sqrt(arima_results$var.coef[1, 1])
  white_noise_sd <- sqrt(arima_results$sigma2)
  t_value        <- phi_1 / standard_error
  valid_indices  <- which(!is.na(y[-length(y)]) & !is.na(y[-1]))
  n              <- length(valid_indices)
  p_value        <- 2 * (1 - pt(abs(t_value), df = n - 2))

  return(list(
    phi_1 = unname(phi_1),
    standard_error = unname(standard_error),
    white_noise_sd = unname(white_noise_sd),
    t_value = unname(t_value),
    p_value = unname(p_value),
    approach = "alternative",
    arima_results = arima_results
  ))
}


#' Method of Moments for AR(1) Parameters
#'
#' Estimate AR(1) parameters using the method of moments (MM) approach.
#' The MM approach avoids computing initial values and does not
#' rely on numerical optimization, making it less prone to convergence issues.
#'
#' The MM approach is used for estimating the parameters of an autoregressive
#' model of order 1 (AR(1)). The primary focus is on estimating the
#' autoregressive coefficient \eqn{\phi_1}, its standard error, the standard
#' deviation of the white noise, the t-value, and the p-value.
#'
#' First, the sample mean (\eqn{\bar{y}}) is computed excluding missing values.
#' Next, missing data are processed by identifying valid pairs where both
#' \eqn{y_t} and \eqn{y_{t-1}} are non-missing. These valid pairs are used to
#' compute the autocovariance at lag 0 (\eqn{\gamma_0}) and lag 1
#' (\eqn{\gamma_1}). Note that \eqn{\gamma_0} is the sample variance. The AR(1)
#' coefficient (\eqn{\phi_1}) is estimated as \eqn{\gamma_1 / \gamma_0}.
#' Residuals are then computed as the difference between observed values and
#' those predicted by the AR(1) model. The standard deviation of these residuals
#' represents the white noise standard deviation (\eqn{\sigma_\epsilon}).
#'
#' The standard error of \eqn{\phi_1} (\eqn{SE_{\phi_1}}) is computed as
#' \eqn{\sigma_\epsilon / \sqrt{n \gamma_0}}, where \eqn{\sigma_\epsilon} and
#' \eqn{\gamma_0} are defined earlier and \eqn{n} is the number of valid pairs.
#' The t-value is calculated as \eqn{\phi_1 / SE_{\phi_1}}, and the p-value is
#' obtained from the t-distribution with \eqn{n - 4} degrees of freedom. \eqn{n
#' - 4} is used since the parameters \eqn{\bar{y}}, \eqn{\gamma_0},
#' \eqn{\gamma_1}, and \eqn{\phi_1} are computed in this method. Note that
#' \eqn{n - 2} is used for the ML method since the method does not explicitly
#' calculate the autocovariance parameters.
#'
#' @param y A numeric vector representing the time series data.
#' @return A list containing the estimated AR(1) parameter (phi_1), standard error, white noise standard deviation, t-value, p-value, and the approach used.
#' @examples
#' set.seed(123)
#' y    <- arima.sim(n = 40, list(ar = 0.40))
#' y[8] <- NA
#' ar1_moment(y)
#'
#'
#' @importFrom stats arima pt sd
#'
#' @seealso \code{\link{estimate_ar1}},
#'   \code{\link{plot_acf_pacf}},
#'   \code{\link{ar1_default}},
#'   \code{\link{ar1_alter}}
#'
#' @export
#' @keywords internal
#'
ar1_moment <- function(y) {

  # Function to remove NA pairs
  remove_na_pairs <- function(y) {
    valid_indices <- which(!is.na(y[-length(y)]) & !is.na(y[-1]))
    return(valid_indices)
  }

  # Function to compute sample mean, excluding NAs
  compute_mean <- function(y) {
    return(mean(y, na.rm = TRUE))
  }

  # Function to compute sample autocovariance
  compute_autocovariance <- function(y, y_lag) {
    y_mean <- compute_mean(y)
    valid_indices <- remove_na_pairs(y)
    if (length(valid_indices) == 0) {
      stop("No valid pairs to compute autocovariance")
    }

    if (y_lag == 0) {
      return(compute_mean((y - y_mean)^2))
    } else {
      return(compute_mean((y[valid_indices] - y_mean) * (y[valid_indices + y_lag] - y_mean)))
    }
  }

  # Function to estimate phi_1
  estimate_phi_1 <- function(y) {
    gamma_0 <- compute_autocovariance(y, 0)
    gamma_1 <- compute_autocovariance(y, 1)

    if (gamma_0 == 0) {
      stop("Variance is zero, cannot compute phi_1")
    }

    phi_1 <- gamma_1 / gamma_0
    return(phi_1)
  }

  # Function to compute the residuals
  compute_residuals <- function(y, phi_1) {
    valid_indices <- remove_na_pairs(y)
    residuals <- y[valid_indices + 1] - phi_1 * y[valid_indices]
    return(residuals)
  }

  # Function to compute standard deviation of white noise
  compute_white_noise_sd <- function(residuals) {
    return(sd(residuals, na.rm = TRUE))
  }

  # Function to compute standard error of phi_1
  compute_standard_error <- function(y, residuals) {
    valid_indices <- remove_na_pairs(y)
    n <- length(valid_indices)
    white_noise_sd <- compute_white_noise_sd(residuals)
    gamma_0 <- compute_autocovariance(y, 0)
    standard_error <- white_noise_sd / sqrt(n * gamma_0)
    return(standard_error)
  }

  # Function to compute t-value
  compute_t_value <- function(phi_1, standard_error) {
    t_value <- phi_1 / standard_error
    return(t_value)
  }

  # Function to compute p-value
  compute_p_value <- function(t_value, df) {
    p_value <- 2 * (1 - pt(abs(t_value), df))
    return(p_value)
  }

  # > Calculations ####
  phi_1 <- estimate_phi_1(y)
  residuals      <- compute_residuals(y, phi_1)
  standard_error <- compute_standard_error(y, residuals)
  white_noise_sd <- compute_white_noise_sd(residuals)
  t_value        <- compute_t_value(phi_1, standard_error)
  valid_indices  <- which(!is.na(y[-length(y)]) & !is.na(y[-1]))
  n              <- length(valid_indices)
  p_value        <- compute_p_value(t_value, df = n - 4)

  (list(
    phi_1 = phi_1,
    standard_error = standard_error,
    white_noise_sd = white_noise_sd,
    t_value = t_value,
    p_value = p_value,
    approach = "moments",
    arima_results = ""
  ))
}



#' Plot Time Series, ACF, and PACF
#'
#' This function plots a time series along with its ACF (Autocorrelation Function) and PACF (Partial Autocorrelation Function).
#' It also performs AR(1) parameter estimation and displays the estimated phi and its p-value in the time series plot.
#'
#' @param y A numeric vector representing the time series data.
#'
#' @return This function does not return a value. It generates a series of plots.
#'
#' @details The function first estimates the AR(1) parameters using the
#'   \code{\link{estimate_ar1}} function. It then sets up a layout for plotting:
#' \itemize{
#'   \item The first row contains the time series plot and lag-1 plot
#'   \item The second row contains the ACF and PACF plots
#' }
#' If the AR(1) estimation fails, the phi and p-value are set to NA and displayed accordingly.
#'
#' @import graphics
#' @importFrom stats arima na.pass acf pacf
#' @importFrom utils head tail
#' @importFrom tibble tibble
#'
#' @examples
#' set.seed(123)
#' y    <- arima.sim(n = 40, list(ar = 0.40))
#' y[8] <- NA
#' plot_acf_pacf(y)
#' estimate_ar1(y)
#'
#' @seealso \code{\link{estimate_ar1}},
#'   \code{\link{ar1_default}},
#'   \code{\link{ar1_alter}},
#'   \code{\link{ar1_moment}}
#'
#' @export
#'
plot_acf_pacf <- function(y) {

  # Function to check if a value is neither NA nor NaN
  is_not_na_and_not_nan <- function(x) {
    if (!is.list(x)) {
      !is.na(x) & !is.nan(x)
    } else if (is.list(x)) {
      TRUE
    }
  }

  # Set up the plotting layout to have the time series on the top row
  # and ACF/PACF side by side on the second row
  graphics::layout(matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE), heights = c(1, 1))

  ar_result <- estimate_ar1(y)

  # Check if all values in ar_result are neither NA nor NaN
  if (!(all(sapply(ar_result, is_not_na_and_not_nan)))) {
    phi <- NA_real_
    phi_pvalue1 <- NA_real_
  } else {
    phi <- ar_result$phi_1
    phi_pvalue1 <- ar_result$p_value
    phi <- signif(phi, 4)
    phi_pvalue1 <- signif(phi_pvalue1, 4)
  }

  # > Plot time series ####
  plot(y, type = "p", pch = 21, col = "black", bg = "white", main = paste0("phi = ", phi, " (p-value = ", phi_pvalue1,")"),
       xlab = "Time Index", ylab = "Value")
  grid()
  abline(h = 0, col = "red", lty = 2, lwd = 0.5)

  # > Plot lag-1 plot ####
  lagged_data <- tibble::tibble(
    e_t = tail(y, -1),
    e_t_1 = head(y, -1),
    id = 2:length(y)
  )

  plot(lagged_data$e_t_1, lagged_data$e_t, type = "p", pch = 21, col = "black", bg = "white",
       main = "Lag-1 Plot", xlab = expression(e[t-1]), ylab = expression(e[t]))
  grid()
  abline(h = 0, col = "red", lty = 2, lwd = 0.5)
  abline(v = 0, col = "red", lty = 2, lwd = 0.5)

  # > Plot the ACF ####
  acf(y, main = "ACF", na.action = na.pass)

  # > Plot the PACF ####
  pacf(y, main = "PACF", na.action = na.pass)

  # Reset plotting layout
  graphics::layout(1)

} # FUN ~ plot_acf_pacf

