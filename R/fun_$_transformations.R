# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
#' @title Transform Data Based on a Given Transformation Name
#'
#' @description Applies a specified transformation to a numeric vector `y_obs`
#' based on the transformation name (`trans_name`) and parameters
#' (`trans_parms`). Supports no transformation, logarithmic, and beta logit
#' transformations specified by `trans_name`. This function allows for flexible
#' data manipulation in preprocessing steps, where different types of
#' transformations might be required based on the characteristics of the data or
#' the analytical requirements.
#'
#' @details When `trans_name` is:
#' \itemize{
#'   \item {"none": No transformation is applied, and `y_obs` is returned as-is.}
#'   \item {"log": A natural logarithm transformation is applied to each element of `y_obs`.}
#'   \item {starts with "BL_": A beta logit transformation, as defined by the `beta_logit_tran` function, is applied using the parameters specified in `trans_parms`.}
#' }
#' It's important to ensure that `y_obs` and `trans_parms` are compatible with the chosen
#' transformation to avoid errors or unexpected behavior.
#'
#' @param y_obs A numeric vector of observations to be transformed.
#' @param trans_name A character string specifying the type of transformation to
#' apply. Supported values include "none", "log", and those starting with "BL_"
#' for beta logit transformations.
#' @param trans_parms A numeric vector or other relevant parameters required by
#' the transformation. For beta logit transformations, this should be a numeric
#' vector specifying the parameters of the transformation.
#' @param show_msgs Logical; if `TRUE`, messages will be issue where applicable
#'
#' @return A numeric vector containing the transformed data.
#'
#' @examples
#' y_obs <- c(1, 2, 3, 4, 5)
#' # No transformation
#' y <- transform_data(y_obs, "none", NULL)
#' transform_data_inverse(y, "none", NULL)
#'
#' # Logarithmic transformation
#' y <- transform_data(y_obs, "log", NULL)
#' transform_data_inverse(y, "log", NULL)
#'
#' # Beta logit transformation (example parameters)
#' trans_parms <- c(0.1, 6, 2, 2)
#' y <- transform_data(y_obs, "BL_example", trans_parms)
#' transform_data_inverse(y, "BL_example", trans_parms)
#'
#' @export
#'
#' @seealso \code{\link[stats]{qbeta}} for details on beta distribution
#'   functions. \code{\link{beta_logit_tran}},
#'   \code{\link{beta_logit_tran_inverse}},
#'   \code{\link{transform_data_inverse}}
#'
#'
transform_data <- function(y_obs, trans_name, trans_parms, show_msgs = FALSE) {

  # Transform "y_obs" to "y"

  if (trans_name == "none") {
    y <- y_obs
  } else if (trans_name == "log") {
    y <- log(y_obs)
  } else if (startsWith(trans_name, "BL_")) {
    y <- beta_logit_tran(y_obs, bl4 = trans_parms, show_msgs)
  }

  return(y)
}## FUN ~ transform_data

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
#' @title Inverse Transformation of Data
#'
#' @description Applies the inverse of a specified transformation to a numeric
#' vector `y` based on the transformation name (`trans_name`) and parameters
#' (`trans_parms`). This function reverses the effects of transformations
#' applied by `transform_data` or similar functions, returning the data to its
#' original scale. Supports reversing no transformation, logarithmic, and beta
#' logit transformations specified by `trans_name`.
#'
#' @details When `trans_name` is:
#' \itemize{
#'   \item{"none": No transformation is reversed, and `y` is returned as-is, implying the data
#'      remains on its original scale.}
#'   \item{"log": An exponential transformation is applied to each element of `y`, reversing the
#'      logarithmic transformation.}
#'   \item{starts with "BL_": An inverse beta logit transformation, as defined by the
#'      `beta_logit_tran_inverse` function, is applied using the parameters specified in `trans_parms`,
#'      returning the data to its original bounded domain.}
#' }
#' It's crucial to match the transformation name and parameters with those used
#'  in the initial transformation to ensure accurate reversal.
#'
#' @param y A numeric vector of transformed observations to be back-transformed.
#' @param trans_name A character string specifying the type of transformation to
#'  reverse. Supported values include "none", "log", and those starting with
#'  "BL_" for beta logit transformations.
#' @param trans_parms A numeric vector or other relevant parameters required by
#' the transformation to be reversed. For beta logit transformations, this
#' should be a numeric vector specifying the parameters of the transformation.
#' @param show_msgs Logical; if `TRUE`, messages will be issue where applicable
#'
#' @return A numeric vector containing the back-transformed data, aiming to
#' reflect the original observations before any transformation was applied.
#'
#' @examples
#' y_obs <- c(1, 2, 3, 4, 5)
#' # No transformation
#' y <- transform_data(y_obs, "none", NULL)
#' transform_data_inverse(y, "none", NULL)
#'
#' # Logarithmic transformation
#' y <- transform_data(y_obs, "log", NULL)
#' transform_data_inverse(y, "log", NULL)
#'
#' # Beta logit transformation (example parameters)
#' trans_parms <- c(0.1, 6, 2, 2)
#' y <- transform_data(y_obs, "BL_example", trans_parms)
#' transform_data_inverse(y, "BL_example", trans_parms)
#'
#' @export
#'
#' @seealso \code{\link[stats]{qbeta}} for details on beta distribution
#'   functions. \code{\link{beta_logit_tran}},
#'   \code{\link{beta_logit_tran_inverse}}, \code{\link{transform_data}}
#'
transform_data_inverse <- function(y
                                   , trans_name
                                   , trans_parms
                                   , show_msgs = FALSE) {

  # Back transform "y" to "y_obs"

  if (trans_name == "none") {
    y_obs <- y
  } else if (trans_name == "log") {
    y_obs <- exp(y)
  } else if (startsWith(trans_name, "BL_")) {
    y_obs <- beta_logit_tran_inverse(y, bl4 = trans_parms, show_msgs=FALSE)
  }

  return(y_obs)
}## FUN ~ transform_data_inverse

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
#' @title Beta Logit Transformation
#'
#' @description Applies a beta logit transformation to a numeric vector `x`,
#' effectively scaling it from a bounded domain defined by parameters `a` and
#' `b` to an unbounded range. This transformation involves normalizing `x` to a
#' [0,1] range, applying a beta cumulative distribution function, and then using
#' a logit function to transform the result to the real line. This is
#' particularly useful for data that must be constrained within specific bounds
#' but requires transformation to an unbounded scale for statistical modeling or
#'  analysis.
#'
#' @details The transformation process is defined as follows:
#' \enumerate{
#'   \item Normalize `x` to a [0,1] range based on the lower (`a`) and upper (`b`) bounds.
#'   \item Apply the beta cumulative distribution function with parameters `b1` and `b2`.
#'   \item Use the logit function to map the beta distribution values to an unbounded range.
#' }
#' This function is particularly suited for preparing bounded data for
#' regression models where normality or linearity assumptions on the original
#' scale are problematic.
#'
#' @param x A numeric vector representing the data to be transformed.
#' @param bl4 A numeric vector of length four providing the transformation
#' parameters: the lower (`a`) and upper (`b`) bounds of the data's domain, and
#' the `b1` and `b2` parameters of the beta distribution function.
#' @param show_msgs Logical; if `TRUE`, messages will be issue where applicable
#'
#' @return A numeric vector of the same length as `x`, containing the
#' transformed values.
#'
#' @examples
#' \dontrun{
#' x <- seq(0, 1, length.out = 101)
#' bl4 <- c(0, 1, 2, 2)  # beta logit transformation parameters
#' transformed_x <- beta_logit_tran(x, bl4, TRUE)
#' back_transformed_x <- beta_logit_tran_inverse(transformed_x, bl4, TRUE)
#' }
#'
#' @importFrom zeallot %<-%
#' @importFrom dplyr %>%
#' @importFrom stats pbeta
#'
#' @seealso \code{\link[stats]{qbeta}} for details on beta distribution
#'   functions. \code{\link{beta_logit_tran_inverse}},
#'   \code{\link{transform_data}}, \code{\link{transform_data_inverse}}
#'
#' @export
#' @keywords internal
#'
beta_logit_tran <- function(x, bl4, show_msgs = FALSE) {

  # global variable bindings
  a <- b <- b1 <- b2 <- . <- NULL

  # Ensure x is a numeric vector
  if(!is.numeric(x)) {
    stop("x must be a numeric vector.")
  }

  # Validate bl4 for correct length and numeric values
  if(length(bl4) != 4 || any(is.na(bl4))) {
    stop("bl4 must be a numeric vector with exactly four non-na elements.")
  }

  # Validate input
  if (show_msgs) {
    if(any(x <= bl4[1])) message("beta_logit_tran: element of x < a, not in the interval (a, b)")
    if(any(x >= bl4[2])) message("beta_logit_tran: element of x > b, not in the interval (a, b)")
  }

  blt <- function(x, bl4) {
    # Set parameters in the function's environment
    c(a, b, b1, b2) %<-% bl4

    # perform blt
    x %>%
      {(. - a) / (b - a)} %>%  # Normalize x to [0,1] range
      pbeta(shape1 = b1, shape2 = b2, ncp = 0, log.p = FALSE) %>%
      {log(. / (1 - .))} -> z  # Apply logit transformation
    return(z)
  }

  # Transformation process
  z <- blt(x, bl4)

  # Check for non-finite values
  if(any(is.infinite(z))) {

    # Determine a max parameter value that will transform a non +Inf value
    max_x   <- max(x)
    max_z   <- blt(max_x, bl4)
    max_x_del <- ifelse(max(x) == 0, 0.0001, abs(0.0001 * max(x)))

    while (is.infinite(max_z)) {
      max_x <- max_x - max_x_del
      max_z <- blt(max_x, bl4)
    }

    # Determine a max parameter value that will transform a non -Inf value
    min_x   <- min(x)
    min_z   <- blt(min_x, bl4)
    min_x_del <- ifelse(min(x) == 0, 0.0001, abs(0.0001 * min(x)))

    while (is.infinite(min_z)) {
      min_x <- min_x + min_x_del
      min_z <- blt(min_x, bl4)
    }

    # Display message
    if(show_msgs) {
      message("beta_logit_tran: extreme events truncated to finite values")
      print(x[is.infinite(z)])
    }

    z[z ==  Inf] <- max_z
    z[z == -Inf] <- min_z

  }

  # Return transformed values
  return(z)
}## FUN ~ beta_logit_tran

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
#' @title Inverse Beta Logit Transformation
#'
#' @description Reverses the beta logit transformation applied by
#' `beta_logit_tran`, converting a numeric vector `z` from an unbounded range
#' back to its original bounded domain. This process involves applying the
#' inverse logit function to `z`, using the inverse beta cumulative distribution
#'  function, and then scaling the result back to the original domain defined by
#'  parameters `a` and `b`. It is useful for interpreting the results of
#' analyses performed on data transformed by `beta_logit_tran`, allowing the
#' transformed values to be mapped back to their original scale.
#'
#' @details The inverse transformation process consists of the following steps:
#' \enumerate{
#'   \item Apply the inverse logit function to `z`, mapping it from an unbounded range to (0,1).
#'   \item Use the inverse beta cumulative distribution function with parameters `b1` and `b2` to
#'        find the corresponding quantile values.
#'   \item Scale these values back to the original domain of the data, defined by the lower (`a`)
#'        and upper (`b`) bounds.
#' }
#' This function enables the recontextualization of model outputs or analyses
#' back to the practical, bounded domain of the original data, facilitating
#' interpretation and decision-making.
#'
#' @param z A numeric vector representing the transformed data to be
#' back-transformed.
#' @param bl4 A numeric vector of length four providing the transformation
#' parameters: the lower (`a`) and upper (`b`) bounds of the original data's
#' domain, and the `b1` and `b2` parameters of the beta distribution function.
#' @param show_msgs Logical; if `TRUE`, messages will be issue where applicable
#'
#' @return A numeric vector of the same length as `z`, containing the
#' back-transformed values, aiming to reflect the original observations.
#'
#' @examples
#' \dontrun{
#' x <- seq(0, 1, length.out = 101)
#' bl4 <- c(0, 1, 2, 2)  # beta logit transformation parameters
#' transformed_x <- beta_logit_tran(x, bl4, TRUE)
#' back_transformed_x <- beta_logit_tran_inverse(transformed_x, bl4, TRUE)
#' }
#'
#' @importFrom zeallot %<-%
#' @importFrom dplyr %>%
#' @importFrom stats qbeta
#'
#' @seealso \code{\link[stats]{qbeta}} for details on beta distribution
#'   functions. \code{\link{beta_logit_tran}},
#'   \code{\link{transform_data}},
#'   \code{\link{transform_data_inverse}}
#'
#' @export
#' @keywords internal
#'
beta_logit_tran_inverse <- function(z, bl4, show_msgs=FALSE) {

  # global variable bindings
  a <- b <- b1 <- b2 <- . <- NULL

  # Ensure z is a numeric vector
  if(!is.numeric(z)) {
    stop("z must be a numeric vector.")
  }

  # Validate bl4 for correct length and numeric values
  if(length(bl4) != 4 || any(is.na(bl4))) {
    stop("bl4 must be a numeric vector with exactly four non-na elements.")
  }

  # Set parameters in the function's environment
  c(a, b, b1, b2) %<-% bl4

  # Inverse transformation process
  z %>%
    exp(.) %>%
    {./ (1 + .)} %>%                       # Reverse logit transformation
    qbeta(shape1 = b1, shape2 = b2,
          ncp = 0, log.p = FALSE) %>%      # Find beta quantile
    {a + . * (b - a)} %>%                  # Scale back to original domain
    {replace(., is.na(.), b)} -> x         # Replace NA values with b

  # Return original values
  return(x)
}## FUN ~ beta_logit_tran_inverse

