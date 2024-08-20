

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
#' @title Compute Covariance Matrix and P-Values
#'
#' @description This function computes the covariance matrix and the
#'   corresponding p-values for a given dataset using standard methods.
#'
#' @param data A data frame or matrix containing the numeric variables for which
#'   the covariance matrix is to be computed.
#'
#' @return A list containing the following components:
#' \item{cov_matrix}{The covariance matrix computed using standard methods.}
#' \item{p_matrix}{A matrix of p-values corresponding to each element in the
#' covariance matrix.}
#'
#' @examples
#' data <- mtcars[, c("mpg", "disp", "hp", "qsec", "drat", "wt")]
#' compute_cov_matrix(data)
#'
#' @importFrom stats pnorm cov
#'
#' @export
#'
#' @seealso \code{\link{plot_cov_mat}}
#'
compute_cov_matrix <- function(data) {

  # Compute the covariance matrix
  cov_matrix <- cov(data)
  colnames(cov_matrix) <- colnames(data)
  rownames(cov_matrix) <- colnames(data)

  # Initialize p-value matrix
  p_matrix <- matrix(NA, nrow = ncol(data), ncol = ncol(data))
  colnames(p_matrix) <- colnames(data)
  rownames(p_matrix) <- colnames(data)

  # Calculate p-values for each covariance value
  n <- nrow(data)
  for (i in 1:ncol(data)) {
    for (j in 1:ncol(data)) {
      t_stat <- cov_matrix[i, j] / (sqrt(cov_matrix[i, i] * cov_matrix[j, j]) / sqrt(n))
      p_matrix[i, j] <- 2 * (1 - pnorm(abs(t_stat)))
    }
  }

  return(list(cov_matrix = cov_matrix, p_matrix = p_matrix, mean_vector = colMeans(data)))
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
#' @title Matrix-Based Covariance Plot
#'
#' @description This function creates a `ggpairs` plot that displays the
#'   covariance matrix and p-values in the upper diagonal, bivariate scatter
#'   plots in the lower diagonal, and density plots on the diagonal.
#'
#' @param data A data frame containing the numeric variables to be plotted.
#' @param sig.level Significance level to determine if an "X" should be added to
#'   non-significant correlations. Default is 0.05.
#' @param alpha Transparency level for the scatter plots in the lower diagonal.
#'   Default is 0.35.
#'
#' @return A `ggpairs` plot displaying the covariance matrix, p-values, and
#'   bivariate relationships.
#'
#' @examples
#' data <- mtcars[, c("mpg", "disp", "hp", "qsec", "drat", "wt")]
#' plot_cov_mat(data, sig.level = 0.02)
#'
#' @importFrom GGally ggpairs wrap
#' @importFrom ggplot2 theme_minimal element_rect
#'
#' @export
#'
#' @seealso \code{\link{compute_cov_matrix}}
#'
plot_cov_mat <- function(data, sig.level = 0.05, alpha = 0.35) {

  # Compute the full covariance matrix and p-values
  calc_results <- compute_cov_matrix(data)

  # Use ggpairs to plot
  GGally::ggpairs(data,

          # Output covariance and significance to figure in the upper diagonal plots
          upper = list(continuous = function(data, mapping, ...)
            plot_cov_mat_aux(data, mapping, calc_results, sig.level = sig.level, ...)),

          # Bivariate plot of data in the lower diagonal plots
          lower = list(continuous = GGally::wrap("points", alpha = alpha)),

          # Plot density functions on the diagonal
          diag = list(continuous = GGally::wrap("densityDiag")),

          # Suppress the progress bar
          progress = FALSE) +

    theme_minimal() +
    theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.25))
}




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
#' @title plot_cov_mat Helper Function
#'
#' @description This helper function displays the precomputed covariance and p-values on a `ggpairs` plot.
#'
#' @param data A data frame containing the variables to be plotted.
#' @param mapping Aesthetic mapping created by `aes` or `aes_`.
#' @param calc_results A list containing the covariance matrix and p-value matrix as computed by `compute_cov_matrix`.
#' @param sig.level Significance level to determine if an "X" should be added to non-significant correlations.
#' @param ... Additional arguments passed to `ggally_text`.
#'
#' @return A `ggplot` object displaying the covariance and p-value in the upper diagonal of a `ggpairs` plot.
#'
#' @importFrom GGally ggally_text
#' @importFrom ggplot2 geom_text theme_void
#'
#' @export
#' @keywords internal
#'
#' @seealso \code{\link{compute_cov_matrix}}, \code{\link{plot_cov_mat}}
#'
plot_cov_mat_aux <- function(data, mapping, calc_results, sig.level, ...) {

  # Extract the covariance and p-value matrices from the results
  full_cov_matrix <- calc_results$cov_matrix
  p_matrix <- calc_results$p_matrix

  # Identify the variables being plotted
  x_var <- rlang::as_name(mapping$x)
  y_var <- rlang::as_name(mapping$y)

  # Directly match the variable names to the data columns
  x_var <- match(x_var, names(data))
  y_var <- match(y_var, names(data))

  # Check if the variables exist in the covariance matrix
  if (is.na(x_var) || is.na(y_var)) {
    stop("Variable names do not match with the covariance matrix")
  }

  # Extract the covariance value
  cov_value <- signif(full_cov_matrix[x_var, y_var], 3)

  # Format the covariance value: exponential notation if |cov_value| >= 10000
  if (abs(cov_value) >= 10000) {
    cov_value <- format(cov_value, scientific = TRUE, digits = 3)
  }

  # Extract the  p-value
  p_value <- p_matrix[x_var, y_var]

  # Format the p-value
  p_label <- ifelse(p_value < 0.001, "<0.001", round(p_value, 3))

  # Create the label for display
  label <- paste0("Cov = ",cov_value, "\np = ", p_label)

  # Plot the label in the panel
  p <- ggally_text(
    label = label,
    mapping = mapping,
    xP = 0.5, yP = 0.5,
    color = "black",
    size = 4,
    ...
  ) + theme_void()

  # Add "X" to figure if correlation is not significant
  if (p_value > sig.level) {
    p <- p + geom_text(aes(x = 0.5, y = 0.5, label = "X"), color = "black", size = 8, fontface = "bold")
  }

  return(p)
}
