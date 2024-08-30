
# fun_$_correlation.R - functions to compute and plot correlations

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
#' @title Compute Correlation Matrix and P-Values
#'
#' @description This function computes the correlation matrix and the
#'   corresponding p-values for a given dataset using standard methods.
#'
#' @param data A data frame or matrix containing the numeric variables for which
#'   the correlation matrix is to be computed.
#'
#' @return A list containing the following components:
#' \item{cor_matrix}{The correlation matrix computed using standard methods.}
#' \item{p_matrix}{A matrix of p-values corresponding to each element in the
#' correlation matrix.}
#' \item{mean_vector}{A vector of means corresponding to each column of data.}
#'
#' @examples
#' data <- mtcars[, c("mpg", "disp", "hp", "qsec", "drat", "wt")]
#' compute_cor_matrix(data)
#'
#' @importFrom stats pnorm cor
#'
#' @export
#'
#' @seealso \code{\link{plot_cor_mat}}
#'
compute_cor_matrix <- function(data) {

  # Set the seed for reproducibility
  # set.seed(12271963)

  # Compute the correlation matrix
  cor_matrix <- cor(data)
  colnames(cor_matrix) <- colnames(data)
  rownames(cor_matrix) <- colnames(data)

  # Initialize p-value matrix
  p_matrix <- matrix(NA, nrow = ncol(data), ncol = ncol(data))
  colnames(p_matrix) <- colnames(data)
  rownames(p_matrix) <- colnames(data)

  n <- ncol(data)

  # Calculate p-values for each covariance value
  data <- as.matrix(data)
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      test <- cor.test(data[, i], data[, j])
      p_matrix[i, j] <- p_matrix[j, i] <- test$p.value
    }
  }

  return(list(cor_matrix = cor_matrix, p_matrix = p_matrix, mean_vector = colMeans(data)))
}



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
#' @title Matrix-Based Correlation Plot
#' @description This function allows the user to create a correlation matrix plot using either
#' the `corrplot` or `ggpairs` approach. The function is flexible and allows the user to
#' customize various parameters for the plots.
#'
#' @param data The dataset to be used for correlation analysis.
#' @param method A string indicating the desired plotting method. Options are `"corrplot"` or `"ggpairs"`.
#' @param sig.level The significance level for indicating when to cross out panels. Default is 0.05.
#' @param color_scheme The color scheme to be used for both the lower and upper triangles in `corrplot.mixed`. Default is 'RdYlBu'.
#' @param color_n The number of colors to use in the color scale. Default is 10.
#' @param alpha Transparency level for scatter plots in `ggpairs`. Default is 0.35.
#'
#' @return A correlation matrix plot based on the specified method.
#' @examples
#'
#' # Create a correlation plot using ggpairs
#' data <- mtcars[, c("mpg","disp", "hp", "qsec", "drat", "wt")]
#' plot_cor_mat(data, sig.level = 0.01)
#'
#' # Create a correlation plot using corrplot
#' plot_cor_mat(data, method = "corrplot", sig.level = 0.01)
#'
#' @export
#'
#' @seealso \code{\link{compute_cor_matrix}}, \code{\link{plot_cor_mat_ggpairs}},
#'   \code{\link{plot_cor_mat_corrplot}}
#'
plot_cor_mat <- function(data, method = c("ggpairs", "corrplot"),
                                    sig.level = 0.05, color_scheme = "RdYlBu",
                                    color_n = 10, alpha = 0.35) {

  method <- match.arg(method)

  if (method == "corrplot") {
    plot_cor_mat_corrplot(data, sig.level, color_scheme, color_scheme, color_n)
  } else if (method == "ggpairs") {
    plot_cor_mat_ggpairs(data, sig.level, alpha)
  }
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
#' @title Correlation Plot using ggpairs
#' @description A helper function to create a correlation plot using `ggpairs`.
#'
#' @param data The dataset to be used for correlation analysis.
#' @param sig.level The significance level for indicating when to cross out panels. Default is 0.05.
#' @param alpha Transparency level for scatter plots in `ggpairs`. Default is 0.35.
#'
#' @importFrom GGally ggpairs wrap
#' @importFrom ggplot2 theme_minimal element_rect
#'
#' @export
#' @keywords internal
#'
#' @seealso \code{\link{plot_cor_mat}}, \code{\link{compute_cor_matrix}}
#'
plot_cor_mat_ggpairs <- function(data, sig.level = 0.05, alpha = 0.35) {

  GGally::ggpairs(data,

          # output correlation and significance to figure in the upper diagonal plots
          upper = list(continuous = function(data, mapping, ...)
            plot_cor_mat_ggpairs_aux(data, mapping, sig.level = sig.level, ...)),

          # bivariate plot of data in the lower diagonal plots
          lower = list(continuous = GGally::wrap("points", alpha = alpha)),

          # plot pdf on diagonal
          diag = list(continuous = GGally::wrap("densityDiag")),

          # Suppress the progress bar
          progress = FALSE) +

    theme_minimal() +
    theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.25))
}

#' @title plot_cor_mat_ggpairs Helper Function
#' @description This function calculates and displays the correlation coefficient, p-value,
#' covariance, and its p-value. It adds an "X" on non-significant correlations.
#'
#' @param data The dataset containing the variables to be analyzed.
#' @param mapping Aesthetic mapping created by `aes` or `aes_`.
#' @param sig.level The significance level for indicating when to cross out panels. Default is 0.05.
#'
#' @importFrom stats cor.test
#' @importFrom GGally ggally_text eval_data_col
#' @importFrom ggplot2 geom_text aes theme_void
#'
#' @export
#' @keywords internal
#'
plot_cor_mat_ggpairs_aux <- function(data, mapping, sig.level, ...) {

  # extract var1 and var2 from data and store as x and y
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)

  # Run correlation test
  corr <- cor.test(x, y)

  # format r and p
  r <- round(corr$estimate, 3)

  if (corr$p.value < 0.001) {
    p <- "<0.001"
  } else {
    p <- formatC(corr$p.value, format = "f", digits = 3)
  }

  # create label
  label <- paste0("r = ", r, "\np = ", p)

  # put label on figure
  p <- GGally::ggally_text(
    label = label,
    mapping = mapping,
    xP = 0.5, yP = 0.5,
    color = "black",
    ...
  ) +
    theme_void()

  # add "X" to figure if correlation is not significant
  if (corr$p.value > sig.level) {
    p <- p + geom_text(aes(x = 0.5, y = 0.5, label = "X")
                       , color = "black"
                       , size = 8
                       , fontface = "bold")
  }

  return(p)
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
#' @title Correlation Plot using corrplot
#' @description A helper function to create a correlation plot using `corrplot.mixed`.
#'
#' @param data The dataset to be used for correlation analysis.
#' @param sig.level The significance level for indicating when to cross out panels. Default is 0.05.
#' @param lower_col The color scheme for the lower triangle. Default is 'RdYlBu'.
#' @param upper_col The color scheme for the upper triangle. Default is 'RdYlBu'.
#' @param color_n The number of colors to use in the color scale. Default is 10.
#'
#' @importFrom corrplot corrplot.mixed cor.mtest COL2
#' @importFrom stats cor
#'
#' @export
#' @keywords internal
#'
#' @seealso \code{\link{plot_cor_mat}}, \code{\link{compute_cor_matrix}}
#'
plot_cor_mat_corrplot <- function(data, sig.level = 0.05, lower_col = "RdYlBu", upper_col = "RdYlBu", color_n = 10) {

  # calculate correlation matrix
  M <- cor(data)

  # calculate p values (also returns lower and upper bounds)
  testRes <- corrplot::cor.mtest(data, conf.level = (1-sig.level))

  # make plot
  corrplot::corrplot.mixed(M,

                 # lower diaganol by default is the numerical value
                 lower.col = corrplot::COL2(lower_col, n = color_n),

                 # upper diaganol are circles
                 upper.col = corrplot::COL2(upper_col, n = color_n),

                 # color bar and label location
                 cl.length = color_n + 1, tl.pos = "lt",

                 # cross out results that are not significant
                 p.mat = testRes$p, sig.level = sig.level)
}
