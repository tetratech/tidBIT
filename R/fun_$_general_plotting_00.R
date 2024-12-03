
# fun_$_general_plotting_00.r

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
#' @title Create and Combine Bivariate Plots for Diagnostics
#'
#' @description This function creates and combines bivariate plots for observed versus predicted values
#' and residuals versus observed values. It is a diagnostic tool to visualize the relationship between
#' observed, predicted, and residual values in a 1x2 grid of plots.
#'
#' @details The function selects specified columns from a data frame, renames them for consistency,
#' and creates two bivariate plots using the `plot_bivariate_data` function. The combined plots are
#' displayed in a 1x2 grid with an optional title.
#'
#' @param df A data frame containing the observed, predicted, and residual values.
#' @param vars A character vector specifying the column names for observed, predicted, and residual values.
#' Defaults to `c("y_obs", "y_pred", "y_resid")`.
#' @param vlab A character vector specifying the labels for the plots. Defaults to `c("Observed", "Predicted", "Residual")`.
#' @param main A character string specifying the main title for the combined plot. Defaults to `NA`.
#' @param ... Additional arguments passed to the `plot_bivariate_data` function.
#'
#' @return A ggplot object representing the combined bivariate plots.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(y_obs = rnorm(6000), y_pred = rnorm(6000), y_resid = rnorm(6000))
#' plot_obs_pred_res(df)
#' plot_obs_pred_res(df, main = "Diagnostic Plots")
#' plot_obs_pred_res(df, main = "Diagnostic Plots", plot_type = "thin")
#' plot_obs_pred_res(df, main = "Diagnostic Plots", plot_type = "hex")
#' df1 <- data.frame(obs = rnorm(1000), pred = rnorm(1000), resid  = rnorm(1000))
#' plot_obs_pred_res(df1, vars = c("obs", "pred", "resid"),
#'                   vlab = c("Obs.", "Pred.", "Res."),
#'                   main = "Diagnostic Plots", plot_type = "all",
#'                   smooth_line = TRUE)
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#'
#' @seealso \code{\link{plot_bivariate_data}}
#'
#' @export

plot_obs_pred_res <- function(df, vars = c("y_obs", "y_pred", "y_resid"), vlab = c("Observed", "Predicted", "Residual"), main = NA, ...) {

  # vars = c("y_obs", "y_pred", "y_resid"); vlab = c("Observed", "Predicted", "Residual"); main = "test"

  stopifnot(all(vars %in% names(df)))

  df <- df %>%
    select(all_of(vars)) %>%
    `names<-`(c("y_obs", "y_pred", "y_resid"))

  obs_v_pred_plot <- plot_bivariate_data(df, "y_pred", "y_obs", vlab[2], vlab[1], ref_line = c(1,0), ...)
  res_v_obs_plot  <- plot_bivariate_data(df, "y_obs",  "y_resid", vlab[1], vlab[3], ref_line = c(0,0), ...)

  # > output a 1x2 grid of plots
  p1 <- patchwork::wrap_plots(obs_v_pred_plot, res_v_obs_plot, ncol = 2)

  # > add title
  if (!is.na(main)) {
    p1 <- p1 + patchwork::plot_annotation(title = main)
  }

  return(p1)

}## FUN ~ plot_obs_pred_res

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
#' @title Create a Bivariate Plot
#'
#' @description This function creates a bivariate plot for two specified variables from a data frame.
#' The plot can include points, hex bins, reference lines, and smooth lines.
#'
#' @details The function creates a scatter plot of the specified x and y variables. It supports different
#' plot types (points, hex bins) and adds optional reference lines and smooth lines. Data can be thinned
#' if the number of points exceeds a specified maximum.
#'
#' @param df A data frame containing the data to be plotted.
#' @param x_var A character string specifying the column name for the x variable.
#' @param y_var A character string specifying the column name for the y variable.
#' @param xlab A character string specifying the label for the x-axis.
#' Default = x_var
#' @param ylab A character string specifying the label for the y-axis.
#' Default = y_var
#' @param ref_line A numeric vector of length 2 specifying the slope and intercept of a reference line. Defaults to `NA`.
#' @param smooth_line Logical; if `TRUE`, adds a smooth line to the plot. Defaults to `FALSE`.
#' @param plot_title A character string specifying the title for the plot. Defaults to `NA`.
#' @param na.rm Logical; if `TRUE`, removes missing values before plotting. Defaults to `TRUE`.
#' @param plot_type A character string specifying the type of plot ("all", "thin", or "hex"). Defaults to `"all"`.
#' @param n An integer specifying the maximum number of points to plot. Defaults to `3000`.
#' @param sym_color A character string specifying the color of the points. Defaults to `"gray40"`.
#' @param alpha A numeric value specifying the transparency of the points. Defaults to `0.35`.
#' @param bins An integer specifying the number of bins for hex plots. Defaults to `30`.
#' @param bin_color_low A character string specifying the low color for hex bins. Defaults to `"gray75"`.
#' @param bin_color_high A character string specifying the high color for hex bins. Defaults to `"gray5"`.
#' @param ref_line_color A character string specifying the color of the reference line. Defaults to `"red"`.
#' @param ref_line_width A numeric value specifying the width of the reference line. Defaults to `1`.
#' @param ref_line_type A character string specifying the line type of the reference line. Defaults to `"solid"`.
#' @param smooth_line_color A character string specifying the color of the smooth line. Defaults to `"blue"`.
#' @param smooth_line_width A numeric value specifying the width of the smooth line. Defaults to `1`.
#' @param smooth_line_type A character string specifying the line type of the smooth line. Defaults to `"dashed"`.
#'
#' @return A ggplot object representing the bivariate plot.
#'
#' @examples
#' set.seed(12271963)
#' df <- data.frame(MASS::mvrnorm(500, mu = c(1, 1), Sigma = matrix(c(1, 0.5, 0.5, 1), 2)))
#' names(df) <- c("x", "y")
#' plot_bivariate_data(df, "x", "y", "X-axis Label", "Y-axis Label")
#' plot_bivariate_data(df, "x", "y", "X-axis Label", "Y-axis Label",
#'                      ref_line = c(1,0), smooth_line = TRUE)
#'
#' @seealso \code{\link{plot_obs_pred_res}}
#'
#' @importFrom dplyr %>%
#' @importFrom ggplot2 ggplot aes geom_point theme_minimal element_rect xlab ylab geom_abline geom_smooth annotate ggtitle scale_fill_gradient geom_hex
#' @importFrom dplyr mutate row_number select .data
#' @importFrom stats qnorm
#' @importFrom tidyr drop_na
#' @importFrom rlang sym
#'
#' @export
#'
plot_bivariate_data <- function(df
                                , x_var
                                , y_var
                                , xlab = x_var
                                , ylab = y_var
                                , ref_line = NA
                                , smooth_line = FALSE
                                , plot_title = NA
                                , na.rm = TRUE
                                , plot_type ="all"
                                , n=3000
                                , sym_color = "gray40"
                                , alpha = 0.35
                                , bins = 30
                                , bin_color_low = "gray75"
                                , bin_color_high = "gray5"
                                , ref_line_color = "red"
                                , ref_line_width = 1
                                , ref_line_type = "solid"
                                , smooth_line_color = "blue"
                                , smooth_line_width = 1
                                , smooth_line_type = "dashed") {


  # remove missing values
  if (na.rm) {
    df <- df %>% drop_na()
  }

  max_points_to_plot <- n

  # Convert x_var and y_var to symbols for safe referencing
  x_sym <- rlang::sym(x_var)
  y_sym <- rlang::sym(y_var)

  # Thinning the data if required
  if (nrow(df) > max_points_to_plot && plot_type == "thin") {
    thin_plot = ceiling(nrow(df) / max_points_to_plot)
    thin_perc = 100/thin_plot
    thin_perc_char <- ifelse(thin_perc < 0.1, sprintf("%.1g", thin_perc), sprintf("%.1f", thin_perc))
    df <- df %>%
      mutate(plot = row_number() %% thin_plot == 1 | (row_number() == nrow(df)))
    data_to_plot <- subset(df, plot)
  } else {
    thin_plot = 1
    thin_perc = 100
    data_to_plot <- df
  }

  # Create the plot
  p1 <- ggplot(data_to_plot, aes(x = !!x_sym, y = !!y_sym)) +
    theme_minimal() +
    theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.25)) +
    xlab(xlab) +
    ylab(ylab) +
    geom_point(color = sym_color, alpha = alpha)

  # Add points or hex plot based on plot_type
  if (plot_type == "all" || plot_type == "thin") {
    p1 <- p1 +
      geom_point(color = sym_color, alpha = alpha)
  } else if (plot_type == "hex") {
    p1 <- p1 +
      geom_hex(bins = bins, color = NA) +
      scale_fill_gradient(low = bin_color_low, high = bin_color_high)
  }

  # Add reference line
  if (all(!is.na(ref_line)) && is.numeric(ref_line) && length(ref_line) == 2) {
    p1 <- p1 + geom_abline(slope = ref_line[1], intercept = ref_line[2],
                           color = ref_line_color, linewidth = ref_line_width, linetype = ref_line_type)
  }

  # Add gam smooth
  if (smooth_line) {
    p1 <- p1 + geom_smooth(data = df, aes(x = .data[[x_var]], y = .data[[y_var]]), method = NULL, se = FALSE,
                           color = smooth_line_color, linewidth = smooth_line_width, linetype = smooth_line_type)
  }

  # Add thinning annotation
  if (plot_type == "thin" & (thin_perc < 100) ) {
    p1 <- p1 +
      annotate(geom = "label", x = -Inf, y = Inf,
               label = sprintf("%s%% of %s obs. plotted", thin_perc_char, formatC(nrow(df), format = "f", big.mark = ",", digits = 0)),
               hjust = -0.1, vjust = 1.1, size = 2.5, colour = "gray40",
               fill = "white", label.size = NA)
  }

  # Add title
  if (!is.na(plot_title)) {
    p1 <- p1 + ggtitle(plot_title)
  }

  return(p1)
}## FUN ~ plot_bivariate_data

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
#' @title Create a 4-Panel Distribution Plot
#'
#' @description This function creates a 4-panel distribution plot displaying the normal probability plot,
#' boxplot, histogram, and density plot for a single variable.
#'
#' @details The function creates a 4-panel distribution plot by combining a normal probability plot, boxplot,
#' histogram, and density plot for the input variable. It allows for thinning the data if the number of points
#' exceeds a specified maximum.
#'
#' @param y A numeric vector of data to evaluate.
#' @param vlab A character string specifying the label for the plots. Used as
#'   the y-axis label for the normal probability plot and boxplot, and the
#'   x-axis label for the histogram and density plot.
#' @param main A character string specifying the main title for the combined
#'   plot. Defaults to 'Distribution Plots'.
#' @param na.rm Logical; if `TRUE`, removes missing values before analysis.
#'   Defaults to `TRUE`.
#' @param plot_type A character string specifying the type of plot ("all" or
#'   "thin"). Defaults to `"all"`.
#' @param n An integer specifying the maximum number of points to plot for the
#'   normal probability plot and density plot. Defaults to `3000`.
#' @param bin_width_min A numeric value specifying the minimum bin width for the
#'   histogram and density plot. Defaults to `0.2`.
#'
#' @return A ggplot object representing the 4-panel distribution plot.
#'
#' @examples
#' \dontrun{
#' y <- rnorm(6000)
#' plot_4_panel_distr(y)
#' plot_4_panel_distr(y, plot_type = "thin")
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr tibble mutate select filter
#' @importFrom ggplot2 annotate coord_cartesian geom_boxplot geom_density
#'   geom_histogram geom_point ggplot ggplot_build ggtitle stat_qq_line theme
#'   theme_minimal xlab ylab position_jitter
#' @importFrom stats IQR quantile density
#'
#' @export
plot_4_panel_distr <- function(y, vlab = "", main = 'Distribution Plots',
                               na.rm = TRUE, plot_type = "all", n=3000,
                               bin_width_min = 0.2) {

  # Define outlier_set to avoid CRAN NOTE
  outlier_set <- NULL

  # phase 2 refactor idea: convert this function as a wrapper for the 4 plots

  # for testing
  # y <- rnorm(60000)
  # vlab = 'Observed'; main = 'Distribution Plots'; na.rm = TRUE; plot_type = "all"; n=3000;bin_width_min = 0.1
  # vlab = 'Observed'; main = 'Distribution Plots'; na.rm = TRUE; plot_type = "thin"; n=3000;bin_width_min = 0.1

  # > Error check to ensure plot_type is either "all" or "thin"
  stopifnot(plot_type %in% c("thin", "all"))

  max_points_to_plot <- n

  # remove missing values, set # of records & plotting range
  if (na.rm) y <- y[!is.na(y)]
  n_y <- length(y)
  pretty_limits <- range(pretty(range(y)))

  # sort y into data frame, mark records with "outlier" set to TRUE for
  # boxplot plotting of outliers, and q set to normal probability plotting
  # position
  df <- tibble(y = sort(y)) %>%
    mutate(outlier_set = y < (quantile(y, 0.25) - 1.5 * IQR(y)) |
             y > (quantile(y, 0.75) + 1.5 * IQR(y)) ) %>%
    mutate(q = qnorm((row_number()-0.5)/dplyr::n()))

  # mark records for thinned plotting for normal probability plot and
  # density plot
  if (n_y > max_points_to_plot & plot_type == "thin") {
    thin_plot = ceiling(nrow(df) / max_points_to_plot)
    thin_perc = 100/thin_plot
    thin_perc_char <- ifelse(thin_perc < 0.1, sprintf("%.1g", thin_perc), sprintf("%.1f", thin_perc))
    df <- df %>%
      mutate(plot = row_number() %% thin_plot == 1 | (row_number() == dplyr::n()))
  } else {
    thin_plot = 1
    thin_perc = 100
    df <- df %>%
      mutate(plot = TRUE)
  }

  # > Normal Probability Plot ####
  npp <- ggplot(df, aes(sample = y)) +
    stat_qq_line(color = "black") +
    geom_point(data = subset(df, plot), aes(x = q, y = y), color = "gray40", alpha = 0.35) +
    ggtitle("Normal Probability Plot") +
    xlab("Expected") +
    ylab(vlab) +
    coord_cartesian(ylim = pretty_limits) +
    theme_minimal() +
    theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.25))

  if (plot_type == "thin" & (thin_perc < 100) ) {
    npp <- npp +
      annotate(geom = "label", x = -Inf, y = Inf,
               label = sprintf("%s%% of %s obs. plotted", thin_perc_char, formatC(nrow(df), format = "f", big.mark = ",", digits = 0)),
               hjust = -0.1, vjust = 1.1, size = 2.5, colour = "gray40",
               fill = "white", label.size = NA)
  }

  # > Boxplot - outliers plotted with jitter ####

  # Plot
  box_plot <- ggplot(df, aes(x = "", y = y)) +
    geom_boxplot(outlier.shape = NA, fill = "gray90", alpha = 0.35) +
    geom_point(data = subset(df, subset = outlier_set), aes(y = y),
               position = position_jitter(width = 0.01), size = 1, shape = 8, color = "gray40", alpha = 0.35) +
    annotate("point", x = 1, y = mean(df$y, na.rm = TRUE), shape = 18, size = 4, color = "black") +
    ggtitle("Boxplot") +
    xlab(" ") +
    ylab(vlab) +
    coord_cartesian(ylim = pretty_limits) +
    theme_minimal() +
    theme(panel.border = element_rect(color = "black",  fill = NA, linewidth = 0.25))

  # Calculate optimal bandwidth using "SJ" method
  optimal_bw <- max(density(df$y, bw = "SJ")$bw, bin_width_min)

  # > Histogram ####
  histogram <- ggplot(df, aes(x = y)) +
    geom_histogram(binwidth = optimal_bw, fill = "gray90", color = "black", linewidth = 0.25) +  # Set fill and border
    ggtitle("Histogram") +
    xlab(vlab) +
    ylab("Frequency") +
    theme_minimal() +
    theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.25))

  # > Density Plot with Data Points ####
  density_plot <- ggplot(df, aes(x = y)) +
    geom_density(color = "red", bw = optimal_bw) + # Apply the calculated bandwidth here
    ggtitle("Density Plot") +
    xlab(vlab) +
    coord_cartesian(xlim = pretty_limits) +
    ylab("Density") +
    theme_minimal() +
    theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.05))

  cloud <- ggplot_build(density_plot)$layout$panel_params[[1]]$y.range[2] *.15

  density_plot <- density_plot +
    geom_point(data = subset(df, plot), aes(y = cloud), position = position_jitter(width = 0.0, height = cloud),
               color = "gray40", alpha = 0.35, size = 1) +
    geom_density(data = df, color = "red", bw = optimal_bw)

  if (plot_type == "thin" & (thin_perc < 100) ) {
    density_plot <- density_plot +
      annotate(geom = "label", x = -Inf, y = Inf,
               label = sprintf("%s%% of %s obs. plotted", thin_perc_char, formatC(nrow(df), format = "f", big.mark = ",", digits = 0)),
               hjust = -0.1, vjust = 1.1, size = 2.5, colour = "gray40",
               fill = "white", label.size = NA)
  }

  # > Arrange as 2x2 grid ####

  # Assuming npp, box_plot, histogram, and density_plot are your ggplot objects
  p1 <- patchwork::wrap_plots(npp, box_plot, histogram, density_plot, ncol = 2, nrow = 2) +
    patchwork::plot_annotation(title = main)

  return(p1)

}## FUN ~ plot_4_panel_distr


