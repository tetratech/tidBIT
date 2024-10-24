% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fun_$_correlation.R
\name{plot_cor_mat_ggpairs}
\alias{plot_cor_mat_ggpairs}
\title{Correlation Plot using ggpairs}
\usage{
plot_cor_mat_ggpairs(data, sig.level = 0.05, alpha = 0.35)
}
\arguments{
\item{data}{The dataset to be used for correlation analysis.}

\item{sig.level}{The significance level for indicating when to cross out panels. Default is 0.05.}

\item{alpha}{Transparency level for scatter plots in `ggpairs`. Default is 0.35.}
}
\description{
A helper function to create a correlation plot using `ggpairs`.
}
\seealso{
\code{\link{plot_cor_mat}}, \code{\link{compute_cor_matrix}}
}
\keyword{internal}
