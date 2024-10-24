% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fun_$_covariance.R
\name{compute_cov_matrix}
\alias{compute_cov_matrix}
\title{Compute Covariance Matrix and P-Values}
\usage{
compute_cov_matrix(data)
}
\arguments{
\item{data}{A data frame or matrix containing the numeric variables for which
the covariance matrix is to be computed.}
}
\value{
A list containing the following components:
\item{cov_matrix}{The covariance matrix computed using standard methods.}
\item{p_matrix}{A matrix of p-values corresponding to each element in the
covariance matrix.}
\item{mean_vector}{A vector of means corresponding to each column of data.}
}
\description{
This function computes the covariance matrix and the
  corresponding p-values for a given dataset using standard methods.
}
\examples{
data <- mtcars[, c("mpg", "disp", "hp", "qsec", "drat", "wt")]
compute_cov_matrix(data)

}
\seealso{
\code{\link{plot_cov_mat}}
}
