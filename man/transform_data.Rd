% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fun_$_transformations.R
\name{transform_data}
\alias{transform_data}
\title{Transform Data Based on a Given Transformation Name}
\usage{
transform_data(y_obs, trans_name, trans_parms, show_msgs = FALSE)
}
\arguments{
\item{y_obs}{A numeric vector of observations to be transformed.}

\item{trans_name}{A character string specifying the type of transformation to
apply. Supported values include "none", "log", and those starting with "BL_"
for beta logit transformations.}

\item{trans_parms}{A numeric vector or other relevant parameters required by
the transformation. For beta logit transformations, this should be a numeric
vector specifying the parameters of the transformation.}

\item{show_msgs}{Logical; if `TRUE`, messages will be issued where applicable.}
}
\value{
A numeric vector containing the transformed data.
}
\description{
Applies a specified transformation to a numeric vector `y_obs`
based on the transformation name (`trans_name`) and parameters
(`trans_parms`). Supports no transformation, logarithmic, beta logit,
and Fisher Z transformations specified by `trans_name`. This function allows
for flexible data manipulation in preprocessing steps, where different types of
transformations might be required based on the characteristics of the data or
the analytical requirements.
}
\details{
When `trans_name` is:
\itemize{
  \item {"none": No transformation is applied, and `y_obs` is returned as-is.}
  \item {"log": A natural logarithm transformation is applied to each element of `y_obs`.}
  \item {starts with "BL_": A beta logit transformation, as defined by the `beta_logit_tran` function.}
  \item {"fisherZ": A Fisher Z transformation is applied, as defined by the `fisherZ` function.}
}
It's important to ensure that `y_obs` and `trans_parms` are compatible with the chosen
transformation to avoid errors or unexpected behavior.
}
\examples{
y_obs <- c(1, 2, 3, 4, 5)
# No transformation
y <- transform_data(y_obs, "none", NULL)
transform_data_inverse(y, "none", NULL)

# Logarithmic transformation
y <- transform_data(y_obs, "log", NULL)
transform_data_inverse(y, "log", NULL)

# Beta logit transformation (example parameters)
trans_parms <- c(0.1, 6, 2, 2)
y <- transform_data(y_obs, "BL_example", trans_parms)
transform_data_inverse(y, "BL_example", trans_parms)

}
\seealso{
\code{\link[stats]{qbeta}} for details on beta distribution
  functions.
  \code{\link{beta_logit_tran}},
  \code{\link{beta_logit_tran_inverse}},
  \code{\link{fisherZ}},
  \code{\link{fisherZ_inverse}},
  \code{\link{transform_data_inverse}}
}
