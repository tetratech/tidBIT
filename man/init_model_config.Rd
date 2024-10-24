% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fun_run_gam_models_baywide_00.R
\name{init_model_config}
\alias{init_model_config}
\title{Initialize Modeling Configuration Options}
\usage{
init_model_config(config_file, subestuary, show_msgs = FALSE)
}
\arguments{
\item{config_file}{A string specifying the path to the JSON configuration
file containing modeling settings.}

\item{subestuary}{A data frame containing information about subestuaries,
which is required for initializing certain model configuration lists like
`cfg_estuary_lst` and `cfg_by_term`.}

\item{show_msgs}{Logical; if `TRUE`, prints detailed information about the
initialized modeling configuration to the console.}
}
\value{
A list containing the initialized configuration variables for
modeling. This includes selections for subestuary domains, water quality
variables, GAM formulas, transformation parameters, and more.
}
\description{
Initializes modeling configuration variables from a JSON
configuration file. This includes setting up various parameters such as
waterbody identifiers (subestuary), Generalized Additive Model (GAM) formulas
, temporal domains, and data sources. It's designed to facilitate the setup
process for environmental modeling by organizing and validating configuration
 settings required for model execution.
}
\details{
The function reads and validates the JSON configuration file,
extracting key modeling parameters and initializing them for use in the
modeling process. This includes the configuration for GAM formulas, date
ranges, data sources, water quality variables, transformation names, and
parameters. It also allows for the selection of subestuary domains based on
the provided subestuary data frame. Optional verbose output provides a
detailed overview of the initialized settings for verification and review.
}
\examples{
\dontrun{
subestuary_data <- data.frame(cb4d_subestuary = c("CB1"
                                                  , "CB2"
                                                  , "CB3"
                                                  , "CB4"
                                                  , "CB5"))
modeling_vars <- init_model_config("path/to/config_file.json"
                                   , subestuary_data)
}

}
