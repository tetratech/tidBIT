% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fun_run_gam_models_baywide_00.R
\name{create_job_que}
\alias{create_job_que}
\title{Create Job Queue for Modeling}
\usage{
create_job_que(
  cntrl_subestuary = NA,
  cntrl_wq_var = NA,
  cntrl_trans_names = NA,
  cntrl_gam_formula = NA,
  cntrl_by_term = NA,
  cntrl_date = NA,
  cntrl_data_source = NA,
  model_config = NA,
  show_msgs = FALSE
)
}
\arguments{
\item{cntrl_subestuary}{Numeric or NA; control for selecting subestuaries for
modeling.}

\item{cntrl_wq_var}{xyz}

\item{cntrl_trans_names}{zyx}

\item{cntrl_gam_formula}{Numeric or NA; control for selecting GAM formulas.}

\item{cntrl_by_term}{Numeric or NA; control for selecting terms to model by.}

\item{cntrl_date}{Numeric or NA; control for selecting date ranges.}

\item{cntrl_data_source}{Numeric or NA; control for selecting data sources.}

\item{model_config}{A list containing the model configuration settings.}

\item{show_msgs}{Logical; if `TRUE`, displays messages regarding the job
queue creation process.}
}
\value{
A data frame representing the job queue, with each row specifying a
unique job configuration for modeling.
}
\description{
Generates a comprehensive job queue based on various control
parameters and the model configuration. This function combines control
settings with the provided model configuration to form a detailed list of GAM
modeling jobs. Each job within the queue specifies the subsets of data and
configurations to be used in the modeling process, effectively organizing the
 modeling workflow.
}
\examples{
\dontrun{
model_config <- init_model_config("path/to/config_file.json", subestuary_data)
job_queue <- create_job_que(model_config=model_config)
}

}
