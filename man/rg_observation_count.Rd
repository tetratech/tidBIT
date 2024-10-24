% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fun_run_gam_models_baywide_00.R
\name{rg_observation_count}
\alias{rg_observation_count}
\title{Count Observations by Source in Data Frame}
\usage{
rg_observation_count(df)
}
\arguments{
\item{df}{A data frame containing environmental or water quality
observations, including a 'source' column to indicate the data source for
each observation.}
}
\value{
A tibble summarizing the total number of stations, observations, and
counts of observations categorized by source (e.g., 'datahub', 'eotb',
'vecos', 'noaa').
}
\description{
Aggregates and counts observations within a given data frame,
categorizing counts by data source. This function is particularly useful for
summarizing the number of observations from different sources in
environmental and water quality datasets, providing a quick overview of data
availability and source
distribution.
}
\examples{
\dontrun{
observation_summary <- rg_observation_count(data_frame)
}

}
\keyword{internal}
