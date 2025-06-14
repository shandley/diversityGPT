% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shapley_values.R
\name{prepare_component_data_for_shapley}
\alias{prepare_component_data_for_shapley}
\title{Prepare Component Data for Shapley Analysis}
\usage{
prepare_component_data_for_shapley(components, n_samples)
}
\arguments{
\item{components}{Universal information components}

\item{n_samples}{Number of samples}
}
\value{
Matrix of component values per sample
}
\description{
Converts universal information components to per-sample values for coalition analysis
}
\keyword{internal}
