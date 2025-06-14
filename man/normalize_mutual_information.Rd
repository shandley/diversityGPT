% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/information_theory.R
\name{normalize_mutual_information}
\alias{normalize_mutual_information}
\title{Normalize Mutual Information}
\usage{
normalize_mutual_information(mi_matrix, discrete_taxa, comp_data, method)
}
\arguments{
\item{mi_matrix}{Mutual information matrix}

\item{discrete_taxa}{Discretized taxa data}

\item{comp_data}{Component data}

\item{method}{Estimation method}
}
\value{
Normalized mutual information matrix
}
\description{
Converts mutual information to normalized scale (0-1)
}
\keyword{internal}
