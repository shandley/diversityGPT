% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/null_model_validation.R
\name{run_row_shuffle_null}
\alias{run_row_shuffle_null}
\title{Row Shuffle Null Model}
\usage{
run_row_shuffle_null(physeq, indicators, n_perm, parallel, n_cores)
}
\arguments{
\item{physeq}{Phyloseq object}

\item{indicators}{Original indicators}

\item{n_perm}{Number of permutations}

\item{parallel}{Use parallel processing}

\item{n_cores}{Number of cores}
}
\value{
List of null indicator distributions
}
\description{
Randomizes taxon occurrences while maintaining row sums (total abundances)
}
\keyword{internal}
