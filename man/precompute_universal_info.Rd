% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/performance_optimization.R
\name{precompute_universal_info}
\alias{precompute_universal_info}
\title{Precompute and save universal information}
\usage{
precompute_universal_info(physeq, output_file, compress = 9, ...)
}
\arguments{
\item{physeq}{phyloseq object}

\item{output_file}{Path to save precomputed results}

\item{compress}{Compression level (0-9)}

\item{...}{Additional arguments}
}
\value{
Path to saved file
}
\description{
Precompute universal information for large datasets and save to disk
}
\examples{
\dontrun{
# Precompute for later use
precompute_universal_info(
  large_phyloseq,
  "universal_info_large.rds",
  compress = 9
)
}
}
