% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cache_system.R
\name{cache_cleanup}
\alias{cache_cleanup}
\title{Clean up cache based on age and size limits}
\usage{
cache_cleanup(force_size_limit = FALSE, verbose = TRUE)
}
\arguments{
\item{force_size_limit}{Force cleanup if size limit exceeded}

\item{verbose}{Print cleanup information}
}
\value{
List with cleanup statistics
}
\description{
Clean up cache based on age and size limits
}
\examples{
\dontrun{
# Regular cleanup
cache_cleanup()

# Force cleanup with details
stats <- cache_cleanup(force_size_limit = TRUE, verbose = TRUE)
}
}
