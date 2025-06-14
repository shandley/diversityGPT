% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cached_analysis.R
\name{cached_extract_universal_information}
\alias{cached_extract_universal_information}
\title{Cached Universal Analysis Functions}
\usage{
cached_extract_universal_information(
  physeq,
  groups = NULL,
  include_phylogenetic = NULL,
  use_cache = TRUE,
  force_recompute = FALSE,
  show_progress = TRUE,
  cache_metadata = NULL
)
}
\arguments{
\item{physeq}{phyloseq object}

\item{groups}{Optional grouping variable}

\item{include_phylogenetic}{Include phylogenetic diversity (default: auto-detect)}

\item{use_cache}{Whether to use caching (default: TRUE)}

\item{force_recompute}{Force recomputation even if cached (default: FALSE)}

\item{show_progress}{Show progress bar (default: TRUE)}

\item{cache_metadata}{Optional metadata for cached results}
}
\value{
universal_information object
}
\description{
High-performance cached versions of expensive universal analysis operations
with intelligent caching and progress tracking
}
\details{
Cached universal information extraction

Extract universal information components with intelligent caching
}
\examples{
\dontrun{
# Cached universal analysis
universal_info <- cached_extract_universal_information(phyloseq_obj)

# Force recomputation
universal_info <- cached_extract_universal_information(
  phyloseq_obj, 
  force_recompute = TRUE
)
}
}
