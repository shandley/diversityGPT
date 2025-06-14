% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cached_analysis.R
\name{cached_diversity_suite}
\alias{cached_diversity_suite}
\title{Cached diversity suite analysis}
\usage{
cached_diversity_suite(
  physeq,
  groups = NULL,
  metrics = c("shannon", "simpson", "observed", "chao1"),
  include_universal = TRUE,
  include_consensus = TRUE,
  use_cache = TRUE,
  force_recompute = FALSE,
  show_progress = TRUE
)
}
\arguments{
\item{physeq}{phyloseq object}

\item{groups}{Optional grouping variable}

\item{metrics}{Diversity metrics to calculate}

\item{include_universal}{Include universal transformation (default: TRUE)}

\item{include_consensus}{Include consensus analysis (default: TRUE)}

\item{use_cache}{Whether to use caching (default: TRUE)}

\item{force_recompute}{Force recomputation (default: FALSE)}

\item{show_progress}{Show progress bar (default: TRUE)}
}
\value{
Complete diversity analysis results
}
\description{
Complete diversity analysis with caching and progress tracking
}
