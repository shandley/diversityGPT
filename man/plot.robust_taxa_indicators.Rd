% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/robust_taxa_indicators.R
\name{plot.robust_taxa_indicators}
\alias{plot.robust_taxa_indicators}
\title{Plot Robust Taxa Indicators}
\usage{
\method{plot}{robust_taxa_indicators}(
  x,
  type = c("consensus", "comparison", "reliability", "summary"),
  component = "richness",
  top_n = 10,
  interactive = FALSE,
  ...
)
}
\arguments{
\item{x}{A robust_taxa_indicators object}

\item{type}{Plot type: "consensus", "comparison", "reliability", or "summary"}

\item{component}{Component to visualize (default: "richness")}

\item{top_n}{Number of top taxa to show}

\item{interactive}{Create interactive plot using plotly}

\item{...}{Additional arguments passed to plotting functions}
}
\value{
A ggplot2 or plotly object
}
\description{
Creates visualizations of robust indicator analysis results
}
