% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_diversity.R
\name{calculate_diversity}
\alias{calculate_diversity}
\title{Calculate multiple diversity metrics}
\usage{
calculate_diversity(
  physeq,
  metrics = c("shannon", "simpson", "chao1", "observed"),
  groups = NULL
)
}
\arguments{
\item{physeq}{A phyloseq object containing the microbiome data}

\item{metrics}{Character vector of diversity metrics to calculate.
Options include: "shannon", "simpson", "invsimpson", "chao1", "observed",
"faith_pd" (requires phylogenetic tree)}

\item{groups}{Optional character string specifying the sample variable to use for grouping}
}
\value{
A data frame with samples as rows and diversity metrics as columns,
with an additional group column if specified
}
\description{
Calculates a comprehensive suite of alpha diversity metrics from a phyloseq object.
This is the core function that computes all requested diversity measures in a
consistent format, preparing them for consensus analysis and interpretation.
}
\examples{
\dontrun{
library(phyloseq)
data(GlobalPatterns)

# Calculate basic diversity metrics
div_results <- calculate_diversity(GlobalPatterns)
head(div_results)

# Calculate specific metrics with grouping
div_results <- calculate_diversity(
  GlobalPatterns,
  metrics = c("shannon", "simpson", "chao1"),
  groups = "SampleType"
)
}
}
