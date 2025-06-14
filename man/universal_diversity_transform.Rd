% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/universal_transformations.R
\name{universal_diversity_transform}
\alias{universal_diversity_transform}
\title{Universal Diversity Metric Transformations}
\usage{
universal_diversity_transform(
  source_metrics,
  target_metrics,
  transformation_matrix,
  method = c("linear", "nonlinear", "ensemble"),
  quality_threshold = 0.6
)
}
\arguments{
\item{source_metrics}{Named vector or data.frame of available diversity metrics}

\item{target_metrics}{Character vector of desired metrics to predict/transform}

\item{transformation_matrix}{Transformation matrix from extract_universal_information()}

\item{method}{Character: transformation method ("linear", "nonlinear", "ensemble")}

\item{quality_threshold}{Numeric: minimum R² threshold for reliable transformations}
}
\value{
A list containing:
\itemize{
\item predicted_metrics: Data.frame of predicted metric values
\item transformation_quality: Quality assessment for each prediction
\item information_components: Estimated R, E, P, S components
\item confidence_intervals: Bootstrap confidence intervals
}
}
\description{
Revolutionary any-to-any diversity metric transformation system. Uses
universal information components (R, E, P, S) to mathematically convert
between ANY diversity metrics. Enables prediction of missing metrics,
cross-study standardization, and relationship discovery.
}
\examples{
# Create demo data and extract universal information
demo_data <- create_demo_phyloseq(n_samples = 10, n_taxa = 30)
universal_info <- extract_universal_information(demo_data, 
                                              include_phylogenetic = FALSE)

# Transform Shannon to Simpson
source_metrics <- data.frame(
  shannon = c(2.1, 2.3, 1.9),
  row.names = c("Sample1", "Sample2", "Sample3")
)

transformed <- universal_diversity_transform(
  source_metrics = source_metrics,
  target_metrics = c("simpson"),
  transformation_matrix = universal_info$transformation_matrix
)
}
