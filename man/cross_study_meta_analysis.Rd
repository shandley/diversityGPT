% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/meta_analysis.R
\name{cross_study_meta_analysis}
\alias{cross_study_meta_analysis}
\title{Meta-Analysis Tools for diversityGPT}
\usage{
cross_study_meta_analysis(
  study_list,
  study_names = NULL,
  standardize_metrics = TRUE,
  normalization_method = "z_score",
  meta_analysis_method = "random_effects",
  study_weights = NULL,
  include_forest_plots = TRUE,
  include_heterogeneity = TRUE,
  confidence_level = 0.95,
  bootstrap_n = 1000
)
}
\arguments{
\item{study_list}{List of phyloseq objects or universal_information objects}

\item{study_names}{Optional names for studies (default: Study1, Study2, ...)}

\item{standardize_metrics}{Whether to standardize metrics across studies}

\item{normalization_method}{Method for cross-study normalization: "z_score", "min_max", "quantile"}

\item{meta_analysis_method}{Method for meta-analysis: "fixed_effects", "random_effects", "weighted"}

\item{study_weights}{Optional weights for each study in meta-analysis}

\item{include_forest_plots}{Whether to generate forest plots}

\item{include_heterogeneity}{Whether to assess between-study heterogeneity}

\item{confidence_level}{Confidence level for meta-analysis (default: 0.95)}

\item{bootstrap_n}{Number of bootstrap iterations for confidence intervals}
}
\value{
A list containing:
\item{study_summaries}{Summary statistics for each study}
\item{standardized_data}{Standardized diversity metrics across studies}
\item{meta_analysis_results}{Meta-analysis results for each metric}
\item{heterogeneity_assessment}{Between-study heterogeneity measures}
\item{forest_plots}{Forest plot data for visualization}
\item{cross_study_patterns}{Patterns identified across studies}
\item{recommendations}{Recommendations for interpretation}
}
\description{
Functions for comparing diversity analyses across multiple studies,
standardizing metrics between datasets, and performing meta-analyses
of universal diversity patterns.
Cross-Study Diversity Meta-Analysis
}
\details{
Performs comprehensive meta-analysis of diversity patterns across multiple
studies using the universal information framework for standardization.
}
\examples{
\dontrun{
# Load multiple datasets
data(GlobalPatterns)
study1 <- create_demo_subset(GlobalPatterns, n_samples = 20, seed = 123)
study2 <- create_demo_subset(GlobalPatterns, n_samples = 25, seed = 456)
study3 <- create_demo_subset(GlobalPatterns, n_samples = 15, seed = 789)

# Perform meta-analysis
meta_results <- cross_study_meta_analysis(
  study_list = list(study1, study2, study3),
  study_names = c("Marine", "Soil", "Gut"),
  standardize_metrics = TRUE,
  meta_analysis_method = "random_effects",
  include_forest_plots = TRUE
)

print(meta_results)
plot(meta_results)

# Generate meta-analysis report
generate_meta_analysis_report(meta_results, "multi_study_analysis")
}

}
