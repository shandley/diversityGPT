% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualization_dashboards.R
\name{plot_information_components}
\alias{plot_information_components}
\title{Information Component Dashboard Visualization}
\usage{
plot_information_components(
  universal_info,
  metrics = NULL,
  plot_type = c("grid", "interactive"),
  n_cols = 2,
  color_scheme = c("default", "viridis", "RColorBrewer")
)
}
\arguments{
\item{universal_info}{A universal_information object}

\item{metrics}{Character vector: specific metrics to display (NULL for all)}

\item{plot_type}{Character: "grid" for static grid, "interactive" for plotly}

\item{n_cols}{Number of columns for grid layout (default 2)}

\item{color_scheme}{Character: color palette ("default", "viridis", "RColorBrewer")}
}
\value{
A multi-panel visualization (ggplot2 or plotly object)
}
\description{
Creates multi-panel dashboards showing the decomposition of diversity metrics
into their universal information components (R, E, P, S). Includes component
proportions, metric profiles, and quality assessments.
}
\examples{
\dontrun{
universal_info <- extract_universal_information(phyloseq_obj)

# Create information dashboard
plot_information_components(universal_info)

# Interactive version
plot_information_components(universal_info, plot_type = "interactive")
}
}
