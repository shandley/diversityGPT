% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shiny_app.R
\name{launch_simple_explorer}
\alias{launch_simple_explorer}
\title{Launch simple diversity explorer}
\usage{
launch_simple_explorer(
  launch.browser = TRUE,
  port = getOption("shiny.port"),
  host = getOption("shiny.host", "127.0.0.1")
)
}
\arguments{
\item{launch.browser}{Logical; should the app launch in a browser?}

\item{port}{The TCP port that the application should listen on.}

\item{host}{The IPv4 address that the application should listen on.}
}
\description{
Launch a simplified version of the explorer without complex dependencies
}
\examples{
\dontrun{
# Launch simple explorer
launch_simple_explorer()
}
}
