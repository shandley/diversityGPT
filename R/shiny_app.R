#' Launch diversityGPT Interactive Explorer
#'
#' Opens the Shiny application for interactive diversity metric transformation
#' and exploration. The app provides a user-friendly interface for:
#' - Uploading and exploring microbiome data
#' - Extracting universal information components
#' - Transforming between any diversity metrics
#' - Creating interactive visualizations
#' - Getting AI-powered interpretations
#'
#' @param launch.browser Logical; should the app launch in a browser?
#'   Default is TRUE.
#' @param port The TCP port that the application should listen on.
#'   Default chooses a random available port.
#' @param host The IPv4 address that the application should listen on.
#'   Default is "127.0.0.1".
#'
#' @return Launches the Shiny application
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Launch the interactive explorer
#' launch_diversity_explorer()
#' 
#' # Launch on specific port
#' launch_diversity_explorer(port = 3838)
#' }
launch_diversity_explorer <- function(launch.browser = TRUE, 
                                    port = getOption("shiny.port"),
                                    host = getOption("shiny.host", "127.0.0.1")) {
  
  # Check for required packages
  required_packages <- c("shiny", "shinydashboard", "plotly", "DT")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  
  if (length(missing_packages) > 0) {
    stop(
      "The following packages are required for the Shiny app but not installed:\n",
      paste(missing_packages, collapse = ", "),
      "\n\nInstall them with: install.packages(c('", 
      paste(missing_packages, collapse = "', '"), "'))"
    )
  }
  
  # Get app directory
  app_dir <- system.file("shiny", "diversity_explorer", package = "diversityGPT")
  
  if (app_dir == "") {
    stop("Could not find Shiny app directory. Make sure the package is properly installed.")
  }
  
  # Launch the app
  message("Launching diversityGPT Interactive Explorer...")
  message("This may take a moment to load...")
  
  shiny::runApp(
    app_dir,
    launch.browser = launch.browser,
    port = port,
    host = host
  )
}

#' Launch diversity explorer with example data
#' 
#' Convenience function that pre-loads example data into the explorer
#' 
#' @param dataset Character; which example dataset to load. 
#'   Options: "GlobalPatterns", "enterotype", "soilrep"
#' @param ... Additional arguments passed to launch_diversity_explorer
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' # Launch with GlobalPatterns data
#' explore_example("GlobalPatterns")
#' }
explore_example <- function(dataset = c("GlobalPatterns", "enterotype", "soilrep"), ...) {
  dataset <- match.arg(dataset)
  
  # Set option for the app to read
  options(diversityGPT.example = dataset)
  
  # Launch the explorer
  launch_diversity_explorer(...)
}