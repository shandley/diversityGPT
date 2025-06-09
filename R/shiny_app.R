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
    message("\nMissing required packages: ", paste(missing_packages, collapse = ", "))
    message("\nWould you like to install them now? (y/n)")
    
    if (interactive()) {
      response <- readline()
      if (tolower(response) == "y") {
        install.packages(missing_packages, repos = "https://cran.r-project.org")
        message("Packages installed. Launching app...")
      } else {
        stop(
          "Cannot launch app without required packages.\n",
          "Install manually with: install.packages(c('", 
          paste(missing_packages, collapse = "', '"), "'))"
        )
      }
    } else {
      stop(
        "The following packages are required but not installed:\n",
        paste(missing_packages, collapse = ", "),
        "\n\nInstall them with: install.packages(c('", 
        paste(missing_packages, collapse = "', '"), "'))"
      )
    }
  }
  
  # Get app directory
  app_dir <- system.file("shiny", "diversity_explorer", package = "diversityGPT")
  
  if (app_dir == "") {
    stop("Could not find Shiny app directory. Make sure the package is properly installed.")
  }
  
  # Launch the app
  message("Launching diversityGPT Interactive Explorer...")
  message("This may take a moment to load...")
  
  # Try main app first, fallback to simple app if needed
  tryCatch({
    shiny::runApp(
      app_dir,
      launch.browser = launch.browser,
      port = port,
      host = host
    )
  }, error = function(e) {
    warning("Main app failed to launch: ", e$message)
    message("Trying simple fallback app...")
    
    simple_app_path <- file.path(app_dir, "simple_app.R")
    if (file.exists(simple_app_path)) {
      shiny::runApp(
        simple_app_path,
        launch.browser = launch.browser,
        port = port,
        host = host
      )
    } else {
      stop("Both main and fallback apps failed to launch.")
    }
  })
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

#' Launch simple diversity explorer
#' 
#' Launch a simplified version of the explorer without complex dependencies
#' 
#' @param launch.browser Logical; should the app launch in a browser?
#' @param port The TCP port that the application should listen on.
#' @param host The IPv4 address that the application should listen on.
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' # Launch simple explorer
#' launch_simple_explorer()
#' }
launch_simple_explorer <- function(launch.browser = TRUE,
                                 port = getOption("shiny.port"),
                                 host = getOption("shiny.host", "127.0.0.1")) {
  
  # Check for basic packages
  required_packages <- c("shiny", "plotly", "DT")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  
  if (length(missing_packages) > 0) {
    stop(
      "Required packages missing: ",
      paste(missing_packages, collapse = ", "),
      "\nInstall with: install.packages(c('", 
      paste(missing_packages, collapse = "', '"), "'))"
    )
  }
  
  # Get app directory
  app_dir <- system.file("shiny", "diversity_explorer", package = "diversityGPT")
  simple_app_path <- file.path(app_dir, "simple_app.R")
  
  if (!file.exists(simple_app_path)) {
    stop("Simple app not found in package.")
  }
  
  message("Launching simple diversityGPT Explorer...")
  
  shiny::runApp(
    simple_app_path,
    launch.browser = launch.browser,
    port = port,
    host = host
  )
}

#' Launch Component Explorer
#' 
#' Interactive widget with sliders to explore R, E, P, S parameter space
#' and see how components create all diversity metrics in real-time
#' 
#' @param launch.browser Logical; should the app launch in a browser?
#' @param port The TCP port that the application should listen on.
#' @param host The IPv4 address that the application should listen on.
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' # Launch component explorer
#' launch_component_explorer()
#' }
launch_component_explorer <- function(launch.browser = TRUE,
                                    port = getOption("shiny.port"),
                                    host = getOption("shiny.host", "127.0.0.1")) {
  
  # Check for required packages
  required_packages <- c("shiny", "plotly")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  
  if (length(missing_packages) > 0) {
    stop(
      "Required packages missing: ",
      paste(missing_packages, collapse = ", "),
      "\nInstall with: install.packages(c('", 
      paste(missing_packages, collapse = "', '"), "'))"
    )
  }
  
  # Get app directory
  app_dir <- system.file("shiny", "diversity_explorer", package = "diversityGPT")
  component_app_path <- file.path(app_dir, "component_explorer.R")
  
  if (!file.exists(component_app_path)) {
    stop("Component explorer not found in package.")
  }
  
  message("Launching diversityGPT Component Explorer...")
  message("🎛️  Interactive R, E, P, S parameter space exploration")
  
  shiny::runApp(
    component_app_path,
    launch.browser = launch.browser,
    port = port,
    host = host
  )
}

#' Launch Enhanced Component Explorer
#' 
#' Advanced interactive widget showing mathematical relationships between
#' R, E, P, S components and diversity metrics with live equations,
#' contribution visualization, and biological interpretation
#' 
#' @param launch.browser Logical; should the app launch in a browser?
#' @param port The TCP port that the application should listen on.
#' @param host The IPv4 address that the application should listen on.
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' # Launch enhanced component explorer
#' launch_enhanced_explorer()
#' }
launch_enhanced_explorer <- function(launch.browser = TRUE,
                                   port = getOption("shiny.port"),
                                   host = getOption("shiny.host", "127.0.0.1")) {
  
  # Check for required packages
  required_packages <- c("shiny", "plotly", "DT")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  
  if (length(missing_packages) > 0) {
    stop(
      "Required packages missing: ",
      paste(missing_packages, collapse = ", "),
      "\nInstall with: install.packages(c('", 
      paste(missing_packages, collapse = "', '"), "'))"
    )
  }
  
  # Get app directory
  app_dir <- system.file("shiny", "diversity_explorer", package = "diversityGPT")
  enhanced_app_path <- file.path(app_dir, "enhanced_component_explorer.R")
  
  if (!file.exists(enhanced_app_path)) {
    stop("Enhanced component explorer not found in package.")
  }
  
  message("Launching Enhanced Component Explorer 2.0...")
  message("📐 Live mathematical equations")
  message("📊 Component contribution visualization")
  message("🔬 Real-time biological interpretation")
  message("📈 3D response surfaces and more!")
  
  shiny::runApp(
    enhanced_app_path,
    launch.browser = launch.browser,
    port = port,
    host = host
  )
}