#' Check and Install Package Dependencies
#'
#' @description
#' Checks for required and suggested packages, providing installation
#' guidance for missing dependencies. This ensures full functionality
#' of the diversityGPT package.
#'
#' @param install_missing Logical, whether to offer to install missing packages
#' @param check_suggested Logical, whether to check suggested packages too
#' @param verbose Logical, whether to print detailed messages
#'
#' @return Invisible list with status of all dependencies
#' @export
#' @examples
#' # Check dependencies
#' check_dependencies()
#' 
#' # Check and offer to install missing packages
#' check_dependencies(install_missing = TRUE)
#' 
#' # Check only required packages silently
#' status <- check_dependencies(check_suggested = FALSE, verbose = FALSE)
check_dependencies <- function(install_missing = FALSE, 
                             check_suggested = TRUE,
                             verbose = TRUE) {
  
  # Define critical packages for full functionality
  critical_packages <- list(
    picante = "Faith's phylogenetic diversity, UniFrac distances, phylogenetic null models",
    ape = "Phylogenetic tree manipulation and visualization",
    FD = "Functional diversity calculations",
    hilldiv = "Hill numbers and diversity profiles",
    mixOmics = "Advanced multivariate analysis",
    FactoMineR = "PCA and factor analysis",
    MCMCpack = "Bayesian statistical methods",
    boot = "Bootstrap confidence intervals",
    progress = "Progress bars for long computations"
  )
  
  # Core required packages (from DESCRIPTION Imports)
  required_packages <- c(
    "phyloseq", "vegan", "stats", "httr2", "jsonlite", 
    "cli", "rlang", "digest", "utils", "methods",
    "graphics", "grDevices", "ggplot2", "plotly", "igraph"
  )
  
  # Suggested packages for extended functionality
  suggested_packages <- c(
    "knitr", "rmarkdown", "testthat", "mockery", "withr",
    "shiny", "shinydashboard", "DT", "networkD3", "biomformat",
    "reshape2", "dplyr", "tidyr", "parallel", "base64enc", 
    "callr", "ape", "Matrix"
  )
  
  # Initialize results
  results <- list(
    required = list(),
    critical = list(),
    suggested = list(),
    all_good = TRUE
  )
  
  if (verbose) {
    cli::cli_h1("diversityGPT Dependency Check")
  }
  
  # Check required packages
  if (verbose) cli::cli_h2("Required Packages")
  
  for (pkg in required_packages) {
    installed <- requireNamespace(pkg, quietly = TRUE)
    results$required[[pkg]] <- installed
    
    if (!installed) {
      results$all_good <- FALSE
      if (verbose) {
        cli::cli_alert_danger("{pkg} is REQUIRED but not installed")
      }
    } else if (verbose) {
      cli::cli_alert_success("{pkg} is installed")
    }
  }
  
  # Check critical packages for full functionality
  if (verbose) cli::cli_h2("Critical Packages for Full Functionality")
  
  for (pkg in names(critical_packages)) {
    installed <- requireNamespace(pkg, quietly = TRUE)
    results$critical[[pkg]] <- installed
    
    if (!installed) {
      if (verbose) {
        cli::cli_alert_warning("{pkg} is not installed")
        cli::cli_alert_info("  Needed for: {critical_packages[[pkg]]}")
      }
    } else if (verbose) {
      cli::cli_alert_success("{pkg} is installed")
    }
  }
  
  # Check suggested packages
  if (check_suggested) {
    if (verbose) cli::cli_h2("Suggested Packages")
    
    for (pkg in suggested_packages) {
      installed <- requireNamespace(pkg, quietly = TRUE)
      results$suggested[[pkg]] <- installed
      
      if (verbose && !installed) {
        cli::cli_alert_info("{pkg} is suggested but not installed")
      }
    }
  }
  
  # Summary
  missing_required <- names(which(!unlist(results$required)))
  missing_critical <- names(which(!unlist(results$critical)))
  missing_suggested <- names(which(!unlist(results$suggested)))
  
  if (verbose) {
    cli::cli_h2("Summary")
    
    if (length(missing_required) == 0) {
      cli::cli_alert_success("All required packages are installed")
    } else {
      cli::cli_alert_danger("{length(missing_required)} required packages missing")
    }
    
    if (length(missing_critical) == 0) {
      cli::cli_alert_success("All critical packages for full functionality are installed")
    } else {
      cli::cli_alert_warning("{length(missing_critical)} critical packages missing - some features will be limited")
    }
    
    if (check_suggested && length(missing_suggested) > 0) {
      cli::cli_alert_info("{length(missing_suggested)} suggested packages not installed")
    }
  }
  
  # Offer to install missing packages
  if (install_missing && (length(missing_required) > 0 || length(missing_critical) > 0)) {
    
    if (interactive()) {
      cli::cli_h2("Installation Options")
      
      # Required packages must be installed
      if (length(missing_required) > 0) {
        cli::cli_alert_danger("Required packages must be installed for basic functionality:")
        cat(paste("  -", missing_required), sep = "\n")
        
        response <- readline("Install required packages now? (y/n): ")
        if (tolower(response) == "y") {
          install.packages(missing_required)
        }
      }
      
      # Critical packages
      if (length(missing_critical) > 0) {
        cli::cli_alert_warning("Critical packages needed for full functionality:")
        for (pkg in missing_critical) {
          cat(sprintf("  - %s: %s\n", pkg, critical_packages[[pkg]]))
        }
        
        response <- readline("Install critical packages now? (y/n): ")
        if (tolower(response) == "y") {
          install.packages(missing_critical)
        }
      }
      
    } else {
      # Non-interactive mode - provide installation commands
      if (length(missing_required) > 0) {
        cli::cli_alert("To install required packages, run:")
        cli::cli_code('install.packages(c({paste0(\'"\', missing_required, \'"\', collapse = ", ")}))')
      }
      
      if (length(missing_critical) > 0) {
        cli::cli_alert("To install critical packages for full functionality, run:")
        cli::cli_code('install.packages(c({paste0(\'"\', missing_critical, \'"\', collapse = ", ")}))')
      }
    }
  }
  
  # Add recommendation for picante specifically
  if (!"picante" %in% names(which(unlist(results$critical)))) {
    if (verbose) {
      cli::cli_h2("Special Note on picante Package")
      cli::cli_alert_warning("The 'picante' package is STRONGLY RECOMMENDED for:")
      cli::cli_bullets(c(
        "Faith's Phylogenetic Diversity calculations",
        "UniFrac distance calculations",
        "Phylogenetic null models",
        "Community phylogenetic structure analysis"
      ))
      cli::cli_alert_info("Without picante, these analyses will return NA values")
      cli::cli_code('install.packages("picante")')
    }
  }
  
  invisible(results)
}

#' Check if Critical Function Dependencies are Available
#'
#' @description
#' Internal function to check if dependencies for specific functions
#' are available, with informative messages.
#'
#' @param func_name Name of the function being called
#' @return Logical indicating if dependencies are met
#' @keywords internal
check_function_dependencies <- function(func_name) {
  
  deps <- switch(func_name,
    "faith_pd" = list(
      packages = "picante",
      message = "Faith's phylogenetic diversity requires the 'picante' package.",
      install = 'install.packages("picante")'
    ),
    "unifrac" = list(
      packages = "picante",
      message = "UniFrac distance requires the 'picante' package.",
      install = 'install.packages("picante")'
    ),
    "null_model_phylo" = list(
      packages = c("picante", "ape"),
      message = "Phylogenetic null models require 'picante' and 'ape' packages.",
      install = 'install.packages(c("picante", "ape"))'
    ),
    "functional_diversity" = list(
      packages = "FD",
      message = "Functional diversity calculations require the 'FD' package.",
      install = 'install.packages("FD")'
    ),
    NULL
  )
  
  if (is.null(deps)) return(TRUE)
  
  # Check if all required packages are available
  all_available <- all(sapply(deps$packages, requireNamespace, quietly = TRUE))
  
  if (!all_available) {
    cli::cli_alert_warning(deps$message)
    cli::cli_alert_info("To install, run: {deps$install}")
    cli::cli_alert_info("This function will return NA values until dependencies are installed.")
  }
  
  return(all_available)
}

#' Setup diversityGPT for First Time Use
#'
#' @description
#' One-time setup function that checks dependencies, installs critical
#' packages, and verifies the installation.
#'
#' @param install_all Logical, whether to install all recommended packages
#' @export
#' @examples
#' \dontrun{
#' # Run after installing diversityGPT
#' setup_diversitygpt()
#' }
setup_diversitygpt <- function(install_all = FALSE) {
  
  cli::cli_h1("Setting up diversityGPT")
  
  # Check R version
  r_version <- getRversion()
  if (r_version < "4.1.0") {
    cli::cli_alert_danger("R version {r_version} detected. diversityGPT requires R >= 4.1.0")
    cli::cli_alert_info("Please update R before proceeding")
    return(invisible(FALSE))
  } else {
    cli::cli_alert_success("R version {r_version} meets requirements")
  }
  
  # Run dependency check
  dep_status <- check_dependencies(install_missing = TRUE, verbose = TRUE)
  
  # If install_all requested, install everything
  if (install_all && interactive()) {
    all_missing <- c(
      names(which(!unlist(dep_status$required))),
      names(which(!unlist(dep_status$critical))),
      names(which(!unlist(dep_status$suggested)))
    )
    
    if (length(all_missing) > 0) {
      cli::cli_alert_info("Installing all {length(all_missing)} missing packages...")
      install.packages(all_missing)
    }
  }
  
  # Test basic functionality
  cli::cli_h2("Testing Basic Functionality")
  
  tryCatch({
    # Create demo data
    demo_data <- create_demo_phyloseq(n_samples = 10, n_taxa = 50)
    cli::cli_alert_success("Demo data creation works")
    
    # Test diversity calculation
    div <- calculate_diversity(demo_data, metrics = c("shannon", "simpson"))
    cli::cli_alert_success("Basic diversity calculations work")
    
    # Test universal information
    if (requireNamespace("picante", quietly = TRUE)) {
      info <- extract_universal_information(demo_data)
      cli::cli_alert_success("Universal information extraction works")
    } else {
      cli::cli_alert_warning("Universal information extraction limited without picante")
    }
    
    cli::cli_alert_success("diversityGPT is ready to use!")
    
  }, error = function(e) {
    cli::cli_alert_danger("Setup test failed: {e$message}")
    return(invisible(FALSE))
  })
  
  # Final recommendations
  cli::cli_h2("Recommendations")
  cli::cli_bullets(c(
    "!" = "Install 'picante' for phylogenetic diversity: install.packages('picante')",
    "i" = "Run check_dependencies() anytime to verify your setup",
    ">" = "See vignette('getting-started', package = 'diversityGPT') for tutorials"
  ))
  
  invisible(TRUE)
}

# Add to package startup message
.onAttach <- function(libname, pkgname) {
  # Check for critical missing dependencies
  if (!requireNamespace("picante", quietly = TRUE)) {
    packageStartupMessage(
      "Note: The 'picante' package is recommended for full functionality.\n",
      "Install with: install.packages('picante')\n",
      "Run check_dependencies() to see all recommendations."
    )
  }
}