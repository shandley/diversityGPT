#' Safe Package Loading Utilities
#'
#' @description
#' Internal utilities for safely checking and using optional packages
#' with appropriate error messages.
#'
#' @name utils_dependencies
#' @keywords internal
NULL

#' Safely Use Optional Package
#'
#' @description
#' Wrapper to safely use functions from optional packages with
#' informative error messages when packages are missing.
#'
#' @param package Package name as string
#' @param func Function name as string
#' @param ... Arguments to pass to the function
#' @param error_value Value to return if package is missing
#' @param message Custom error message (optional)
#'
#' @return Result of function call or error_value if package missing
#' @keywords internal
safe_use_package <- function(package, func, ..., error_value = NULL, message = NULL) {
  
  if (!requireNamespace(package, quietly = TRUE)) {
    if (is.null(message)) {
      message <- sprintf(
        "Package '%s' is required for %s. Install with: install.packages('%s')",
        package, func, package
      )
    }
    cli::cli_warn(message)
    return(error_value)
  }
  
  # Get the function
  pkg_func <- utils::getFromNamespace(func, package)
  
  # Call it with error handling
  tryCatch(
    pkg_func(...),
    error = function(e) {
      cli::cli_warn("Error calling {package}::{func}: {e$message}")
      return(error_value)
    }
  )
}

#' Check Multiple Package Dependencies
#'
#' @description
#' Check if multiple packages are available and provide installation
#' instructions if not.
#'
#' @param packages Character vector of package names
#' @param context Description of what the packages are needed for
#'
#' @return Logical indicating if all packages are available
#' @keywords internal
check_multiple_packages <- function(packages, context = "this analysis") {
  
  missing <- character()
  
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      missing <- c(missing, pkg)
    }
  }
  
  if (length(missing) > 0) {
    cli::cli_alert_warning(
      "The following packages are required for {context} but not installed: {missing}"
    )
    
    if (length(missing) == 1) {
      cli::cli_alert_info("Install with: install.packages('{missing}')")
    } else {
      cli::cli_alert_info(
        "Install with: install.packages(c({paste0('\"', missing, '\"', collapse = ', ')}))"
      )
    }
    
    return(FALSE)
  }
  
  return(TRUE)
}

#' Create NA Result with Warning
#'
#' @description
#' Helper to create consistent NA results with appropriate warnings
#' when calculations cannot be performed.
#'
#' @param n Length of NA vector to create
#' @param reason Reason for returning NA
#' @param suggestion What user should do
#'
#' @return Vector of NA values
#' @keywords internal
create_na_result <- function(n, reason, suggestion = NULL) {
  
  cli::cli_alert_warning(reason)
  
  if (!is.null(suggestion)) {
    cli::cli_alert_info(suggestion)
  }
  
  return(rep(NA_real_, n))
}

#' Get Package Function Safely
#'
#' @description
#' Get a function from a package namespace safely, returning NULL
#' if package is not available.
#'
#' @param package Package name
#' @param func Function name
#'
#' @return Function or NULL
#' @keywords internal
get_package_function <- function(package, func) {
  
  if (!requireNamespace(package, quietly = TRUE)) {
    return(NULL)
  }
  
  tryCatch(
    utils::getFromNamespace(func, package),
    error = function(e) NULL
  )
}