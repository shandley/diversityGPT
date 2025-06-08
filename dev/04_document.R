# Documentation helpers

# Update all documentation
document_all <- function() {
  devtools::document()
  cli::cli_alert_success("Documentation updated")
}

# Create a new function with documentation template
create_function <- function(name, file = NULL) {
  if (is.null(file)) {
    file <- name
  }
  
  # Create R file
  usethis::use_r(file)
  
  # Create test file
  usethis::use_test(name)
  
  cli::cli_alert_success("Created R/{file}.R and tests/testthat/test-{name}.R")
}

# Build README
build_readme <- function() {
  devtools::build_readme()
  cli::cli_alert_success("README updated")
}

# Build package website (if using pkgdown)
build_site <- function() {
  if (requireNamespace("pkgdown", quietly = TRUE)) {
    pkgdown::build_site()
  } else {
    cli::cli_alert_info("Install pkgdown to build package website")
  }
}

# Check documentation coverage
check_docs <- function() {
  # Get all exported functions
  exports <- getNamespaceExports("diversityGPT")
  
  # Check which have documentation
  documented <- list.files("man", pattern = "\\.Rd$")
  documented <- gsub("\\.Rd$", "", documented)
  
  # Find undocumented functions
  undocumented <- setdiff(exports, documented)
  
  if (length(undocumented) > 0) {
    cli::cli_alert_warning("Undocumented functions:")
    cli::cli_ul(undocumented)
  } else {
    cli::cli_alert_success("All exported functions are documented!")
  }
}

# Create vignette
create_vignette <- function(name) {
  usethis::use_vignette(name)
  cli::cli_alert_success("Created vignette: vignettes/{name}.Rmd")
}