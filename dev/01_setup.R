# Run this script once to set up your development environment

# Install development dependencies if needed
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

if (!requireNamespace("usethis", quietly = TRUE)) {
  install.packages("usethis")
}

if (!requireNamespace("testthat", quietly = TRUE)) {
  install.packages("testthat")
}

if (!requireNamespace("roxygen2", quietly = TRUE)) {
  install.packages("roxygen2")
}

# Install package dependencies
devtools::install_deps()

# Set up git hooks for automated checks (optional)
if (file.exists(".git")) {
  usethis::use_git_hook(
    "pre-commit",
    script = '#!/bin/sh
# Run tests before committing
Rscript -e "devtools::test()" || exit 1
'
  )
}

# Create .Renviron file if it doesn't exist
if (!file.exists(".Renviron")) {
  usethis::edit_r_environ()
  cli::cli_alert_info("Add your API keys to .Renviron:")
  cli::cli_alert_info("ANTHROPIC_API_KEY=your-key-here")
  cli::cli_alert_info("OPENAI_API_KEY=your-key-here")
}

# Create .Rbuildignore entries
usethis::use_build_ignore(c("dev", "^.*\\.Rproj$", "^\\.Rproj\\.user$", "data-raw"))

cli::cli_alert_success("Development environment setup complete!")
cli::cli_alert_info("Next: Run source('dev/02_load.R') to start developing")