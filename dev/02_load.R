# Development loading script - source this at the start of each session

# Load all development packages
library(devtools)
library(testthat)
library(usethis)

# Load the package
devtools::load_all()

# Set options for nicer printing
options(
  width = 80,
  scipen = 999,
  max.print = 100
)

# Check API keys are set
check_api_keys <- function() {
  anthropic_key <- Sys.getenv("ANTHROPIC_API_KEY")
  openai_key <- Sys.getenv("OPENAI_API_KEY")
  
  if (anthropic_key == "") {
    cli::cli_alert_warning("ANTHROPIC_API_KEY not set in .Renviron")
  } else {
    cli::cli_alert_success("Anthropic API key configured")
  }
  
  if (openai_key == "") {
    cli::cli_alert_warning("OPENAI_API_KEY not set in .Renviron")
  } else {
    cli::cli_alert_success("OpenAI API key configured")
  }
}

# Run checks
check_api_keys()

# Quick shortcuts
test_file <- function(name) {
  devtools::test_active_file(paste0("tests/testthat/test-", name, ".R"))
}

check_function <- function(name) {
  devtools::check_man(paste0("man/", name, ".Rd"))
}

# Display helpful shortcuts
cli::cli_h1("diversityGPT Development Session")
cli::cli_alert_info("Shortcuts available:")
cli::cli_ul(c(
  "load_all() - Reload package",
  "test() - Run all tests",
  "test_file('name') - Test specific file",
  "check() - Full R CMD check",
  "document() - Update documentation",
  "build() - Build package"
))

# Load example data if available
if (file.exists("data/example_physeq.rda")) {
  cli::cli_alert_success("Example data loaded: example_physeq")
}