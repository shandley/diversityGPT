# Quick testing helpers for development

# Run all tests
test_all <- function() {
  devtools::test()
}

# Test a specific function
test_function <- function(func_name) {
  test_file <- paste0("tests/testthat/test-", func_name, ".R")
  if (file.exists(test_file)) {
    testthat::test_file(test_file)
  } else {
    cli::cli_alert_danger("Test file not found: {test_file}")
    cli::cli_alert_info("Create it with: usethis::use_test('{func_name}')")
  }
}

# Run tests with coverage
test_coverage <- function() {
  covr::package_coverage()
}

# Test specific functionality
test_diversity <- function() {
  test_function("calculate_diversity")
}

test_consensus <- function() {
  test_function("consensus_diversity")
}

test_llm <- function() {
  test_function("llm_integration")
}

# Interactive testing with example data
test_interactive <- function() {
  # Load example data
  if (exists("example_physeq")) {
    cli::cli_h2("Testing with example_physeq")
    
    # Test diversity calculation
    div_results <- calculate_diversity(example_physeq)
    print(head(div_results))
    
    # Test consensus
    consensus <- consensus_diversity(div_results)
    print(consensus)
    
    cli::cli_alert_success("Interactive tests complete")
  } else {
    cli::cli_alert_warning("No example data found. Run create_example_data() first.")
  }
}

# Quick validation checks
validate_package <- function() {
  cli::cli_h1("Package Validation")
  
  # Check documentation
  cli::cli_h2("Documentation")
  devtools::check_man()
  
  # Check examples
  cli::cli_h2("Examples")
  devtools::run_examples()
  
  # Check tests
  cli::cli_h2("Tests")
  devtools::test()
  
  # Check build
  cli::cli_h2("Build")
  devtools::check()
}