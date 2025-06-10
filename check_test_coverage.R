#!/usr/bin/env Rscript

# Test coverage report for diversityGPT

library(covr)
library(testthat)

cat("=== diversityGPT Test Coverage Report ===\n\n")

# Run test coverage
cat("Running test coverage analysis...\n")
cov <- package_coverage()

# Print summary
cat("\n--- Coverage Summary ---\n")
print(cov)

# Get detailed coverage by file
file_coverage <- coverage_to_list(cov)
cat("\n--- Coverage by File ---\n")
for (file in names(file_coverage)) {
  file_cov <- file_coverage[[file]]
  total_lines <- length(file_cov)
  covered_lines <- sum(!is.na(file_cov) & file_cov > 0)
  coverage_pct <- round(100 * covered_lines / total_lines, 1)
  
  cat(sprintf("%-40s %5.1f%% (%d/%d lines)\n", 
              basename(file), coverage_pct, covered_lines, total_lines))
}

# Overall coverage
overall <- percent_coverage(cov)
cat("\n--- Overall Coverage ---\n")
cat(sprintf("Total coverage: %.1f%%\n", overall))

if (overall < 95) {
  cat("\n⚠️  Coverage is below 95% target for CRAN\n")
  cat("Consider adding tests for uncovered functions\n")
} else {
  cat("\n✓ Coverage meets CRAN target (>95%)\n")
}

# Generate HTML report
cat("\nGenerating HTML coverage report...\n")
report(cov, file = "coverage_report.html", browse = FALSE)
cat("Coverage report saved to: coverage_report.html\n")

# Identify untested functions
cat("\n--- Untested Functions ---\n")
zero_cov <- zero_coverage(cov)
if (nrow(zero_cov) > 0) {
  unique_functions <- unique(zero_cov$function)
  for (func in unique_functions[1:min(10, length(unique_functions))]) {
    cat("-", func, "\n")
  }
  if (length(unique_functions) > 10) {
    cat("... and", length(unique_functions) - 10, "more\n")
  }
} else {
  cat("All functions have some test coverage!\n")
}