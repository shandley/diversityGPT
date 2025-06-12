#!/usr/bin/env Rscript

# Test script for the fixes we just made

cat("=== Testing diversityGPT Fixes ===\n\n")

# Test 1: Reinstall package
cat("1. Reinstalling package...\n")
install.packages(".", repos = NULL, type = "source", quiet = TRUE)
cat("✓ Package reinstalled\n\n")

# Test 2: Load package and create test data
cat("2. Loading package and creating test data...\n")
library(diversityGPT)
demo_data <- create_demo_phyloseq(n_samples = 30, n_taxa = 100)
cat("✓ Package loaded and demo data created\n\n")

# Test 3: Test consensus_diversity with adaptive method
cat("3. Testing consensus_diversity with 'adaptive' method...\n")
div_results <- calculate_diversity(demo_data)

tryCatch({
  consensus <- consensus_diversity(div_results, method = "adaptive")
  cat("✓ Consensus analysis with 'adaptive' method works!\n")
  cat("  Consensus value:", round(consensus$consensus_value, 3), "\n\n")
}, error = function(e) {
  cat("✗ Error:", e$message, "\n\n")
})

# Test 4: Test cached_diversity_suite
cat("4. Testing cached_diversity_suite...\n")
tryCatch({
  suite_results <- cached_diversity_suite(
    demo_data,
    groups = "Group",  # Add a group variable
    include_consensus = TRUE
  )
  cat("✓ cached_diversity_suite works!\n")
  cat("  Components:", paste(names(suite_results), collapse = ", "), "\n\n")
}, error = function(e) {
  cat("✗ Error:", e$message, "\n\n")
})

# Test 5: Test transformation (should still work)
cat("5. Testing universal transformation...\n")
result <- extract_universal_information(demo_data, include_phylogenetic = FALSE)
source_data <- result$metric_profiles[1:5, c("shannon", "observed")]

tryCatch({
  transform_result <- universal_diversity_transform(
    source_metrics = source_data,
    target_metrics = c("simpson", "chao1"),
    transformation_matrix = result$transformation_matrix
  )
  cat("✓ Transformation still works!\n")
  cat("  First predicted Simpson:", round(transform_result$predicted_metrics$simpson[1], 4), "\n\n")
}, error = function(e) {
  cat("✗ Error:", e$message, "\n\n")
})

cat("=== Fix Testing Complete ===\n")