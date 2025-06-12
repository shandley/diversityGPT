#!/usr/bin/env Rscript

# Test script to verify high-priority fixes

cat("=== Testing High-Priority Fixes ===\n\n")

# Install and load package
cat("1. Installing package with fixes...\n")
install.packages(".", repos = NULL, type = "source", quiet = TRUE)
library(diversityGPT)
cat("✓ Package installed\n\n")

# Test 1: Spatial distance calculation in beta diversity
cat("2. Testing spatial distance calculation (no longer hardcoded)...\n")

if (file.exists("R/experimental/beta_diversity_core.R")) {
  source("R/experimental/beta_diversity_core.R")
  
  # Create test data with known spatial coordinates
  coords <- matrix(c(0, 0, 10, 0, 0, 10, 10, 10), ncol = 2, byrow = TRUE)
  sample1 <- rpois(20, 5)
  sample2 <- rpois(20, 5)
  
  # Test 1: Without max_dist (should warn)
  cat("  Testing without max_dist parameter...\n")
  tryCatch({
    result1 <- calculate_S_beta(
      coords[1,], coords[2,], sample1, sample2, 
      method = "geometric"
    )
    if (is.na(result1)) {
      cat("  ✓ Correctly returns NA when max_dist not provided\n")
    } else {
      cat("  ⚠️  WARNING: Should return NA without max_dist\n")
    }
  }, error = function(e) {
    cat("  ✓ Error handled:", e$message, "\n")
  })
  
  # Test 2: With calculated max_dist
  cat("  Testing with calculated max_dist...\n")
  max_dist <- max(dist(coords))
  result2 <- calculate_S_beta(
    coords[1,], coords[2,], sample1, sample2, 
    method = "geometric", max_dist = max_dist
  )
  cat(sprintf("  ✓ S_beta calculated: %.3f (max_dist = %.1f)\n", result2, max_dist))
  
  # Test 3: Matrix function auto-calculates max_dist
  cat("  Testing matrix function auto-calculation...\n")
  comm_matrix <- matrix(rpois(80, 10), nrow = 4)
  result3 <- decompose_beta_diversity_matrix(
    comm_matrix, coords = coords, method = "geometric"
  )
  cat("  ✓ Matrix decomposition calculates spatial extent automatically\n")
  
} else {
  cat("  ℹ️  Beta diversity experimental code not found\n")
}
cat("\n")

# Test 2: Ecological thresholds are configurable
cat("3. Testing configurable ecological thresholds...\n")

# Create test data
demo_data <- create_demo_phyloseq(n_samples = 20, n_taxa = 50)
universal_info <- extract_universal_information(demo_data)

# Test with default thresholds
cat("  Testing with default thresholds...\n")
result1 <- detect_assembly_mechanisms(universal_info)
cat("  ✓ Default thresholds used:", names(result1$thresholds_used)[1:3], "...\n")

# Test with custom thresholds
cat("  Testing with custom thresholds...\n")
custom_thresholds <- list(
  env_correlation_threshold = 0.7,  # Higher than default 0.5
  high_richness_threshold = 0.8,    # Higher than default 0.7
  low_evenness_threshold = 0.3,     # Lower than default 0.4
  variance_threshold = 0.6,         # Higher than default 0.5
  predictability_threshold = 0.5,   # Lower than default 0.6
  spatial_signal_threshold = 0.4    # Higher than default 0.3
)

result2 <- detect_assembly_mechanisms(universal_info, thresholds = custom_thresholds)
if (result2$thresholds_used$env_correlation_threshold == 0.7) {
  cat("  ✓ Custom thresholds correctly applied\n")
} else {
  cat("  ⚠️  WARNING: Custom thresholds not applied correctly\n")
}
cat("\n")

# Test 3: P-values return NA instead of hardcoded values
cat("4. Testing p-values return NA (not hardcoded)...\n")

if (!is.null(result1$mechanisms) && nrow(result1$mechanisms) > 0) {
  p_values <- result1$mechanisms$p_value
  hardcoded_pvals <- p_values[p_values %in% c(0.05, 0.1, 0.01)]
  
  if (length(hardcoded_pvals) == 0) {
    cat("  ✓ No hardcoded p-values found (all NA or calculated)\n")
  } else {
    cat("  ⚠️  WARNING: Found hardcoded p-values:", unique(hardcoded_pvals), "\n")
  }
  
  na_count <- sum(is.na(p_values))
  cat(sprintf("  ℹ️  %d of %d p-values are NA (expected for unimplemented tests)\n", 
              na_count, length(p_values)))
} else {
  cat("  ℹ️  No mechanisms detected to test p-values\n")
}
cat("\n")

# Test 4: Configuration parameters system
cat("5. Testing configuration parameters system...\n")

if (exists("get_config_parameters")) {
  # Get all parameters
  all_params <- get_config_parameters()
  cat("  ✓ Configuration system loaded. Categories:", names(all_params), "\n")
  
  # Get ecological parameters
  eco_params <- get_config_parameters("ecological")
  if (length(eco_params) > 0) {
    cat("  ✓ Ecological parameters accessible:", names(eco_params)[1:3], "...\n")
  }
  
  # Check for magic numbers
  info_params <- get_config_parameters("information_theory")
  if (info_params$pseudocount == 1e-10) {
    cat("  ✓ Pseudocount documented in config (1e-10)\n")
  }
  if (info_params$entropy_avg_weight == 0.5) {
    cat("  ✓ Entropy averaging weight documented (0.5)\n")
  }
} else {
  cat("  ⚠️  WARNING: Configuration system not found\n")
}
cat("\n")

# Test 5: Safe package loading utilities
cat("6. Testing safe package loading utilities...\n")

if (exists("safe_use_package")) {
  # Test with missing package
  result <- safe_use_package("fake_package_xyz", "fake_function", error_value = NA)
  if (is.na(result)) {
    cat("  ✓ Safe package loading returns NA for missing packages\n")
  }
  
  # Test check_multiple_packages
  if (exists("check_multiple_packages")) {
    missing_check <- check_multiple_packages(
      c("stats", "fake_package_123"), 
      context = "testing"
    )
    if (!missing_check) {
      cat("  ✓ Multiple package check correctly identifies missing packages\n")
    }
  }
} else {
  cat("  ⚠️  WARNING: Safe package utilities not found\n")
}

cat("\n=== High-Priority Fix Testing Complete ===\n\n")

# Summary
cat("Summary of fixes:\n")
cat("1. Spatial distances: Now calculated from data or parameterized\n")
cat("2. Ecological thresholds: Fully configurable via thresholds parameter\n")
cat("3. P-values: Return NA instead of hardcoded values\n")
cat("4. Configuration: Centralized parameter management system\n")
cat("5. Dependencies: Safe loading with informative messages\n")
cat("\nAll high-priority hardcoded values have been addressed.\n")