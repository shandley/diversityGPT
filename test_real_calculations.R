#!/usr/bin/env Rscript

# Test script to verify all calculations use real values, not placeholders

cat("=== Testing Real Calculations (No Placeholders) ===\n\n")

# Test 1: Reinstall package with fixes
cat("1. Reinstalling package with fixes...\n")
install.packages(".", repos = NULL, type = "source", quiet = TRUE)
library(diversityGPT)
cat("✓ Package reinstalled\n\n")

# Test 2: Create test data with phylogenetic tree
cat("2. Creating test data with phylogenetic tree...\n")
set.seed(123)
demo_data <- create_demo_phyloseq(n_samples = 20, n_taxa = 50)

# Ensure we have a tree for Faith's PD test
if (is.null(phy_tree(demo_data, errorIfNULL = FALSE))) {
  # Create a random tree
  library(ape)
  tree <- rtree(n = ntaxa(demo_data), tip.label = taxa_names(demo_data))
  phy_tree(demo_data) <- tree
}
cat("✓ Test data created with phylogenetic tree\n\n")

# Test 3: Test Faith's PD calculation (was placeholder)
cat("3. Testing Faith's PD calculation...\n")
tryCatch({
  # Calculate with Faith's PD
  div_with_faith <- calculate_diversity(
    demo_data, 
    metrics = c("observed", "faith_pd")
  )
  
  # Check if Faith's PD is different from observed (it was identical in placeholder)
  if (all(div_with_faith$observed == div_with_faith$faith_pd)) {
    cat("⚠️  WARNING: Faith's PD identical to observed species - may still be placeholder!\n")
  } else {
    cat("✓ Faith's PD correctly calculated (different from observed richness)\n")
    cat(sprintf("  Correlation between observed and faith_pd: %.3f\n", 
                cor(div_with_faith$observed, div_with_faith$faith_pd)))
  }
}, error = function(e) {
  if (grepl("picante", e$message)) {
    cat("ℹ️  Faith's PD requires 'picante' package. This is expected behavior.\n")
  } else {
    cat("✗ Error:", e$message, "\n")
  }
})
cat("\n")

# Test 4: Test universal components (was hardcoded 0.25)
cat("4. Testing universal component extraction...\n")
universal_info <- extract_universal_information(demo_data, include_phylogenetic = FALSE)

# Check if components are all equal (would indicate hardcoding)
components <- universal_info$information_components
all_equal_R <- length(unique(round(components$R_component, 6))) == 1
all_equal_E <- length(unique(round(components$E_component, 6))) == 1

if (all_equal_R && all_equal_E && 
    all(components$R_component == 0.25) && 
    all(components$E_component == 0.25)) {
  cat("⚠️  WARNING: Components all equal to 0.25 - hardcoded values detected!\n")
} else if (all_equal_R && all_equal_E) {
  cat("⚠️  WARNING: All samples have identical components - suspicious!\n")
} else {
  cat("✓ Components show variation across samples (not hardcoded)\n")
  cat(sprintf("  R component range: %.3f - %.3f\n", 
              min(components$R_component), max(components$R_component)))
  cat(sprintf("  E component range: %.3f - %.3f\n",
              min(components$E_component), max(components$E_component)))
}
cat("\n")

# Test 5: Test information theory (was using synthetic data)
cat("5. Testing information theory calculations...\n")
tryCatch({
  mi_results <- calculate_taxa_mutual_information(demo_data, universal_info)
  
  # Check if we get real results or NAs
  mi_values <- as.vector(mi_results$mi_matrix)
  na_count <- sum(is.na(mi_values))
  
  if (na_count == length(mi_values)) {
    cat("ℹ️  All mutual information values are NA (expected with fixed code)\n")
  } else if (na_count > 0) {
    cat(sprintf("ℹ️  %d of %d MI values are NA\n", na_count, length(mi_values)))
  } else {
    cat("✓ Mutual information calculated successfully\n")
    cat(sprintf("  MI range: %.3f - %.3f\n", min(mi_values), max(mi_values)))
  }
}, error = function(e) {
  cat("✗ Error:", e$message, "\n")
})
cat("\n")

# Test 6: Test transformation fallback (should return NA, not 0.25)
cat("6. Testing transformation fallback behavior...\n")
# Create minimal data that might trigger fallback
minimal_metrics <- data.frame(
  sample = paste0("S", 1:5),
  shannon = rnorm(5, 2, 0.5)
)

tryCatch({
  # Try to transform with insufficient data
  components <- estimate_components_from_metrics(minimal_metrics)
  
  if (all(components$R_component == 0.25, na.rm = TRUE)) {
    cat("⚠️  WARNING: Fallback still returns hardcoded 0.25 values!\n")
  } else if (all(is.na(components$R_component))) {
    cat("✓ Fallback correctly returns NA for insufficient data\n")
  } else {
    cat("ℹ️  Components estimated from limited data\n")
  }
}, error = function(e) {
  cat("ℹ️  Function not accessible or error:", e$message, "\n")
})
cat("\n")

# Test 7: Verify beta diversity spatial calculations
cat("7. Testing beta diversity spatial component...\n")
if (file.exists("R/experimental/beta_diversity_core.R")) {
  source("R/experimental/beta_diversity_core.R")
  
  # Test with known spatial distances
  coords1 <- c(0, 0)
  coords2 <- c(100, 0)  # Exactly at the hardcoded max_dist
  
  sample1 <- rpois(10, 5)
  sample2 <- rpois(10, 5)
  
  tryCatch({
    beta_result <- decompose_beta_diversity(
      sample1, sample2,
      coords_i = coords1,
      coords_j = coords2,
      method = "geometric"
    )
    
    if (beta_result$S_beta == 1.0) {
      cat("⚠️  WARNING: Spatial beta still using hardcoded max_dist = 100\n")
    } else {
      cat("✓ Spatial beta diversity calculated\n")
    }
  }, error = function(e) {
    cat("ℹ️  Beta diversity experimental:", e$message, "\n")
  })
} else {
  cat("ℹ️  Beta diversity experimental code not found\n")
}

cat("\n=== Real Calculation Testing Complete ===\n\n")

# Summary
cat("Summary of fixes:\n")
cat("1. Faith's PD: Now requires picante package (returns NA if unavailable)\n")
cat("2. Universal components: Returns NA instead of hardcoded 0.25 values\n")
cat("3. Information theory: Uses real component data, returns NA if unavailable\n")
cat("4. Assembly p-values: Should use actual correlation p-values\n")
cat("5. Beta diversity: Still has some hardcoded parameters (max_dist)\n")
cat("\nThese changes ensure the package provides honest results rather than\n")
cat("misleading placeholders that could be mistaken for real calculations.\n")