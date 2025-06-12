#!/usr/bin/env Rscript

# Test script for p-value formatting fixes

cat("=== Testing P-value Formatting Fix ===\n\n")

# Test 1: Reinstall package
cat("1. Reinstalling package...\n")
install.packages(".", repos = NULL, type = "source", quiet = TRUE)
cat("✓ Package reinstalled\n\n")

# Test 2: Load package and create test data with strong group differences
cat("2. Creating test data with strong group differences...\n")
library(diversityGPT)

# Create demo data with extreme group differences
set.seed(42)
demo_data <- create_demo_phyloseq(n_samples = 50, n_taxa = 200)

# Create strong group differences
sample_data(demo_data)$Group <- rep(c("A", "B"), each = 25)

# Manipulate abundances to create extreme differences
otu_mat <- as.matrix(otu_table(demo_data))
if (taxa_are_rows(demo_data)) {
  # Group A: Low richness, high evenness (few taxa, evenly distributed)
  otu_mat[, 1:25] <- 0
  keep_taxa <- sample(1:200, 20)
  for (i in 1:25) {
    otu_mat[keep_taxa, i] <- rpois(20, lambda = 50)
  }
  
  # Group B: High richness, low evenness (many taxa, uneven)
  for (i in 26:50) {
    otu_mat[, i] <- rpois(200, lambda = 2)
    # Make a few taxa dominant
    dominant <- sample(1:200, 5)
    otu_mat[dominant, i] <- rpois(5, lambda = 100)
  }
}
otu_table(demo_data) <- otu_table(otu_mat, taxa_are_rows = TRUE)

cat("✓ Test data created with extreme group differences\n\n")

# Test 3: Calculate diversity and consensus
cat("3. Running diversity and consensus analysis...\n")
div_results <- calculate_diversity(demo_data, groups = "Group")
consensus <- consensus_diversity(div_results, method = "adaptive", groups = "Group")

cat("✓ Analysis complete\n\n")

# Test 4: Check the print output
cat("4. Testing print method (should show formatted p-values):\n")
cat("=" * 60, "\n")
print(consensus)
cat("=" * 60, "\n\n")

# Test 5: Check the summary output
cat("5. Testing summary method:\n")
cat("=" * 60, "\n")
summary(consensus)
cat("=" * 60, "\n\n")

# Test 6: Direct access to verify formatting
cat("6. Verifying p-value formatting in conflict analysis:\n")
if (!is.null(consensus$conflict_analysis$results)) {
  results <- consensus$conflict_analysis$results
  
  cat("\nRaw p-values:\n")
  for (i in seq_len(nrow(results))) {
    cat(sprintf("  %s: %.2e\n", results$metric[i], results$p_value[i]))
  }
  
  cat("\nFormatted p-values:\n")
  for (i in seq_len(nrow(results))) {
    cat(sprintf("  %s: %s\n", results$metric[i], results$p_value_formatted[i]))
  }
}

cat("\n=== P-value Formatting Test Complete ===\n")
cat("\nThe p-values should now display as '< 0.001' instead of scientific notation\n")
cat("in both the print() and summary() outputs.\n")