# Test Taxa Indicators with Null Model Validation
# Run this script to test the new functionality

# Load the package
library(devtools)
load_all()

# Load required packages
library(ggplot2)
library(phyloseq)

# 1. Basic Taxa Driver Analysis
# -----------------------------
cat("=== 1. Basic Taxa Driver Analysis ===\n")

# Create demo data with meaningful patterns
set.seed(123)
demo_data <- create_demo_phyloseq(n_samples = 20, n_taxa = 100)

# Extract universal information first
info <- extract_universal_information(demo_data)
cat("Universal information extracted\n")

# Identify taxa drivers
drivers <- identify_taxa_drivers(
  demo_data, 
  components = info,
  top_n = 10,
  method = "contribution",
  verbose = TRUE
)

# Print summary
print(drivers)

# 2. Visualize Taxa Drivers
# -------------------------
cat("\n=== 2. Visualizing Taxa Drivers ===\n")

# Bar plot of top drivers
p1 <- plot(drivers, type = "bar", top_n = 10)
print(p1)

# Network visualization
p2 <- plot(drivers, type = "network", top_n = 8, interactive = FALSE)
print(p2)

# Heatmap of contributions
p3 <- plot(drivers, type = "heatmap", top_n = 15)
print(p3)

# Multi-component drivers
p4 <- plot(drivers, type = "contribution", top_n = 20)
print(p4)

# 3. Null Model Validation
# ------------------------
cat("\n=== 3. Null Model Validation ===\n")

# Quick validation with fewer permutations for testing
validated <- validate_taxa_indicators(
  demo_data,
  drivers,
  null_models = c("row_shuffle", "curveball"),
  n_permutations = 99,  # Use 999 for real analysis
  verbose = TRUE
)

# Print validation summary
print(validated)

# 4. Examine Significant Indicators
# ---------------------------------
cat("\n=== 4. Significant Indicators ===\n")

# Richness indicators
if (nrow(validated$significant_indicators$richness_drivers) > 0) {
  cat("\nSignificant RICHNESS indicators:\n")
  print(validated$significant_indicators$richness_drivers[, 
    c("taxon", "contribution", "p_value", "effect_size")])
} else {
  cat("\nNo significant richness indicators found\n")
}

# Evenness indicators
if (nrow(validated$significant_indicators$evenness_drivers) > 0) {
  cat("\nSignificant EVENNESS indicators:\n")
  print(validated$significant_indicators$evenness_drivers[,
    c("taxon", "contribution", "p_value", "effect_size")])
} else {
  cat("\nNo significant evenness indicators found\n")
}

# 5. Visualize Validation Results
# -------------------------------
cat("\n=== 5. Validation Visualizations ===\n")

# P-values for richness component
p5 <- plot(validated, type = "pvalues", component = "richness")
print(p5)

# Effect sizes for richness
p6 <- plot(validated, type = "effects", component = "richness")
print(p6)

# Null distributions for top indicators
p7 <- plot(validated, type = "null_dist", component = "richness", top_n = 6)
print(p7)

# 6. Test Different Methods
# -------------------------
cat("\n=== 6. Testing Different Driver Methods ===\n")

# Variance method
drivers_var <- identify_taxa_drivers(
  demo_data,
  method = "variance",
  top_n = 5,
  verbose = FALSE
)
cat("\nVariance method - top richness drivers:\n")
print(drivers_var$richness_drivers[, c("taxon", "contribution")])

# Correlation method
drivers_cor <- identify_taxa_drivers(
  demo_data,
  method = "correlation", 
  top_n = 5,
  verbose = FALSE
)
cat("\nCorrelation method - top richness drivers:\n")
print(drivers_cor$richness_drivers[, c("taxon", "contribution")])

# 7. Generate Reports
# -------------------
cat("\n=== 7. Generating Reports ===\n")

# Taxa driver report
report_file1 <- tempfile(fileext = ".html")
report_taxa_drivers(drivers, demo_data, report_file1)
cat("Taxa driver report saved to:", report_file1, "\n")

# Null validation report
report_file2 <- tempfile(fileext = ".html")
report_null_validation(validated, demo_data, report_file2)
cat("Validation report saved to:", report_file2, "\n")

# Open reports in browser (optional)
# browseURL(report_file1)
# browseURL(report_file2)

# 8. Test with Real Data (if available)
# -------------------------------------
cat("\n=== 8. Test with phyloseq Example Data ===\n")

# Try with GlobalPatterns if available
if (requireNamespace("phyloseq", quietly = TRUE)) {
  data("GlobalPatterns", package = "phyloseq")
  
  # Subset for faster testing
  gp_subset <- phyloseq::prune_taxa(
    phyloseq::taxa_sums(GlobalPatterns) > 1000,
    GlobalPatterns
  )
  gp_subset <- phyloseq::prune_samples(
    phyloseq::sample_sums(gp_subset) > 5000,
    gp_subset
  )
  
  cat("Testing with GlobalPatterns subset:\n")
  cat("Samples:", phyloseq::nsamples(gp_subset), "\n")
  cat("Taxa:", phyloseq::ntaxa(gp_subset), "\n")
  
  # Run analysis
  gp_drivers <- identify_taxa_drivers(
    gp_subset,
    top_n = 10,
    method = "contribution",
    verbose = FALSE
  )
  
  # Quick validation
  gp_validated <- validate_taxa_indicators(
    gp_subset,
    gp_drivers,
    null_models = "row_shuffle",
    n_permutations = 49,
    verbose = FALSE
  )
  
  cat("\nGlobalPatterns validation summary:\n")
  print(gp_validated)
}

# 9. Performance Test
# -------------------
cat("\n=== 9. Performance Test ===\n")

# Time the analysis
timing <- system.time({
  large_data <- create_demo_phyloseq(n_samples = 50, n_taxa = 500)
  
  large_drivers <- identify_taxa_drivers(
    large_data,
    top_n = 20,
    verbose = FALSE
  )
})

cat("Time for 50 samples x 500 taxa:", round(timing[3], 2), "seconds\n")

# 10. Edge Cases
# --------------
cat("\n=== 10. Testing Edge Cases ===\n")

# Very small dataset
tiny_data <- create_demo_phyloseq(n_samples = 3, n_taxa = 5)
tiny_drivers <- identify_taxa_drivers(tiny_data, top_n = 3, verbose = FALSE)
cat("Tiny dataset (3x5) - drivers found:", 
    nrow(tiny_drivers$richness_drivers), "\n")

# Sparse data
sparse_data <- demo_data
otu_table(sparse_data)[otu_table(sparse_data) < 5] <- 0
sparse_drivers <- identify_taxa_drivers(sparse_data, top_n = 5, verbose = FALSE)
cat("Sparse dataset - drivers found:", 
    nrow(sparse_drivers$richness_drivers), "\n")

cat("\n=== All tests completed! ===\n")
cat("Check the plots and reports generated.\n")