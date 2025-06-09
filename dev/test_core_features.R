# diversityGPT: Core Features Test
# Clean demonstration of revolutionary capabilities

# Setup
devtools::load_all()
library(phyloseq)

# Create simple test data with clear diversity patterns
set.seed(42)
cat("Creating test microbiome data with known diversity patterns...\n\n")

# 10 samples, 20 species
otu_mat <- matrix(0, nrow = 10, ncol = 20)

# Create gradient from low to high diversity
for (i in 1:10) {
  if (i <= 3) {
    # Low diversity: 1 dominant species
    otu_mat[i, 1] <- 100
    otu_mat[i, 2:3] <- c(5, 2)
  } else if (i <= 7) {
    # Medium diversity
    n_species <- 5 + i
    otu_mat[i, 1:n_species] <- sort(rpois(n_species, 20), decreasing = TRUE)
  } else {
    # High diversity: many species, even distribution
    otu_mat[i, 1:18] <- rep(10, 18)
  }
}

rownames(otu_mat) <- paste0("Sample", 1:10)
colnames(otu_mat) <- paste0("Species", 1:20)

# Create phyloseq object
sample_data <- data.frame(
  DiversityLevel = c(rep("Low", 3), rep("Medium", 4), rep("High", 3)),
  row.names = rownames(otu_mat)
)

ps <- phyloseq::phyloseq(
  phyloseq::otu_table(otu_mat, taxa_are_rows = FALSE),
  phyloseq::sample_data(sample_data)
)

cat("=== TEST 1: Calculate Basic Diversity Metrics ===\n")
div_results <- calculate_diversity(ps, groups = "DiversityLevel")

# Show a few samples
cat("\nDiversity metrics for first 5 samples:\n")
print(div_results[1:5, c("shannon", "simpson", "observed", "group")])

cat("\n=== TEST 2: Extract Universal Information Components ===\n")
cat("Decomposing diversity into Richness (R) and Evenness (E) components...\n\n")

universal_info <- extract_universal_information(
  ps, 
  groups = "DiversityLevel",
  include_phylogenetic = FALSE,  # No tree in this example
  include_spatial = FALSE
)

# Show components for samples with different diversity
components <- universal_info$information_components
cat("Low diversity sample (Sample1):\n")
cat("  R (Richness):", round(components$R_component[1], 3), "\n")
cat("  E (Evenness):", round(components$E_component[1], 3), "\n\n")

cat("High diversity sample (Sample10):\n")
cat("  R (Richness):", round(components$R_component[10], 3), "\n")
cat("  E (Evenness):", round(components$E_component[10], 3), "\n")

cat("\n=== TEST 3: Predict Missing Metrics ===\n")
cat("Scenario: You only measured Shannon diversity = 1.5\n")
cat("But you need Simpson, Observed species, and Chao1...\n\n")

predicted <- predict_missing_diversity_metrics(
  available_metrics = c(shannon = 1.5),
  transformation_matrix = universal_info$transformation_matrix
)

cat("Predicted from Shannon = 1.5:\n")
pred_metrics <- predicted$predicted_metrics
cat("  Simpson:", round(pred_metrics$simpson[1], 3), "\n")
cat("  Observed species:", round(pred_metrics$observed[1], 0), "\n")
cat("  Chao1:", round(pred_metrics$chao1[1], 0), "\n")
cat("  Prediction quality:", predicted$transformation_quality$overall_quality, "\n")

cat("\n=== TEST 4: More Accurate Predictions with Two Metrics ===\n")
cat("Using Shannon = 1.5 AND Observed = 8:\n\n")

predicted2 <- predict_missing_diversity_metrics(
  available_metrics = c(shannon = 1.5, observed = 8),
  transformation_matrix = universal_info$transformation_matrix
)

pred_metrics2 <- predicted2$predicted_metrics
cat("  Simpson:", round(pred_metrics2$simpson[1], 3), "\n")
cat("  Chao1:", round(pred_metrics2$chao1[1], 0), "\n")
cat("  Pielou evenness:", round(pred_metrics2$pielou_evenness[1], 3), "\n")

cat("\n=== TEST 5: Consensus Analysis ===\n")
consensus <- consensus_diversity(div_results, method = "correlation_weighted")
cat("Consensus found that", consensus$interpretation$dominant_metric, 
    "is the most reliable metric\n")
cat("Weight distribution:", consensus$interpretation$weight_distribution, "\n")

cat("\n=== TEST 6: Visualizations ===\n")
cat("Creating visualizations...\n\n")

# Network plot
par(mfrow = c(1, 2))
plot(universal_info, type = "quality")

# Create a simple component plot
barplot(
  t(as.matrix(components[, c("R_proportion", "E_proportion")])),
  names.arg = rownames(components),
  col = c("coral", "skyblue"),
  legend = c("Richness", "Evenness"),
  main = "Information Components by Sample",
  las = 2
)
par(mfrow = c(1, 1))

cat("\n=== SUMMARY ===\n")
cat("✓ Calculated diversity metrics for all samples\n")
cat("✓ Extracted universal R & E components\n")
cat("✓ Predicted missing metrics from minimal data\n")
cat("✓ Found consensus across multiple metrics\n")
cat("✓ Created visualizations of relationships\n")

cat("\nTransformation matrix quality:\n")
quality_summary <- universal_info$deconvolution_quality
cat("  Mean R²:", round(quality_summary$mean_r_squared, 3), "\n")
cat("  High quality transformations:", quality_summary$high_quality_metrics, "\n")
cat("  Overall rating:", quality_summary$overall_quality, "\n")

cat("\nThis demonstrates how diversityGPT enables:\n")
cat("• Cross-study standardization\n")
cat("• Missing data imputation\n")
cat("• Universal metric relationships\n")
cat("• Consensus across conflicting metrics\n")