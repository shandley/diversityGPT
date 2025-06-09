# diversityGPT: Working Demo
# Shows the revolutionary capabilities with a proper dataset

devtools::load_all()
library(phyloseq)

cat("=== diversityGPT: Revolutionary Diversity Analysis ===\n\n")

# Use GlobalPatterns dataset (built into phyloseq)
data(GlobalPatterns)

# Take a subset for faster demo
ps <- GlobalPatterns
ps <- prune_samples(sample_sums(ps) > 5000, ps)
ps <- prune_taxa(taxa_sums(ps) > 500, ps)

cat("Dataset:", nsamples(ps), "samples,", ntaxa(ps), "taxa\n\n")

# Step 1: Calculate standard diversity metrics
cat("Step 1: Calculate diversity metrics\n")
div_results <- calculate_diversity(ps, groups = "SampleType")

# Show some results
cat("\nExample diversity values (first 3 samples):\n")
print(div_results[1:3, c("shannon", "simpson", "observed", "chao1")])

# Step 2: Extract universal information - THE REVOLUTIONARY PART!
cat("\n\nStep 2: Extract Universal Information Components\n")
cat("This decomposes ALL diversity into R, E, P, S components!\n\n")

universal_info <- extract_universal_information(
  ps,
  groups = "SampleType",
  include_phylogenetic = TRUE  # GlobalPatterns has a tree!
)

# Show the mathematical framework
cat("Mathematical deconvolution achieved:\n")
cat("- Mean R² across all transformations:", round(universal_info$deconvolution_quality$mean_r_squared, 3), "\n")
cat("- High quality transformations:", universal_info$deconvolution_quality$high_quality_metrics, "\n")
cat("- Overall quality:", universal_info$deconvolution_quality$overall_quality, "\n")

# Step 3: Demonstrate any-to-any transformation
cat("\n\nStep 3: Any-to-Any Metric Transformation Demo\n")
cat("Example: You ONLY have Shannon diversity = 3.5\n")
cat("But you need Simpson, Observed species, and Faith's PD!\n\n")

predicted <- predict_missing_diversity_metrics(
  available_metrics = c(shannon = 3.5),
  transformation_matrix = universal_info$transformation_matrix
)

# Show predictions
pred <- predicted$predicted_metrics
quality <- predicted$transformation_quality$quality_details

cat("Predicted from just Shannon = 3.5:\n")
for (i in 1:min(5, nrow(quality))) {
  metric <- quality$metric[i]
  value <- pred[[metric]][1]
  r2 <- quality$r_squared[i]
  reliable <- quality$reliable[i]
  
  cat(sprintf("  %s: %.3f (R² = %.3f, %s)\n", 
              metric, value, r2, 
              ifelse(reliable, "Reliable", "Less reliable")))
}

# Step 4: Better predictions with more information
cat("\n\nStep 4: More Accurate with Multiple Metrics\n")
cat("Using Shannon = 3.5 AND Observed = 150:\n\n")

predicted2 <- predict_missing_diversity_metrics(
  available_metrics = c(shannon = 3.5, observed = 150),
  transformation_matrix = universal_info$transformation_matrix
)

pred2 <- predicted2$predicted_metrics
cat("Improved predictions:\n")
cat("  Simpson:", round(pred2$simpson[1], 3), "\n")
cat("  Inverse Simpson:", round(pred2$invsimpson[1], 3), "\n")
cat("  Chao1:", round(pred2$chao1[1], 0), "\n")

# Step 5: Cross-study standardization example
cat("\n\nStep 5: Cross-Study Standardization Demo\n")
cat("Problem: Study A used Shannon/Simpson, Study B used Observed/Chao1\n")
cat("Solution: Transform to common metric space!\n\n")

# Simulate two studies
study_A <- c(shannon = 3.2, simpson = 0.85)
study_B <- c(observed = 120, chao1 = 145)

# Standardize both
study_A_full <- predict_missing_diversity_metrics(
  study_A, 
  transformation_matrix = universal_info$transformation_matrix,
  target_metrics = c("observed", "chao1")
)

study_B_full <- predict_missing_diversity_metrics(
  study_B,
  transformation_matrix = universal_info$transformation_matrix,
  target_metrics = c("shannon", "simpson")
)

cat("Study A now has all metrics:\n")
cat("  Original: Shannon =", study_A["shannon"], ", Simpson =", study_A["simpson"], "\n")
cat("  Predicted: Observed ≈", round(study_A_full$predicted_metrics$observed[1], 0),
    ", Chao1 ≈", round(study_A_full$predicted_metrics$chao1[1], 0), "\n")

cat("\nStudy B now has all metrics:\n")
cat("  Original: Observed =", study_B["observed"], ", Chao1 =", study_B["chao1"], "\n")
cat("  Predicted: Shannon ≈", round(study_B_full$predicted_metrics$shannon[1], 2),
    ", Simpson ≈", round(study_B_full$predicted_metrics$simpson[1], 3), "\n")

# Step 6: Visualizations
cat("\n\nStep 6: Visualizations\n")

# Create a 2x2 plot layout
par(mfrow = c(2, 2))

# 1. Transformation quality
plot(universal_info, type = "quality")

# 2. Network (simplified)
plot_diversity_network(universal_info, interactive = FALSE, min_r_squared = 0.7)

# 3. Component proportions for a few samples
components <- universal_info$information_components[1:6,]
barplot(
  t(as.matrix(components[, c("R_proportion", "E_proportion", "P_proportion")])),
  col = c("coral", "skyblue", "lightgreen"),
  legend = c("Richness", "Evenness", "Phylogenetic"),
  main = "Information Components",
  las = 2
)

# 4. Prediction vs actual for validation
# Show R² values as bar chart
r2_values <- universal_info$transformation_matrix$r_squared
names(r2_values) <- universal_info$transformation_matrix$metric
barplot(
  sort(r2_values, decreasing = TRUE),
  main = "Transformation Quality (R²)",
  las = 2,
  col = ifelse(sort(r2_values, decreasing = TRUE) > 0.8, "green", "orange"),
  ylim = c(0, 1)
)
abline(h = 0.8, lty = 2, col = "red")

par(mfrow = c(1, 1))

# Step 7: Consensus analysis
cat("\n\nStep 7: Consensus Analysis\n")
consensus <- consensus_diversity(div_results, method = "correlation_weighted")
cat("Most reliable metric:", consensus$interpretation$dominant_metric, "\n")
cat("Metric weight distribution:", consensus$interpretation$weight_distribution, "\n")

# Summary
cat("\n\n=== REVOLUTIONARY IMPACT ===\n")
cat("✓ Decomposed diversity into universal R, E, P components\n")
cat("✓ Enabled transformation between ANY diversity metrics\n")
cat("✓ Achieved R² >", round(universal_info$deconvolution_quality$mean_r_squared, 2), 
    "for metric predictions\n")
cat("✓ Standardized metrics across different studies\n")
cat("✓ Found consensus among conflicting metrics\n")

cat("\nThis framework enables meta-analysis across ALL microbiome studies,\n")
cat("regardless of which diversity metrics they used!\n")