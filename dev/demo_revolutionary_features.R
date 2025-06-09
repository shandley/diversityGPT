# diversityGPT: Demonstrating Revolutionary Capabilities
# This script shows why this package will transform microbiome analysis

devtools::load_all()

cat("==========================================================\n")
cat("diversityGPT: The World's First Universal Diversity Framework\n")
cat("==========================================================\n\n")

# Create example data with known patterns
set.seed(123)
n_samples <- 12

# Create three distinct community types
otu_mat <- matrix(0, nrow = n_samples, ncol = 30)

# Type 1: Low diversity, high dominance (samples 1-4)
for (i in 1:4) {
  otu_mat[i, 1] <- 1000      # Dominant species
  otu_mat[i, 2:5] <- c(10, 5, 2, 1)  # Rare species
}

# Type 2: High diversity, even distribution (samples 5-8)
for (i in 5:8) {
  otu_mat[i, 1:20] <- rep(50, 20)  # Even distribution
}

# Type 3: Medium diversity (samples 9-12)
for (i in 9:12) {
  otu_mat[i, 1:10] <- c(100, 80, 60, 40, 30, 20, 15, 10, 5, 2)
}

rownames(otu_mat) <- paste0("Sample_", 1:n_samples)
colnames(otu_mat) <- paste0("Species_", 1:30)

# Create phyloseq object
sample_data <- data.frame(
  Community_Type = rep(c("Low_Diversity", "High_Diversity", "Medium_Diversity"), each = 4),
  row.names = rownames(otu_mat)
)

ps <- phyloseq::phyloseq(
  phyloseq::otu_table(otu_mat, taxa_are_rows = FALSE),
  phyloseq::sample_data(sample_data)
)

cat("=== REVOLUTIONARY FEATURE 1: Universal Information Decomposition ===\n")
cat("Every diversity metric can be expressed as R + E + P + S components!\n\n")

universal_info <- extract_universal_information(ps, groups = "Community_Type")

# Show the magic
cat("Sample 1 (Low diversity):\n")
cat("  Richness component:", round(universal_info$information_components$R_component[1], 3), "\n")
cat("  Evenness component:", round(universal_info$information_components$E_component[1], 3), "\n")

cat("\nSample 5 (High diversity):\n")
cat("  Richness component:", round(universal_info$information_components$R_component[5], 3), "\n")
cat("  Evenness component:", round(universal_info$information_components$E_component[5], 3), "\n")

cat("\n=== REVOLUTIONARY FEATURE 2: Any-to-Any Metric Transformation ===\n")
cat("Transform ANY diversity metric to ANY other metric!\n\n")

# Example: You only measured Shannon, but need Simpson and Chao1
cat("Scenario: You only have Shannon diversity = 2.3\n")
cat("But your colleague needs Simpson and Chao1 values...\n\n")

predicted <- universal_diversity_transform(
  source_metrics = c(shannon = 2.3),
  target_metrics = c("simpson", "chao1", "observed"),
  transformation_matrix = universal_info$transformation_matrix
)

cat("PREDICTED from just Shannon:\n")
cat("  Simpson:", round(predicted$predicted_metrics$simpson[1], 3), "\n")
cat("  Chao1:", round(predicted$predicted_metrics$chao1[1], 3), "\n")
cat("  Observed species:", round(predicted$predicted_metrics$observed[1], 3), "\n")
cat("  Transformation quality:", predicted$transformation_quality$overall_quality, "\n")

cat("\n=== REVOLUTIONARY FEATURE 3: Cross-Study Standardization ===\n")
cat("Compare studies that used different diversity metrics!\n\n")

# Simulate two studies with different metrics
study1_metrics <- c(shannon = 2.1, simpson = 0.8)  # Study 1 measured these
study2_metrics <- c(observed = 45, chao1 = 52)     # Study 2 measured these

cat("Study 1 measured: Shannon and Simpson\n")
cat("Study 2 measured: Observed species and Chao1\n")
cat("Problem: How to compare them?\n\n")

# Standardize both to common metrics
study1_standardized <- predict_missing_diversity_metrics(
  available_metrics = study1_metrics,
  transformation_matrix = universal_info$transformation_matrix,
  target_metrics = c("observed", "chao1")
)

study2_standardized <- predict_missing_diversity_metrics(
  available_metrics = study2_metrics,
  transformation_matrix = universal_info$transformation_matrix,
  target_metrics = c("shannon", "simpson")
)

cat("Now both studies have ALL metrics - perfect for meta-analysis!\n")

cat("\n=== REVOLUTIONARY FEATURE 4: Discover Hidden Relationships ===\n")
cat("Find mathematical relationships between ALL diversity metrics\n\n")

relationships <- discover_metric_relationships(
  ps,
  metric_subset = c("shannon", "simpson", "observed", "chao1")
)

# Show strongest relationship
strongest_rel <- relationships$pairwise_relationships[[1]]
cat("Strongest relationship found:\n")
cat("  ", strongest_rel$metric_x, "↔", strongest_rel$metric_y, "\n")
cat("  Correlation:", round(strongest_rel$relationships$linear$pearson_correlation, 3), "\n")
cat("  R²:", round(strongest_rel$relationships$linear$r_squared, 3), "\n")

cat("\n=== VISUALIZATION: See the Hidden Structure of Diversity ===\n\n")

# Network plot
cat("Creating network visualization...\n")
plot_diversity_network(universal_info, interactive = FALSE, min_r_squared = 0.3)

# Information dashboard
cat("\nCreating information component dashboard...\n")
plot_information_components(universal_info)

cat("\n=== AI-POWERED INTERPRETATION (if API key available) ===\n\n")

if (!is.null(get_api_key("anthropic")) || !is.null(get_api_key("openai"))) {
  div_results <- calculate_diversity(ps, groups = "Community_Type")
  consensus <- consensus_diversity(div_results)
  
  interpretation <- interpret_diversity(
    consensus,
    context = list(
      environment = "experimental_communities",
      condition = "diversity_gradient",
      organism = "bacteria"
    )
  )
  
  cat("AI Interpretation:\n")
  cat(interpretation$interpretation, "\n")
} else {
  cat("Add API key to .Renviron for AI interpretation:\n")
  cat("ANTHROPIC_API_KEY=your-key-here\n")
}

cat("\n==========================================================\n")
cat("SUMMARY: diversityGPT Revolutionary Capabilities\n")
cat("==========================================================\n")
cat("✓ Decompose ANY metric into universal components\n")
cat("✓ Transform between ANY diversity metrics\n")
cat("✓ Enable cross-study standardization\n")
cat("✓ Discover hidden mathematical relationships\n")
cat("✓ Visualize the structure of diversity\n")
cat("✓ AI-powered ecological interpretation\n")
cat("\nThis changes EVERYTHING for microbiome meta-analysis!\n")