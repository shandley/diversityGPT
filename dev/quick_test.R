# diversityGPT: Quick Test Script
# Simple commands to test the revolutionary features

# Load package
devtools::load_all()

# Quick test with built-in data
library(phyloseq)
data(GlobalPatterns)  # Built-in phyloseq dataset

# Subset for faster testing
ps <- prune_samples(sample_sums(GlobalPatterns) > 10000, GlobalPatterns)
ps <- prune_taxa(taxa_sums(ps) > 100, ps)

cat("Using GlobalPatterns dataset:", nsamples(ps), "samples,", ntaxa(ps), "taxa\n\n")

# 1. Extract universal information (This is revolutionary!)
cat("=== Extracting Universal Information Components ===\n")
universal_info <- extract_universal_information(ps, include_phylogenetic = TRUE)
print(universal_info)

# 2. Predict missing metrics from just Shannon diversity
cat("\n=== Predicting ALL Metrics from Just Shannon ===\n")
predicted <- predict_missing_diversity_metrics(
  available_metrics = c(shannon = 2.5),
  transformation_matrix = universal_info$transformation_matrix
)
print(predicted)

# 3. Visualize metric relationships
cat("\n=== Creating Network Visualization ===\n")
plot_diversity_network(universal_info, interactive = FALSE)

# 4. Information component dashboard
cat("\n=== Creating Information Dashboard ===\n")
plot_information_components(universal_info)

# 5. Quick consensus analysis
cat("\n=== Running Consensus Analysis ===\n")
div_results <- calculate_diversity(ps)
consensus <- consensus_diversity(div_results)
print(consensus)

cat("\n✅ Quick test complete! All core functions working.\n")