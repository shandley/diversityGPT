# diversityGPT: Better Network Visualization Demo
# Shows more interesting network topology with diverse data

devtools::load_all()
library(phyloseq)

cat("Creating network with more interesting topology...\n\n")

# Use full GlobalPatterns for more diversity
data(GlobalPatterns)

# Create subset with diverse sample types for contrast
diverse_samples <- c(
  # Human samples (high diversity)
  sample_names(GlobalPatterns)[sample_data(GlobalPatterns)$SampleType == "Feces"],
  # Ocean samples (different diversity pattern)
  sample_names(GlobalPatterns)[sample_data(GlobalPatterns)$SampleType == "Ocean"],
  # Soil samples (high richness)
  sample_names(GlobalPatterns)[sample_data(GlobalPatterns)$SampleType == "Soil"]
)

ps <- prune_samples(diverse_samples, GlobalPatterns)
ps <- prune_taxa(taxa_sums(ps) > 100, ps)

cat("Using", nsamples(ps), "samples from different environments\n")
cat("Sample types:", unique(sample_data(ps)$SampleType), "\n\n")

# Extract universal information
cat("Extracting universal information...\n")
universal_info <- extract_universal_information(
  ps,
  groups = "SampleType",
  include_phylogenetic = TRUE
)

# Calculate relationships for better network
cat("\nDiscovering metric relationships...\n")
relationships <- discover_metric_relationships(
  ps,
  metric_subset = c("shannon", "simpson", "observed", "chao1", 
                    "pielou_evenness", "faith_pd"),
  relationship_types = c("linear", "nonlinear", "conditional")
)

# Create network with varied R² thresholds
cat("\nCreating network visualizations...\n\n")
par(mfrow = c(2, 2))

# 1. High threshold - only strongest relationships
plot_diversity_network(
  universal_info = universal_info,
  relationships = relationships,
  interactive = FALSE,
  min_r_squared = 0.8
)
title("Strong Relationships (R² > 0.8)")

# 2. Medium threshold - more connections
plot_diversity_network(
  universal_info = universal_info,
  relationships = relationships,
  interactive = FALSE,
  min_r_squared = 0.5
)
title("Medium Relationships (R² > 0.5)")

# 3. Component importance by environment
# Show how R vs E dominance varies
components_by_env <- aggregate(
  universal_info$information_components[, c("R_component", "E_component")],
  by = list(Environment = sample_data(ps)$SampleType),
  FUN = mean
)

barplot(
  t(as.matrix(components_by_env[, -1])),
  names.arg = components_by_env$Environment,
  col = c("coral", "skyblue"),
  legend = c("Richness", "Evenness"),
  main = "Information Components by Environment",
  las = 2
)

# 4. Metric clustering
# Show which metrics group together
hclust_result <- hclust(
  dist(cor(universal_info$metric_profiles[, sapply(universal_info$metric_profiles, is.numeric)])),
  method = "ward.D2"
)
plot(hclust_result, main = "Metric Clustering", xlab = "")

par(mfrow = c(1, 1))

# Create interactive network if package available
if (requireNamespace("networkD3", quietly = TRUE)) {
  cat("\nCreating interactive network (opens in browser)...\n")
  interactive_net <- plot_diversity_network(
    universal_info = universal_info,
    relationships = relationships,
    interactive = TRUE,
    min_r_squared = 0.4,
    node_size_by = "importance"
  )
  print(interactive_net)
}

# Show relationship patterns
cat("\n=== Interesting Patterns Found ===\n")

# Find metrics with different relationships
strong_linear <- sapply(relationships$pairwise_relationships, function(x) {
  if (!is.null(x$relationships$linear)) {
    x$relationships$linear$r_squared > 0.9
  } else FALSE
})

nonlinear_better <- sapply(relationships$pairwise_relationships, function(x) {
  linear_r2 <- ifelse(!is.null(x$relationships$linear), 
                      x$relationships$linear$r_squared, 0)
  nonlinear_r2 <- ifelse(!is.null(x$relationships$nonlinear),
                         x$relationships$nonlinear$best_nonlinear, 0)
  nonlinear_r2 > linear_r2 + 0.1
})

cat("\nStrong linear relationships:", sum(strong_linear), "\n")
cat("Nonlinear relationships:", sum(nonlinear_better), "\n")

# Show specific interesting relationships
if (any(nonlinear_better)) {
  nonlinear_pairs <- relationships$pairwise_relationships[nonlinear_better]
  cat("\nExample nonlinear relationship:\n")
  example <- nonlinear_pairs[[1]]
  cat(example$metric_x, "vs", example$metric_y, "\n")
  cat("Linear R²:", round(example$relationships$linear$r_squared, 3), "\n")
  cat("Nonlinear R²:", round(example$relationships$nonlinear$best_nonlinear, 3), "\n")
}

cat("\n=== Network Interpretation ===\n")
cat("• Node color = dominant information component\n")
cat("• Edge width = transformation quality (R²)\n")
cat("• Clustering shows which metrics measure similar aspects\n")
cat("• Different environments show different R/E patterns\n")