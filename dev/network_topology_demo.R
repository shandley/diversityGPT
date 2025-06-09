# diversityGPT: Network Topology Demo
# Shows how network structure reveals metric relationships

devtools::load_all()
library(phyloseq)

cat("=== Diversity Metric Network Topology Demo ===\n\n")

# Create synthetic data with known diversity patterns
set.seed(123)
n_samples <- 30
n_species <- 100

# Create three different community types
otu_mat <- matrix(0, nrow = n_samples, ncol = n_species)

for (i in 1:n_samples) {
  if (i <= 10) {
    # Type 1: Low richness, high evenness
    n_present <- 10
    otu_mat[i, 1:n_present] <- rep(50, n_present)
  } else if (i <= 20) {
    # Type 2: High richness, low evenness  
    n_present <- 80
    abundances <- c(500, 200, 100, rep(5, n_present - 3))
    otu_mat[i, 1:n_present] <- abundances
  } else {
    # Type 3: Medium richness, medium evenness
    n_present <- 40
    otu_mat[i, 1:n_present] <- sort(rpois(n_present, 20), decreasing = TRUE)
  }
}

rownames(otu_mat) <- paste0("Sample", 1:n_samples)
colnames(otu_mat) <- paste0("Species", 1:n_species)

# Create phyloseq object
sample_data <- data.frame(
  Community = rep(c("LowRich_HighEven", "HighRich_LowEven", "Medium"), each = 10),
  row.names = rownames(otu_mat)
)

ps <- phyloseq::phyloseq(
  phyloseq::otu_table(otu_mat, taxa_are_rows = FALSE),
  phyloseq::sample_data(sample_data)
)

cat("Created dataset with 3 community types:\n")
cat("- Low richness, high evenness (10 samples)\n")
cat("- High richness, low evenness (10 samples)\n")
cat("- Medium richness/evenness (10 samples)\n\n")

# Extract universal information
cat("Extracting universal information...\n")
universal_info <- extract_universal_information(ps, groups = "Community")

# Show how components differ by community type
cat("\nAverage information components by community type:\n")
comp_summary <- aggregate(
  universal_info$information_components[, c("R_component", "E_component")],
  by = list(Community = sample_data(ps)$Community),
  FUN = mean
)
# Round only the numeric columns
comp_summary[, -1] <- round(comp_summary[, -1], 3)
print(comp_summary)

# Create simple network visualization
cat("\n\nCreating network visualization...\n")
par(mfrow = c(1, 2))

# Network plot
plot_diversity_network(
  universal_info = universal_info,
  interactive = FALSE,
  min_r_squared = 0.5,
  node_size_by = "importance"
)
title("Metric Relationship Network", cex.main = 1.2)

# Show R vs E components
plot(
  universal_info$information_components$R_component,
  universal_info$information_components$E_component,
  col = as.numeric(factor(sample_data(ps)$Community)),
  pch = 19,
  xlab = "Richness Component (R)",
  ylab = "Evenness Component (E)",
  main = "Information Space"
)
legend("topright", 
       legend = levels(factor(sample_data(ps)$Community)),
       col = 1:3,
       pch = 19,
       cex = 0.8)

par(mfrow = c(1, 1))

# Show transformation quality
cat("\n=== Transformation Quality ===\n")
trans_quality <- universal_info$transformation_matrix[, c("metric", "r_squared")]
trans_quality <- trans_quality[order(trans_quality$r_squared, decreasing = TRUE), ]
cat("\nBest transformations:\n")
print(head(trans_quality))

cat("\n=== Network Interpretation ===\n")
cat("The network shows:\n")
cat("• Metrics with similar information content cluster together\n")
cat("• Edge thickness shows how well metrics can predict each other\n")
cat("• Node color indicates dominant component (R=red, E=blue)\n")
cat("• Different community types occupy different regions of R-E space\n")

# Calculate some interesting relationships
cat("\n=== Key Insights ===\n")

# Which metrics are most "central" (predict others well)?
avg_r2 <- aggregate(
  universal_info$transformation_matrix$r_squared,
  by = list(metric = universal_info$transformation_matrix$metric),
  FUN = mean
)
best_predictor <- avg_r2[which.max(avg_r2$x), ]
cat("Most predictive metric:", best_predictor$metric, 
    "(avg R² =", round(best_predictor$x, 3), ")\n")

# Which metrics are hardest to predict?
worst_predictor <- avg_r2[which.min(avg_r2$x), ]
cat("Least predictive metric:", worst_predictor$metric,
    "(avg R² =", round(worst_predictor$x, 3), ")\n")

cat("\nThis demonstrates how the universal framework reveals\n")
cat("the hidden mathematical structure of diversity metrics!\n")