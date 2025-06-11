# Demonstration of Beta Diversity Decomposition Framework
# This shows how the theoretical framework might work in practice

library(devtools)
load_all()
library(vegan)
library(ape)

cat("=== Beta Diversity Decomposition Demonstration ===\n\n")

# 1. Create simulated community data with known properties
set.seed(42)
n_samples <- 20
n_species <- 50

# Create communities with different assembly processes
cat("Creating simulated communities with known assembly patterns...\n")

# Group 1: Environmental filtering (species sorting)
# Different environments select different species
env_gradient <- seq(0, 10, length.out = n_samples)
community_matrix <- matrix(0, n_samples, n_species)

for (i in 1:n_samples) {
  # Species have environmental optima
  for (j in 1:n_species) {
    species_optimum <- runif(1, 0, 10)
    species_tolerance <- runif(1, 1, 3)
    
    # Abundance based on match to environment
    env_match <- exp(-((env_gradient[i] - species_optimum)^2) / (2 * species_tolerance^2))
    if (env_match > 0.3) {
      community_matrix[i, j] <- rpois(1, lambda = env_match * 20)
    }
  }
}

# Add spatial structure
coords <- cbind(
  x = runif(n_samples, 0, 10),
  y = runif(n_samples, 0, 10)
)

# Create phylogenetic tree
tree <- rtree(n = n_species)

# 2. Traditional beta diversity analysis
cat("\n--- Traditional Beta Diversity Metrics ---\n")
beta_bray <- vegdist(community_matrix, method = "bray")
beta_jaccard <- vegdist(community_matrix, method = "jaccard")

cat("Mean Bray-Curtis dissimilarity:", round(mean(beta_bray), 3), "\n")
cat("Mean Jaccard dissimilarity:", round(mean(beta_jaccard), 3), "\n")

# 3. Decompose beta diversity using new framework
cat("\n--- Beta Diversity Decomposition ---\n")

# Pairwise example first
cat("\nExample: Comparing samples 1 and 10:\n")
beta_1_10 <- decompose_beta_diversity(
  community_matrix[1, ],
  community_matrix[10, ],
  tree = tree,
  coords_i = coords[1, ],
  coords_j = coords[10, ],
  method = "information"
)
print(beta_1_10)

# Full matrix decomposition
cat("\nPerforming full matrix decomposition...\n")
beta_decomp <- decompose_beta_diversity_matrix(
  community_matrix,
  tree = tree,
  coords = coords,
  method = "information",
  summary_method = "centroid"
)

# 4. Analyze results
cat("\n--- Decomposition Results ---\n")

# Average contribution of each component
mean_components <- c(
  R_beta = mean(beta_decomp$R_beta[upper.tri(beta_decomp$R_beta)]),
  E_beta = mean(beta_decomp$E_beta[upper.tri(beta_decomp$E_beta)]),
  P_beta = mean(beta_decomp$P_beta[upper.tri(beta_decomp$P_beta)]),
  S_beta = mean(beta_decomp$S_beta[upper.tri(beta_decomp$S_beta)])
)

cat("\nMean component values:\n")
print(round(mean_components, 3))

cat("\nPercentage contribution:\n")
print(round(100 * mean_components / sum(mean_components), 1))

# 5. Test ecological interpretation
cat("\n--- Ecological Interpretation ---\n")

# Since we created communities with environmental filtering,
# R_beta should correlate with environmental distance
env_dist <- dist(env_gradient)

correlations <- c(
  R_beta = cor(as.vector(env_dist), as.vector(beta_decomp$R_beta)),
  E_beta = cor(as.vector(env_dist), as.vector(beta_decomp$E_beta)),
  P_beta = cor(as.vector(env_dist), as.vector(beta_decomp$P_beta)),
  S_beta = cor(as.vector(env_dist), as.vector(beta_decomp$S_beta))
)

cat("\nCorrelation with environmental distance:\n")
print(round(correlations, 3))

cat("\nInterpretation: ")
if (correlations["R_beta"] > 0.5) {
  cat("Strong environmental filtering detected (R_beta responds to environment)\n")
}
if (correlations["E_beta"] > 0.5) {
  cat("Competitive dynamics along gradient (E_beta responds to environment)\n")
}
if (correlations["S_beta"] > 0.5) {
  cat("Spatial structure influences community assembly\n")
}

# 6. Visualization
cat("\n--- Creating Visualizations ---\n")

# Heatmaps of components
pdf("beta_decomposition_heatmaps.pdf", width = 10, height = 10)
plot(beta_decomp, type = "heatmap")
dev.off()
cat("Saved: beta_decomposition_heatmaps.pdf\n")

# Component contributions
pdf("beta_component_contributions.pdf", width = 8, height = 6)
plot(beta_decomp, type = "components")
dev.off()
cat("Saved: beta_component_contributions.pdf\n")

# 7. Validation against traditional metrics
cat("\n--- Validation ---\n")
validation <- validate_beta_decomposition(community_matrix, tree, coords)

cat("\nCorrelation between decomposition and traditional metrics:\n")
cat("Jaccard vs R_beta:", round(validation$jaccard_vs_R, 3), "\n")
cat("Bray-Curtis vs (R_beta + E_beta):", round(validation$bray_vs_RE, 3), "\n")

cat("\nComponent orthogonality (mean off-diagonal correlation):", 
    round(validation$mean_off_diagonal_cor, 3), "\n")
cat("Variance explained:", round(validation$variance_explained * 100, 1), "%\n")

# 8. Demonstrate novel analyses
cat("\n--- Novel Analyses Enabled by Decomposition ---\n")

# Question 1: Which component drives community differences?
cat("\n1. Component importance by sample:\n")
sample_importance <- data.frame(
  Sample = 1:n_samples,
  Dominant_Component = apply(
    cbind(beta_decomp$sample_R_beta, beta_decomp$sample_E_beta,
          beta_decomp$sample_P_beta, beta_decomp$sample_S_beta),
    1, 
    function(x) c("R", "E", "P", "S")[which.max(x)]
  )
)
print(table(sample_importance$Dominant_Component))

# Question 2: Scale-dependent assembly mechanisms
cat("\n2. Testing assembly mechanisms:\n")
assembly_test <- test_beta_assembly_mechanisms(
  community_matrix,
  env_data = data.frame(gradient = env_gradient),
  tree = tree,
  coords = coords
)
print(assembly_test$gradient)

# Question 3: Predict unmeasured beta diversity
cat("\n3. Predicting phylogenetic beta from taxonomic components:\n")

# Use R and E components to predict P
lm_model <- lm(as.vector(beta_decomp$P_beta) ~ 
               as.vector(beta_decomp$R_beta) + 
               as.vector(beta_decomp$E_beta))

cat("R-squared:", round(summary(lm_model)$r.squared, 3), "\n")
cat("Interpretation: Phylogenetic turnover is",
    ifelse(summary(lm_model)$r.squared > 0.5, "well", "poorly"),
    "predicted by taxonomic turnover\n")

# 9. Practical application example
cat("\n--- Practical Application: Identifying Biodiversity Hotspots ---\n")

# Which samples contribute most to beta diversity?
beta_contributions <- data.frame(
  Sample = 1:n_samples,
  R_contribution = beta_decomp$sample_R_beta,
  E_contribution = beta_decomp$sample_E_beta,
  P_contribution = beta_decomp$sample_P_beta,
  S_contribution = beta_decomp$sample_S_beta,
  Total = rowSums(cbind(beta_decomp$sample_R_beta, beta_decomp$sample_E_beta,
                       beta_decomp$sample_P_beta, beta_decomp$sample_S_beta))
)

# Identify samples with high uniqueness
hotspots <- which(beta_contributions$Total > quantile(beta_contributions$Total, 0.75))
cat("\nBiodiversity hotspots (high beta diversity):", hotspots, "\n")
cat("These samples have unique combinations of species, abundances, or phylogenetic composition\n")

# 10. Future directions
cat("\n--- Summary and Future Directions ---\n")
cat("\nThis framework enables:\n")
cat("1. Mechanistic understanding of what drives community differences\n")
cat("2. Cross-study comparison using different beta diversity metrics\n")
cat("3. Prediction of unmeasured beta diversity components\n")
cat("4. Scale-integrated analysis linking alpha and beta diversity\n")
cat("5. Hypothesis testing about assembly mechanisms\n")

cat("\nNext steps for implementation:\n")
cat("- Optimize algorithms for large datasets\n")
cat("- Test on real ecological datasets\n")
cat("- Develop standardized interpretation guidelines\n")
cat("- Create interactive visualization tools\n")
cat("- Integrate with existing diversityGPT workflow\n")

cat("\n=== Demonstration Complete ===\n")