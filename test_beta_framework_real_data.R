# Test Beta Diversity Framework on Real Data
# This script validates the theoretical framework using real ecological datasets

library(devtools)
load_all()
library(vegan)
library(ape)
library(phyloseq)
library(ggplot2)

# Suppress startup messages
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

cat("=== Testing Beta Diversity Framework on Real Data ===\n\n")

# Test 1: Use GlobalPatterns dataset (built into phyloseq)
cat("Test 1: GlobalPatterns Dataset Analysis\n")
cat("======================================\n")

# Load real data
data("GlobalPatterns")
cat("Loaded GlobalPatterns dataset:\n")
cat("- Samples:", nsamples(GlobalPatterns), "\n")
cat("- Taxa:", ntaxa(GlobalPatterns), "\n")
cat("- Sample types:", paste(unique(sample_data(GlobalPatterns)$SampleType), collapse = ", "), "\n\n")

# Subset for faster processing (select representative sample types)
sample_types_to_keep <- c("Feces", "Skin", "Tongue", "Ocean", "Soil")
gp_subset <- subset_samples(GlobalPatterns, SampleType %in% sample_types_to_keep)
gp_subset <- filter_taxa(gp_subset, function(x) sum(x > 0) > 2, TRUE)  # Remove rare taxa

cat("Working with subset:\n")
cat("- Samples:", nsamples(gp_subset), "\n")
cat("- Taxa:", ntaxa(gp_subset), "\n\n")

# Extract community matrix
community_matrix <- as.matrix(otu_table(gp_subset))
if (taxa_are_rows(gp_subset)) {
  community_matrix <- t(community_matrix)
}

# Get sample metadata
sample_metadata <- data.frame(sample_data(gp_subset))

# Get phylogenetic tree
tree <- phy_tree(gp_subset)

cat("Step 1: Traditional Beta Diversity Analysis\n")
cat("-------------------------------------------\n")

# Calculate traditional beta diversity metrics
beta_bray <- vegdist(community_matrix, method = "bray")
beta_jaccard <- vegdist(community_matrix, method = "jaccard")

# If picante is available, calculate UniFrac
if (requireNamespace("picante", quietly = TRUE)) {
  tryCatch({
    beta_unifrac <- picante::unifrac(community_matrix, tree)
    unifrac_available <- TRUE
    cat("✓ UniFrac calculated successfully\n")
  }, error = function(e) {
    beta_unifrac <- NULL
    unifrac_available <- FALSE
    cat("✗ UniFrac calculation failed:", conditionMessage(e), "\n")
  })
} else {
  beta_unifrac <- NULL
  unifrac_available <- FALSE
  cat("✗ picante package not available for UniFrac\n")
}

cat("\nTraditional beta diversity summary:\n")
cat("- Mean Bray-Curtis:", round(mean(beta_bray), 3), "\n")
cat("- Mean Jaccard:", round(mean(beta_jaccard), 3), "\n")
if (unifrac_available) {
  cat("- Mean UniFrac:", round(mean(beta_unifrac), 3), "\n")
}

cat("\nStep 2: Apply Beta Diversity Decomposition Framework\n")
cat("---------------------------------------------------\n")

# Test pairwise decomposition first
cat("Testing pairwise decomposition (samples 1 vs 5):\n")

# Compare two very different samples (e.g., Feces vs Ocean)
sample_types <- sample_metadata$SampleType
feces_idx <- which(sample_types == "Feces")[1]
ocean_idx <- which(sample_types == "Ocean")[1]

if (!is.na(feces_idx) && !is.na(ocean_idx)) {
  pairwise_result <- decompose_beta_diversity(
    community_matrix[feces_idx, ],
    community_matrix[ocean_idx, ],
    tree = tree,
    method = "information"
  )
  
  cat("Feces vs Ocean decomposition:\n")
  print(pairwise_result)
  
  # Interpretation
  dominant_component <- names(which.max(unlist(pairwise_result[1:4])))
  cat("\nDominant component:", dominant_component, "\n")
  
  if (dominant_component == "R_beta") {
    cat("Interpretation: Primarily driven by species identity differences (environmental filtering)\n")
  } else if (dominant_component == "E_beta") {
    cat("Interpretation: Primarily driven by abundance pattern differences (competitive dynamics)\n")
  } else if (dominant_component == "P_beta") {
    cat("Interpretation: Primarily driven by phylogenetic differences (evolutionary constraints)\n")
  }
}

cat("\nStep 3: Full Matrix Decomposition\n")
cat("--------------------------------\n")

# Perform full matrix decomposition
cat("Performing matrix-level decomposition...\n")
beta_decomp <- decompose_beta_diversity_matrix(
  community_matrix,
  tree = tree,
  coords = NULL,  # No spatial coordinates in this dataset
  method = "information",
  summary_method = "centroid"
)

# Analyze results
cat("\nDecomposition results:\n")
mean_components <- c(
  R_beta = mean(beta_decomp$R_beta[upper.tri(beta_decomp$R_beta)]),
  E_beta = mean(beta_decomp$E_beta[upper.tri(beta_decomp$E_beta)]),
  P_beta = mean(beta_decomp$P_beta[upper.tri(beta_decomp$P_beta)]),
  S_beta = mean(beta_decomp$S_beta[upper.tri(beta_decomp$S_beta)])
)

cat("Mean component values:\n")
print(round(mean_components, 3))

cat("\nPercentage contribution:\n")
contributions <- 100 * mean_components / sum(mean_components)
print(round(contributions, 1))

# Determine which component dominates
dominant_component_overall <- names(which.max(mean_components))
cat("\nOverall dominant component:", dominant_component_overall, "\n")

cat("\nStep 4: Validation Against Traditional Metrics\n")
cat("---------------------------------------------\n")

# Validate decomposition against traditional metrics
validation <- validate_beta_decomposition(community_matrix, tree, coords = NULL)

cat("Validation results:\n")
cat("- Jaccard vs R_beta correlation:", round(validation$jaccard_vs_R, 3), "\n")
cat("- Bray-Curtis vs (R_beta + E_beta) correlation:", round(validation$bray_vs_RE, 3), "\n")

# Component orthogonality
cat("\nComponent orthogonality matrix:\n")
print(round(validation$orthogonality, 3))

mean_off_diag <- mean(abs(validation$orthogonality[upper.tri(validation$orthogonality)]))
cat("Mean off-diagonal correlation:", round(mean_off_diag, 3), "\n")

if (mean_off_diag < 0.3) {
  cat("✓ Components are reasonably orthogonal\n")
} else {
  cat("⚠ Components show some correlation - may indicate methodological refinement needed\n")
}

cat("\nStep 5: Ecological Interpretation\n")
cat("---------------------------------\n")

# Analyze patterns by sample type
sample_types_unique <- unique(sample_metadata$SampleType)
cat("Sample types:", paste(sample_types_unique, collapse = ", "), "\n\n")

# For each sample type, calculate average beta contribution
sample_beta_summary <- data.frame(
  SampleType = sample_metadata$SampleType,
  R_beta_contrib = beta_decomp$sample_R_beta,
  E_beta_contrib = beta_decomp$sample_E_beta,
  P_beta_contrib = beta_decomp$sample_P_beta,
  S_beta_contrib = beta_decomp$sample_S_beta
)

# Group by sample type
beta_by_type <- sample_beta_summary %>%
  group_by(SampleType) %>%
  summarise(
    R_beta_mean = mean(R_beta_contrib, na.rm = TRUE),
    E_beta_mean = mean(E_beta_contrib, na.rm = TRUE),
    P_beta_mean = mean(P_beta_contrib, na.rm = TRUE),
    S_beta_mean = mean(S_beta_contrib, na.rm = TRUE),
    .groups = "drop"
  )

cat("Beta diversity contributions by sample type:\n")
print(beta_by_type)

# Identify which sample type has highest contribution to each component
cat("\nSample types with highest contribution to each component:\n")
cat("- R_beta (species turnover):", beta_by_type$SampleType[which.max(beta_by_type$R_beta_mean)], "\n")
cat("- E_beta (abundance turnover):", beta_by_type$SampleType[which.max(beta_by_type$E_beta_mean)], "\n")
cat("- P_beta (phylogenetic turnover):", beta_by_type$SampleType[which.max(beta_by_type$P_beta_mean)], "\n")
cat("- S_beta (spatial turnover):", beta_by_type$SampleType[which.max(beta_by_type$S_beta_mean)], "\n")

cat("\nStep 6: Assembly Mechanism Inference\n")
cat("-----------------------------------\n")

# Create a simple environmental proxy from sample type
# Assign numerical values to different environments
env_proxy <- case_when(
  sample_metadata$SampleType == "Ocean" ~ 1,
  sample_metadata$SampleType == "Freshwater" ~ 2,
  sample_metadata$SampleType == "Soil" ~ 3,
  sample_metadata$SampleType == "Skin" ~ 4,
  sample_metadata$SampleType == "Tongue" ~ 4.5,
  sample_metadata$SampleType == "Feces" ~ 5,
  TRUE ~ 3
)

# Test which component correlates with environmental differences
env_dist <- dist(env_proxy)

correlations <- c(
  R_beta = cor(as.vector(env_dist), as.vector(beta_decomp$R_beta)),
  E_beta = cor(as.vector(env_dist), as.vector(beta_decomp$E_beta)),
  P_beta = cor(as.vector(env_dist), as.vector(beta_decomp$P_beta)),
  S_beta = cor(as.vector(env_dist), as.vector(beta_decomp$S_beta))
)

cat("Correlation with environment proxy:\n")
print(round(correlations, 3))

# Interpret strongest correlation
strongest_env_component <- names(which.max(abs(correlations)))
strongest_corr <- correlations[strongest_env_component]

cat("\nStrongest environmental correlation:", strongest_env_component, "(r =", round(strongest_corr, 3), ")\n")

if (strongest_env_component == "R_beta" && abs(strongest_corr) > 0.5) {
  cat("Interpretation: Environmental filtering dominates - different environments select different species\n")
} else if (strongest_env_component == "E_beta" && abs(strongest_corr) > 0.5) {
  cat("Interpretation: Competitive dynamics vary across environments - abundance patterns differ\n")
} else if (strongest_env_component == "P_beta" && abs(strongest_corr) > 0.5) {
  cat("Interpretation: Phylogenetic constraints vary across environments - evolutionary filtering\n")
} else {
  cat("Interpretation: No strong environmental pattern - may indicate neutral processes or complex interactions\n")
}

cat("\nStep 7: Comparison with Traditional Approaches\n")
cat("---------------------------------------------\n")

# Compare framework insights with traditional PERMANOVA
if (requireNamespace("vegan", quietly = TRUE)) {
  cat("Running PERMANOVA on traditional metrics...\n")
  
  # PERMANOVA on Bray-Curtis
  permanova_bray <- adonis2(beta_bray ~ SampleType, data = sample_metadata, permutations = 99)
  cat("PERMANOVA on Bray-Curtis - R²:", round(permanova_bray$R2[1], 3), 
      "p-value:", round(permanova_bray$`Pr(>F)`[1], 3), "\n")
  
  # PERMANOVA on Jaccard
  permanova_jaccard <- adonis2(beta_jaccard ~ SampleType, data = sample_metadata, permutations = 99)
  cat("PERMANOVA on Jaccard - R²:", round(permanova_jaccard$R2[1], 3), 
      "p-value:", round(permanova_jaccard$`Pr(>F)`[1], 3), "\n")
  
  cat("\nFramework vs Traditional comparison:\n")
  cat("- Traditional: 'SampleType explains", round(permanova_bray$R2[1] * 100, 1), "% of Bray-Curtis variation'\n")
  cat("- Framework: 'SampleType primarily affects", strongest_env_component, 
      "component (", round(abs(strongest_corr) * 100, 1), "% correlation)'\n")
  cat("- Added insight: Framework reveals WHICH ASPECT of diversity drives the pattern\n")
}

cat("\nStep 8: Novel Ecological Questions\n")
cat("---------------------------------\n")

cat("The beta diversity framework enables novel questions:\n\n")

# Question 1: Scale-dependent mechanisms
cat("1. Scale-dependent assembly mechanisms:\n")
cat("   - Within samples (alpha): [Would need alpha analysis]\n")
cat("   - Between samples (beta):", dominant_component_overall, "dominates\n")
cat("   - Insight: Different mechanisms may operate at different scales\n\n")

# Question 2: Sample uniqueness
cat("2. Sample uniqueness analysis:\n")
total_beta_contrib <- rowSums(cbind(
  beta_decomp$sample_R_beta, beta_decomp$sample_E_beta,
  beta_decomp$sample_P_beta, beta_decomp$sample_S_beta
))

unique_samples <- which(total_beta_contrib > quantile(total_beta_contrib, 0.8, na.rm = TRUE))
cat("   - Most unique samples (high beta contribution):", paste(rownames(community_matrix)[unique_samples], collapse = ", "), "\n")
cat("   - These samples have distinct species/abundance/phylogenetic composition\n")
cat("   - Conservation priority: Preserve these samples for regional diversity\n\n")

# Question 3: Component-specific hotspots
cat("3. Component-specific biodiversity hotspots:\n")
if (!is.na(beta_decomp$sample_R_beta[1])) {
  R_hotspot <- which.max(beta_decomp$sample_R_beta)
  E_hotspot <- which.max(beta_decomp$sample_E_beta)
  P_hotspot <- which.max(beta_decomp$sample_P_beta)
  
  cat("   - Species composition hotspot:", rownames(community_matrix)[R_hotspot], "\n")
  cat("   - Abundance pattern hotspot:", rownames(community_matrix)[E_hotspot], "\n")
  cat("   - Phylogenetic hotspot:", rownames(community_matrix)[P_hotspot], "\n")
  cat("   - Insight: Different samples important for different aspects of diversity\n\n")
}

cat("Step 9: Framework Validation Summary\n")
cat("-----------------------------------\n")

# Summarize validation results
validation_score <- 0
total_tests <- 0

# Test 1: Correlation with traditional metrics
if (validation$jaccard_vs_R > 0.5) {
  cat("✓ R_beta correlates well with Jaccard (r =", round(validation$jaccard_vs_R, 3), ")\n")
  validation_score <- validation_score + 1
} else {
  cat("✗ R_beta - Jaccard correlation weak (r =", round(validation$jaccard_vs_R, 3), ")\n")
}
total_tests <- total_tests + 1

if (validation$bray_vs_RE > 0.5) {
  cat("✓ R_beta + E_beta correlates well with Bray-Curtis (r =", round(validation$bray_vs_RE, 3), ")\n")
  validation_score <- validation_score + 1
} else {
  cat("✗ R_beta + E_beta - Bray-Curtis correlation weak (r =", round(validation$bray_vs_RE, 3), ")\n")
}
total_tests <- total_tests + 1

# Test 2: Component orthogonality
if (mean_off_diag < 0.4) {
  cat("✓ Components reasonably orthogonal (mean r =", round(mean_off_diag, 3), ")\n")
  validation_score <- validation_score + 1
} else {
  cat("✗ Components highly correlated (mean r =", round(mean_off_diag, 3), ")\n")
}
total_tests <- total_tests + 1

# Test 3: Ecological interpretability
if (abs(strongest_corr) > 0.3) {
  cat("✓ Clear environmental pattern detected (r =", round(strongest_corr, 3), ")\n")
  validation_score <- validation_score + 1
} else {
  cat("✗ Weak environmental pattern (r =", round(strongest_corr, 3), ")\n")
}
total_tests <- total_tests + 1

cat("\nValidation Score:", validation_score, "/", total_tests, 
    "(", round(validation_score/total_tests * 100, 1), "%)\n")

if (validation_score >= total_tests * 0.75) {
  cat("✅ Framework validation: STRONG - Ready for further development\n")
} else if (validation_score >= total_tests * 0.5) {
  cat("⚠️ Framework validation: MODERATE - Needs refinement\n")
} else {
  cat("❌ Framework validation: WEAK - Major revision needed\n")
}

cat("\nStep 10: Create Visualizations\n")
cat("-----------------------------\n")

# Create visualization of decomposition results
tryCatch({
  # Basic heatmap visualization
  pdf("beta_framework_validation.pdf", width = 12, height = 10)
  
  # Plot 1: Component heatmaps
  par(mfrow = c(2, 2))
  
  image(beta_decomp$R_beta, main = "R_β: Species Turnover", 
        col = heat.colors(20), xlab = "Sample", ylab = "Sample")
  
  image(beta_decomp$E_beta, main = "E_β: Abundance Turnover",
        col = heat.colors(20), xlab = "Sample", ylab = "Sample")
  
  image(beta_decomp$P_beta, main = "P_β: Phylogenetic Turnover", 
        col = heat.colors(20), xlab = "Sample", ylab = "Sample")
  
  image(beta_decomp$S_beta, main = "S_β: Spatial Turnover",
        col = heat.colors(20), xlab = "Sample", ylab = "Sample")
  
  # Plot 2: Component contributions by sample type
  par(mfrow = c(1, 1))
  
  # Create barplot data
  if (!all(is.na(beta_by_type$R_beta_mean))) {
    barplot_data <- as.matrix(beta_by_type[, c("R_beta_mean", "E_beta_mean", "P_beta_mean", "S_beta_mean")])
    rownames(barplot_data) <- beta_by_type$SampleType
    
    barplot(t(barplot_data), 
            beside = TRUE,
            col = c("red", "blue", "green", "purple"),
            legend = c("R_β", "E_β", "P_β", "S_β"),
            main = "Beta Diversity Components by Sample Type",
            xlab = "Sample Type",
            ylab = "Mean Component Value")
  }
  
  dev.off()
  cat("✓ Visualizations saved to: beta_framework_validation.pdf\n")
  
}, error = function(e) {
  cat("✗ Visualization creation failed:", conditionMessage(e), "\n")
})

cat("\n=== Beta Diversity Framework Testing Complete ===\n")
cat("\nSUMMARY:\n")
cat("- Framework successfully decomposed beta diversity into R, E, P, S components\n")
cat("- Validation score:", validation_score, "/", total_tests, "\n")
cat("- Dominant component:", dominant_component_overall, "\n")
cat("- Key insight: Framework reveals", strongest_env_component, "drives environmental patterns\n")
cat("- Novel ecological questions enabled by component-specific analysis\n")

if (validation_score >= total_tests * 0.75) {
  cat("\n🚀 CONCLUSION: Framework shows strong promise for ecological applications!\n")
  cat("Next steps: Refine algorithms, test on more datasets, integrate with diversityGPT\n")
} else {
  cat("\n⚠️ CONCLUSION: Framework needs refinement before broader application\n")
  cat("Next steps: Improve mathematical formulation, validate on simulated data\n")
}

cat("\n" , "=" , rep("=", 50), collapse = "", "\n")