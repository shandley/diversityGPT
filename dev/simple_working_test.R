# diversityGPT: Simple Working Test
# Direct demonstration of core capabilities

devtools::load_all()
library(phyloseq)

cat("diversityGPT: Simple Working Demo\n")
cat("=================================\n\n")

# Load built-in data
data(GlobalPatterns)
ps <- prune_samples(sample_names(GlobalPatterns)[1:10], GlobalPatterns)

cat("Using 10 samples from GlobalPatterns dataset\n\n")

# 1. Calculate diversity metrics
cat("1. Calculating diversity metrics...\n")
div_results <- calculate_diversity(ps)
print(div_results[1:5, c("shannon", "simpson", "observed")])

# 2. Consensus analysis
cat("\n2. Finding consensus across metrics...\n")
consensus <- consensus_diversity(div_results)
print(consensus)

# 3. Extract universal information
cat("\n3. Extracting universal information components...\n")
universal_info <- extract_universal_information(ps, include_phylogenetic = FALSE)

# Show decomposition quality
cat("\nDecomposition quality:\n")
cat("- Overall:", universal_info$deconvolution_quality$overall_quality, "\n")
cat("- Mean R²:", round(universal_info$deconvolution_quality$mean_r_squared, 3), "\n")

# 4. Show information components
cat("\n4. Information components for first 3 samples:\n")
components <- universal_info$information_components[1:3, c("sample_id", "R_component", "E_component")]
print(round(components[,-1], 3))

# 5. Simple visualization
cat("\n5. Creating visualizations...\n")
par(mfrow = c(1, 2))

# Quality plot
plot(universal_info, type = "quality")

# Component proportions
comp_props <- universal_info$information_components[1:5, c("R_proportion", "E_proportion")]
barplot(
  t(as.matrix(comp_props)),
  names.arg = paste0("S", 1:5),
  col = c("coral", "skyblue"),
  legend = c("Richness", "Evenness"),
  main = "Information Components"
)

par(mfrow = c(1, 1))

# 6. AI interpretation (if available)
if (!is.null(get_api_key("anthropic")) || !is.null(get_api_key("openai"))) {
  cat("\n6. AI interpretation of patterns...\n")
  interpretation <- interpret_diversity(
    consensus,
    context = list(
      environment = "human_microbiome",
      condition = "diverse_body_sites"
    )
  )
  cat("AI says:", substr(interpretation$interpretation, 1, 200), "...\n")
} else {
  cat("\n6. AI interpretation available with API key\n")
}

cat("\n✅ Core features demonstrated successfully!\n")