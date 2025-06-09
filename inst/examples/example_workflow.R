# Example diversityGPT Workflow
# This script demonstrates the complete workflow from data to AI interpretation

library(diversityGPT)

# Step 1: Load example data
data(example_physeq)
print("Loaded example phyloseq object:")
print(example_physeq)

# Step 2: Calculate diversity metrics
cat("\n=== CALCULATING DIVERSITY METRICS ===\n")
div_results <- calculate_diversity(
  example_physeq,
  metrics = c("shannon", "simpson", "chao1", "observed"),
  groups = "Group"
)

print(div_results)

# Step 3: Perform consensus analysis
cat("\n=== CONSENSUS ANALYSIS ===\n")
consensus <- consensus_diversity(
  div_results,
  method = "weighted_mean",
  groups = "Group"
)

print(consensus)

# Step 4: AI-powered interpretation (requires API key)
cat("\n=== AI INTERPRETATION ===\n")
if (check_api_setup()) {
  
  interpretation <- interpret_diversity(
    consensus,
    context = list(
      environment = "synthetic_microbiome",
      condition = "high_vs_low_diversity_simulation",
      organism = "bacteria",
      additional_info = "This is simulated data for testing purposes"
    ),
    hypothesis_generation = TRUE
  )
  
  print(interpretation)
  
} else {
  cat("Skipping AI interpretation - no API key configured\n")
  cat("To enable AI features, add your API key to .Renviron:\n")
  cat("ANTHROPIC_API_KEY=your-key-here\n")
}

# Step 5: Compare different consensus methods
cat("\n=== COMPARING CONSENSUS METHODS ===\n")
methods <- c("weighted_mean", "correlation_weighted")

for (method in methods) {
  cat("\nMethod:", method, "\n")
  cons <- consensus_diversity(div_results, method = method, groups = "Group")
  cat("Dominant metric:", cons$interpretation$dominant_metric, "\n")
  cat("Conflict status:", cons$interpretation$conflict_status, "\n")
}

cat("\n=== WORKFLOW COMPLETE ===\n")
cat("This demonstrates the core diversityGPT functionality:\n")
cat("1. Calculate multiple diversity metrics\n")
cat("2. Resolve conflicts with consensus analysis\n") 
cat("3. Get AI-powered biological interpretation\n")
cat("4. Compare different analytical approaches\n")