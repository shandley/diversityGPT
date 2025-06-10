#!/usr/bin/env Rscript
# Comprehensive Phase 3 Feature Testing Script
# Run this to test all new ecological intelligence capabilities

# Load the package
library(diversityGPT)
library(phyloseq)

cat("=====================================\n")
cat("Phase 3 Ecological Intelligence Tests\n")
cat("=====================================\n\n")

# Load example data
data(GlobalPatterns)
subset_data <- create_demo_subset(GlobalPatterns, n_samples = 15, n_taxa = 100)

cat("1. UNIVERSAL INFORMATION EXTRACTION\n")
cat("===================================\n")
universal_info <- extract_universal_information(subset_data)
print(universal_info)
cat("\n")

# Create some mock environmental data
env_data <- data.frame(
  sample_names = sample_names(subset_data),
  pH = rnorm(15, 7, 0.5),
  temperature = rnorm(15, 25, 3),
  nutrients = rnorm(15, 10, 2),
  stringsAsFactors = FALSE
)

cat("2. ASSEMBLY MECHANISM DETECTION\n")
cat("===============================\n")
mechanisms <- detect_assembly_mechanisms(
  universal_info,
  environmental_data = env_data,
  method = "comprehensive"
)
print(mechanisms)
cat("\n")

cat("3. ECOLOGICAL HYPOTHESIS GENERATION\n")
cat("===================================\n")
hypotheses <- generate_ecological_hypotheses(
  universal_info = universal_info,
  assembly_mechanisms = mechanisms,
  study_context = list(
    environment = "marine_sediment",
    organism = "bacteria",
    condition = "depth_gradient"
  ),
  hypothesis_types = c("mechanistic", "predictive", "experimental"),
  max_hypotheses = 3
)
print(hypotheses)
cat("\n")

cat("4. MULTI-STEP LLM ANALYSIS (Offline Mode)\n")
cat("=========================================\n")
analysis_offline <- llm_multi_step_analysis(
  universal_info = universal_info,
  assembly_mechanisms = mechanisms,
  hypotheses = hypotheses,
  study_context = list(
    environment = "marine_sediment",
    organism = "bacteria",
    condition = "depth_gradient"
  ),
  reasoning_depth = "standard",
  llm_provider = "none"  # Offline mode
)
print(analysis_offline)
cat("\n")

cat("5. LITERATURE SEARCH\n")
cat("====================\n")
literature <- search_literature(
  universal_info = universal_info,
  assembly_mechanisms = mechanisms,
  study_context = list(
    environment = "marine",
    organism = "bacteria",
    condition = "depth"
  ),
  search_databases = "pubmed",
  max_papers = 5,
  llm_synthesis = FALSE
)
print(literature)
cat("\n")

cat("6. VISUALIZATION TESTS\n")
cat("======================\n")

# Plot literature results if any papers found
if (nrow(literature$relevance_ranking) > 0) {
  cat("Plotting literature relevance scores...\n")
  plot(literature, type = "relevance")
  
  cat("Plotting publication timeline...\n")
  plot(literature, type = "timeline")
}

cat("\n7. DETAILED INSPECTION OF RESULTS\n")
cat("=================================\n")

# Inspect assembly mechanisms in detail
cat("Assembly Mechanism Details:\n")
if (nrow(mechanisms$mechanisms) > 0) {
  for (i in 1:nrow(mechanisms$mechanisms)) {
    mech <- mechanisms$mechanisms[i, ]
    cat(sprintf("  %s: confidence=%.3f, p-value=%.3f\n", 
                mech$mechanism, mech$confidence, mech$p_value))
  }
}
cat("\n")

# Inspect hypotheses in detail
cat("Hypothesis Details:\n")
if (nrow(hypotheses$hypotheses) > 0) {
  for (i in 1:nrow(hypotheses$hypotheses)) {
    hyp <- hypotheses$hypotheses[i, ]
    cat(sprintf("  [%s] %s\n", toupper(hyp$type), hyp$hypothesis))
    cat(sprintf("    Novelty: %.2f, Testability: %.2f\n", hyp$novelty, hyp$testability))
    
    # Show experimental design if available
    if (length(hypotheses$experimental_designs) >= i) {
      design <- hypotheses$experimental_designs[[i]]
      cat(sprintf("    Approach: %s\n", design$approach))
      cat(sprintf("    Timeline: %s\n", design$timeline))
    }
    cat("\n")
  }
}

# Show literature synthesis
if (length(literature$synthesis$key_themes) > 0) {
  cat("Literature Key Themes:\n")
  for (theme in literature$synthesis$key_themes) {
    cat(sprintf("  - %s\n", theme))
  }
  cat("\n")
}

# Show research gaps
if (length(literature$research_gaps) > 0) {
  cat("Research Gaps Identified:\n")
  for (gap_name in names(literature$research_gaps)) {
    gap <- literature$research_gaps[[gap_name]]
    cat(sprintf("  %s: %s\n", gap_name, gap$description))
  }
}

cat("\n=====================================\n")
cat("Phase 3 Testing Complete!\n")
cat("=====================================\n")

# Summary statistics
cat("\nSUMMARY:\n")
cat(sprintf("- Detected %d assembly mechanisms\n", nrow(mechanisms$mechanisms)))
cat(sprintf("- Generated %d hypotheses\n", nrow(hypotheses$hypotheses)))
cat(sprintf("- Found %d relevant papers\n", nrow(literature$relevance_ranking)))
cat(sprintf("- Identified %d research gaps\n", length(literature$research_gaps)))

cat("\nAll Phase 3 features tested successfully!\n")