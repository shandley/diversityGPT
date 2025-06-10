#!/usr/bin/env Rscript
# Test Phase 3 functionality

# Load the package
devtools::load_all()

# Create test data
components <- data.frame(
  sample = paste0("S", 1:10),
  R = runif(10, 0.2, 0.8),
  E = runif(10, 0.3, 0.7),
  stringsAsFactors = FALSE
)

transformation_matrix <- diag(0.8, 4)
rownames(transformation_matrix) <- c("shannon", "simpson", "observed", "chao1")
colnames(transformation_matrix) <- c("shannon", "simpson", "observed", "chao1")

universal_info <- list(
  information_components = components,
  transformation_matrix = transformation_matrix,
  quality_metrics = list(overall_quality = 0.8)
)
class(universal_info) <- "universal_information"

# Test 1: Assembly Mechanism Detection
cat("Testing detect_assembly_mechanisms...\n")
mechanisms <- detect_assembly_mechanisms(universal_info)
print(mechanisms)
cat("\n")

# Test 2: Hypothesis Generation
cat("Testing generate_ecological_hypotheses...\n")
hypotheses <- generate_ecological_hypotheses(
  universal_info,
  hypothesis_types = c("mechanistic", "predictive")
)
print(hypotheses)
cat("\n")

# Test 3: Multi-step LLM Analysis
cat("Testing llm_multi_step_analysis...\n")
analysis <- llm_multi_step_analysis(
  universal_info,
  assembly_mechanisms = mechanisms,
  hypotheses = hypotheses,
  llm_provider = "none"  # Use offline mode
)
print(analysis)
cat("\n")

# Test 4: Literature Search
cat("Testing search_literature...\n")
literature <- search_literature(
  universal_info,
  search_databases = "pubmed",
  max_papers = 3,
  llm_synthesis = FALSE
)
print(literature)

cat("\nPhase 3 functionality tests completed successfully!\n")