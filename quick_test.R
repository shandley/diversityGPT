#!/usr/bin/env Rscript
# Quick Test of Phase 3 Features

library(diversityGPT)
library(phyloseq)

# Load data
data(GlobalPatterns)
subset_data <- create_demo_subset(GlobalPatterns, n_samples = 8, n_taxa = 30)
universal_info <- extract_universal_information(subset_data)

# Quick test of each major function
cat("Testing detect_assembly_mechanisms...\n")
mechanisms <- detect_assembly_mechanisms(universal_info)
cat("✓ Assembly mechanisms detected\n\n")

cat("Testing generate_ecological_hypotheses...\n")
hypotheses <- generate_ecological_hypotheses(universal_info, max_hypotheses = 2)
cat("✓ Hypotheses generated\n\n")

cat("Testing llm_multi_step_analysis...\n")
analysis <- llm_multi_step_analysis(universal_info, llm_provider = "none")
cat("✓ Multi-step analysis completed\n\n")

cat("Testing search_literature...\n")
literature <- search_literature(universal_info, max_papers = 2, llm_synthesis = FALSE)
cat("✓ Literature search completed\n\n")

cat("All Phase 3 features working correctly!\n")