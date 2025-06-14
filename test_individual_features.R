#!/usr/bin/env Rscript
# Individual Feature Testing Commands
# Copy and paste these into your R console to test specific features

cat("COPY AND PASTE THESE COMMANDS INTO YOUR R CONSOLE:\n")
cat("=================================================\n\n")

cat("# 1. LOAD PACKAGE AND DATA\n")
cat("library(diversityGPT)\n")
cat("library(phyloseq)\n")
cat("data(GlobalPatterns)\n")
cat("subset_data <- create_demo_subset(GlobalPatterns, n_samples = 10, n_taxa = 50)\n")
cat("universal_info <- extract_universal_information(subset_data)\n\n")

cat("# 2. TEST ASSEMBLY MECHANISM DETECTION\n")
cat("env_data <- data.frame(\n")
cat("  sample_names = sample_names(subset_data),\n") 
cat("  pH = rnorm(10, 7, 0.5),\n")
cat("  temperature = rnorm(10, 25, 3)\n")
cat(")\n")
cat("mechanisms <- detect_assembly_mechanisms(universal_info, environmental_data = env_data)\n")
cat("print(mechanisms)\n\n")

cat("# 3. TEST HYPOTHESIS GENERATION\n")
cat("hypotheses <- generate_ecological_hypotheses(\n")
cat("  universal_info,\n")
cat("  assembly_mechanisms = mechanisms,\n")
cat("  study_context = list(environment = 'soil', organism = 'bacteria'),\n")
cat("  hypothesis_types = c('mechanistic', 'predictive')\n")
cat(")\n")
cat("print(hypotheses)\n\n")

cat("# 4. TEST MULTI-STEP ANALYSIS (OFFLINE)\n")
cat("analysis <- llm_multi_step_analysis(\n")
cat("  universal_info,\n")
cat("  assembly_mechanisms = mechanisms,\n")
cat("  hypotheses = hypotheses,\n")
cat("  llm_provider = 'none'  # Offline mode\n")
cat(")\n")
cat("print(analysis)\n\n")

cat("# 5. TEST LITERATURE SEARCH\n")
cat("literature <- search_literature(\n")
cat("  universal_info,\n")
cat("  study_context = list(environment = 'marine', organism = 'bacteria'),\n")
cat("  max_papers = 3,\n")
cat("  llm_synthesis = FALSE\n")
cat(")\n")
cat("print(literature)\n\n")

cat("# 6. INSPECT DETAILED RESULTS\n")
cat("# Assembly mechanisms:\n")
cat("mechanisms$mechanisms\n")
cat("mechanisms$interpretation\n\n")

cat("# Hypotheses:\n")
cat("hypotheses$hypotheses\n")
cat("hypotheses$experimental_designs[[1]]  # First experimental design\n")
cat("hypotheses$predictions[[1]]           # First prediction\n\n")

cat("# Literature:\n")
cat("literature$relevance_ranking\n")
cat("literature$synthesis\n")
cat("literature$research_gaps\n\n")

cat("# 7. VISUALIZATION\n")
cat("if (nrow(literature$relevance_ranking) > 0) {\n")
cat("  plot(literature, type = 'relevance')\n")
cat("  plot(literature, type = 'timeline')\n")
cat("}\n\n")

cat("# 8. WITH LLM PROVIDER (if you have API keys)\n")
cat("# Uncomment and run these if you have API keys set up:\n")
cat("# analysis_llm <- llm_multi_step_analysis(\n")
cat("#   universal_info,\n")
cat("#   assembly_mechanisms = mechanisms,\n")
cat("#   hypotheses = hypotheses,\n")
cat("#   reasoning_depth = 'deep',\n")
cat("#   llm_provider = 'anthropic'  # or 'openai'\n")
cat("# )\n")
cat("# print(analysis_llm)\n\n")

cat("# literature_llm <- search_literature(\n")
cat("#   universal_info,\n")
cat("#   study_context = list(environment = 'marine'),\n")
cat("#   llm_synthesis = TRUE\n")
cat("# )\n\n")

cat("# 9. ADVANCED USAGE\n")
cat("# Test with different hypothesis types:\n")
cat("hyp_experimental <- generate_ecological_hypotheses(\n")
cat("  universal_info,\n")
cat("  hypothesis_types = c('experimental', 'comparative'),\n")
cat("  max_hypotheses = 5\n")
cat(")\n\n")

cat("# Test mechanism detection without environmental data:\n")
cat("mechanisms_simple <- detect_assembly_mechanisms(universal_info)\n\n")

cat("# Test literature search with multiple databases:\n")
cat("literature_multi <- search_literature(\n")
cat("  universal_info,\n")
cat("  search_databases = 'all',  # PubMed, CrossRef, Semantic Scholar\n")
cat("  max_papers = 8\n")
cat(")\n\n")

cat("=================================================\n")
cat("END OF COMMANDS - Copy and paste the sections you want to test!\n")