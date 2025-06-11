# Demonstration: Ecological Intelligence in diversityGPT
# This script showcases the complete ecological intelligence workflow

library(devtools)
load_all()

cat("=== diversityGPT Ecological Intelligence Demo ===\n\n")

# Step 1: Create realistic ecological dataset
cat("Step 1: Creating ecological dataset...\n")
set.seed(123)
demo_data <- create_demo_phyloseq(n_samples = 20, n_taxa = 40)

# Add environmental metadata
env_data <- data.frame(
  pH = rnorm(20, mean = 7, sd = 0.5),
  temperature = rnorm(20, mean = 25, sd = 3),
  nutrient = runif(20, min = 1, max = 10)
)
rownames(env_data) <- sample_names(demo_data)
sample_data(demo_data) <- sample_data(env_data)

cat("✓ Created dataset with", ntaxa(demo_data), "taxa and", 
    nsamples(demo_data), "samples\n\n")

# Step 2: Extract universal information
cat("Step 2: Extracting universal information components...\n")
universal_info <- extract_universal_information(demo_data)
cat("✓ Decomposed diversity into R, E, P, S components\n")
cat("  - Components explain", 
    round(sum(universal_info$components_summary$variance_explained), 1), 
    "% of total variance\n\n")

# Step 3: Detect assembly mechanisms
cat("Step 3: Detecting community assembly mechanisms...\n")
mechanisms <- detect_assembly_mechanisms(
  universal_info,
  environmental_data = env_data,
  method = "comprehensive"
)
cat("✓ Assembly mechanism analysis complete\n\n")

# Step 4: Generate ecological hypotheses
cat("Step 4: Generating testable hypotheses...\n")
hypotheses <- generate_ecological_hypotheses(
  universal_info,
  assembly_mechanisms = mechanisms,
  study_context = list(
    environment = "temperate_soil",
    organism = "bacteria",
    research_focus = "nutrient cycling"
  ),
  hypothesis_types = c("mechanistic", "predictive")
)
cat("✓ Generated ecological hypotheses\n\n")

# Step 5: Search scientific literature
cat("Step 5: Searching and integrating literature...\n")
literature <- search_literature(
  universal_info,
  assembly_mechanisms = mechanisms,
  hypotheses = hypotheses,
  study_context = list(
    environment = "soil_microbiome",
    keywords = c("assembly", "nutrients", "pH")
  ),
  search_databases = "pubmed",
  max_papers = 5
)
cat("✓ Literature search complete\n\n")

# Step 6: Summary of findings
cat("=== ECOLOGICAL INTELLIGENCE SUMMARY ===\n\n")

cat("1. DIVERSITY PATTERNS:\n")
cat("   - Primary driver: ", 
    names(which.max(universal_info$components_summary$variance_explained)), 
    " component\n")
cat("   - Metric relationships: ", 
    length(universal_info$relationships), " discovered\n\n")

cat("2. ASSEMBLY MECHANISMS:\n")
# Print detected mechanisms from the S3 object
print(mechanisms)
cat("\n")

cat("3. TESTABLE HYPOTHESES:\n")
# Print hypotheses summary
print(hypotheses)
cat("\n")

cat("4. LITERATURE CONNECTIONS:\n")
# Print literature summary
print(literature)
cat("\n")

cat("5. INTEGRATED INSIGHTS:\n")
cat("   The analysis reveals a complex ecological system where:\n")
cat("   - Diversity is primarily structured by",
    names(which.max(universal_info$components_summary$variance_explained)),
    "patterns\n")
cat("   - Assembly is likely governed by multiple interacting mechanisms\n")
cat("   - Several testable hypotheses can advance understanding\n")
cat("   - Literature provides context for interpretation\n\n")

# Optional: Multi-step LLM analysis
if (check_api_setup()) {
  cat("Step 6: Running AI-powered synthesis...\n")
  llm_insights <- llm_multi_step_analysis(
    universal_info = universal_info,
    assembly_mechanisms = mechanisms,
    hypotheses = hypotheses,
    literature = literature,
    study_context = list(
      study_system = "temperate soil microbiome",
      research_question = "What controls bacterial diversity in nutrient-rich soils?"
    ),
    reasoning_depth = "standard"
  )
  cat("✓ AI synthesis complete\n")
  print(llm_insights)
} else {
  cat("Note: Configure LLM API for AI-powered insights\n")
  cat("      Use: configure_llm(provider = 'anthropic', api_key = 'your-key')\n")
}

cat("\n=== Demo Complete ===\n")
cat("The diversityGPT ecological intelligence features transform\n")
cat("mathematical patterns into biological understanding!\n")