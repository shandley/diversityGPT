# Create example analyses showcasing different data types and use cases

library(phyloseq)

# Source required functions
source("R/calculate_diversity.R")
source("R/universal_transformations.R")
source("R/universal_information.R")
source("R/consensus_diversity.R")
source("R/llm_integration.R")

# Create output directory for examples
output_dir <- "inst/examples"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Helper function to create a complete analysis
create_complete_analysis <- function(physeq, name, description) {
  
  message("\n", paste(rep("=", 60), collapse = ""))
  message("Creating example analysis: ", name)
  message(paste(rep("=", 60), collapse = ""))
  
  # 1. Calculate diversity metrics
  message("Calculating diversity metrics...")
  div_metrics <- calculate_diversity(
    physeq,
    metrics = c("shannon", "simpson", "observed", "chao1")
  )
  
  # 2. Extract universal information
  message("Extracting universal information...")
  universal_info <- extract_universal_information(
    physeq,
    include_phylogenetic = !is.null(phy_tree(physeq, errorIfNULL = FALSE))
  )
  
  # 3. Perform example transformations
  message("Demonstrating metric transformations...")
  
  # Transform from Shannon to other metrics
  shannon_mean <- mean(div_metrics$shannon, na.rm = TRUE)
  transform_from_shannon <- universal_diversity_transform(
    source_metrics = c(shannon = shannon_mean),
    target_metrics = c("simpson", "observed", "chao1"),
    transformation_matrix = universal_info$transformation_matrix
  )
  
  # Transform from multiple metrics
  multi_source <- c(
    shannon = shannon_mean,
    observed = mean(div_metrics$observed, na.rm = TRUE)
  )
  transform_from_multi <- universal_diversity_transform(
    source_metrics = multi_source,
    target_metrics = c("simpson", "chao1"),
    transformation_matrix = universal_info$transformation_matrix
  )
  
  # 4. Create consensus diversity
  message("Creating consensus diversity...")
  consensus_result <- consensus_diversity(
    div_metrics,
    method = "information_theoretic"
  )
  
  # 5. Generate interpretation (if API available)
  interpretation <- NULL
  if (check_api_setup()) {
    message("Generating AI interpretation...")
    interpretation <- tryCatch({
      interpret_diversity(
        div_metrics,
        consensus_results = consensus_result,
        context = list(
          study_type = "microbiome",
          environment = description
        )
      )
    }, error = function(e) {
      message("AI interpretation skipped: ", e$message)
      NULL
    })
  }
  
  # 6. Create summary
  analysis_summary <- list(
    name = name,
    description = description,
    dataset_info = list(
      n_samples = nsamples(physeq),
      n_taxa = ntaxa(physeq),
      has_tree = !is.null(phy_tree(physeq, errorIfNULL = FALSE))
    ),
    diversity_summary = list(
      shannon_mean = mean(div_metrics$shannon, na.rm = TRUE),
      shannon_sd = sd(div_metrics$shannon, na.rm = TRUE),
      simpson_mean = mean(div_metrics$simpson, na.rm = TRUE),
      observed_mean = mean(div_metrics$observed, na.rm = TRUE)
    ),
    universal_quality = list(
      mean_r_squared = universal_info$deconvolution_quality$mean_r_squared,
      overall_quality = universal_info$deconvolution_quality$overall_quality
    ),
    transformation_examples = list(
      from_shannon = transform_from_shannon,
      from_multiple = transform_from_multi
    ),
    consensus_diversity = consensus_result,
    interpretation = interpretation
  )
  
  # Save the complete analysis
  output_file <- file.path(output_dir, paste0(gsub(" ", "_", tolower(name)), "_analysis.rds"))
  saveRDS(analysis_summary, output_file)
  
  message("Saved complete analysis to: ", output_file)
  
  return(analysis_summary)
}

# Create example analyses for different use cases

# 1. Environmental gradient analysis
data("GlobalPatterns")
env_samples <- subset_samples(GlobalPatterns, SampleType %in% c("Soil", "Ocean", "Freshwater"))
env_analysis <- create_complete_analysis(
  env_samples,
  "Environmental Gradient",
  "Comparison of microbial diversity across different environments"
)

# 2. Human gut enterotype analysis
data("enterotype")
gut_analysis <- create_complete_analysis(
  enterotype,
  "Human Gut Enterotypes",
  "Analysis of human gut microbiome enterotype patterns"
)

# 3. Experimental treatment analysis
data("soilrep")
# Compare warmed vs control
treatment_analysis <- create_complete_analysis(
  soilrep,
  "Climate Change Experiment",
  "Soil microbiome response to warming treatment"
)

# Create a summary report
message("\n", paste(rep("=", 60), collapse = ""))
message("EXAMPLE ANALYSES SUMMARY")
message(paste(rep("=", 60), collapse = ""))

analyses <- list(env_analysis, gut_analysis, treatment_analysis)

for (analysis in analyses) {
  message(sprintf("\n%s:", analysis$name))
  message(sprintf("  Samples: %d, Taxa: %d", 
                  analysis$dataset_info$n_samples,
                  analysis$dataset_info$n_taxa))
  message(sprintf("  Mean Shannon: %.2f (±%.2f)",
                  analysis$diversity_summary$shannon_mean,
                  analysis$diversity_summary$shannon_sd))
  message(sprintf("  Universal R²: %.3f (%s)",
                  analysis$universal_quality$mean_r_squared,
                  analysis$universal_quality$overall_quality))
  message(sprintf("  Consensus diversity: %.2f",
                  analysis$consensus_diversity$consensus_value))
}

message("\nExample analyses created in: ", output_dir)
message("\nThese demonstrate:")
message("  - Diversity calculation across different data types")
message("  - Universal metric transformation capabilities")  
message("  - Consensus diversity computation")
message("  - AI interpretation integration (if API configured)")
message("  - Complete workflow from raw data to insights")