# Generate precomputed datasets with universal transformations
# This creates pre-analyzed datasets for quick loading in the Shiny app

library(phyloseq)
library(vegan)
library(dplyr)
library(tidyr)

# Source all required functions
source("R/calculate_diversity.R")
source("R/universal_transformations.R")
source("R/consensus_diversity.R")  # Fixed filename
source("R/universal_information.R")  # Added this too

# Create output directory
output_dir <- "inst/data"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Function to create a demo subset with stratified sampling
create_stratified_subset <- function(physeq, n_samples = 50, n_taxa = 1000, 
                                   stratify_by = NULL, seed = 123) {
  set.seed(seed)
  
  # Original size
  orig_samples <- nsamples(physeq)
  orig_taxa <- ntaxa(physeq)
  
  # Subset samples
  if (nsamples(physeq) > n_samples) {
    if (!is.null(stratify_by) && stratify_by %in% names(sample_data(physeq))) {
      # Stratified sampling
      metadata <- data.frame(sample_data(physeq))
      metadata$sample_id <- sample_names(physeq)
      
      # Sample proportionally from each group
      keep_samples <- metadata %>%
        group_by(!!sym(stratify_by)) %>%
        sample_frac(n_samples / nsamples(physeq)) %>%
        pull(sample_id)
      
      # Ensure we have exactly n_samples
      if (length(keep_samples) > n_samples) {
        keep_samples <- sample(keep_samples, n_samples)
      } else if (length(keep_samples) < n_samples) {
        remaining <- setdiff(sample_names(physeq), keep_samples)
        n_more <- min(n_samples - length(keep_samples), length(remaining))
        if (n_more > 0) {
          keep_samples <- c(keep_samples, sample(remaining, n_more))
        }
      }
    } else {
      # Random sampling
      keep_samples <- sample(sample_names(physeq), n_samples)
    }
    
    physeq <- prune_samples(keep_samples, physeq)
  }
  
  # Subset taxa to most abundant
  if (ntaxa(physeq) > n_taxa) {
    taxon_sums <- taxa_sums(physeq)
    keep_taxa <- names(sort(taxon_sums, decreasing = TRUE)[1:n_taxa])
    physeq <- prune_taxa(keep_taxa, physeq)
  }
  
  message(sprintf("Created subset: %d samples (from %d), %d taxa (from %d)",
                  nsamples(physeq), orig_samples, ntaxa(physeq), orig_taxa))
  
  return(physeq)
}

# Function to process a single dataset
process_and_save_dataset <- function(dataset_name, physeq, output_name, 
                                   subset_samples = NULL, subset_taxa = 1000,
                                   stratify_by = NULL) {
  
  message("\n", paste(rep("=", 60), collapse = ""))
  message("Processing: ", dataset_name)
  message(paste(rep("=", 60), collapse = ""))
  
  # Create subset if requested
  if (!is.null(subset_samples) || ntaxa(physeq) > subset_taxa) {
    physeq_subset <- create_stratified_subset(
      physeq, 
      n_samples = ifelse(is.null(subset_samples), nsamples(physeq), subset_samples),
      n_taxa = subset_taxa,
      stratify_by = stratify_by
    )
  } else {
    physeq_subset <- physeq
  }
  
  # Check for phylogenetic tree
  has_tree <- !is.null(phy_tree(physeq_subset, errorIfNULL = FALSE))
  message("Has phylogenetic tree: ", has_tree)
  
  # Extract universal information
  message("\nExtracting universal information...")
  
  tryCatch({
    # Get appropriate grouping variable
    group_var <- NULL
    if (ncol(sample_data(physeq_subset)) > 0) {
      # Try to find a good grouping variable
      potential_groups <- c("SampleType", "Enterotype", "Treatment", "body_site", 
                           "disease", "Group", "Condition")
      available_vars <- names(sample_data(physeq_subset))
      group_var <- intersect(potential_groups, available_vars)[1]
      
      if (is.na(group_var)) {
        group_var <- available_vars[1]
      }
      message("Using grouping variable: ", group_var)
    }
    
    # Extract universal information
    universal_info <- extract_universal_information(
      physeq_subset,
      groups = group_var,
      include_phylogenetic = has_tree,
      include_functional = FALSE,
      verbose = TRUE
    )
    
    # Display results
    message("\nResults:")
    message("Overall deconvolution quality: ", universal_info$deconvolution_quality$overall_quality)
    message("Mean R²: ", round(universal_info$deconvolution_quality$mean_r_squared, 3))
    
    # Show component importance
    if (!is.null(universal_info$transformation_matrix)) {
      tm <- universal_info$transformation_matrix
      # Check which component columns exist  
      comp_cols <- intersect(names(tm), c("R_component", "E_component", "P_component", "S_component"))
      
      if (length(comp_cols) > 0) {
        # Calculate average absolute importance, excluding NAs
        avg_importance <- sapply(comp_cols, function(col) {
          values <- tm[[col]]
          mean(abs(values[!is.na(values)]), na.rm = TRUE)
        })
        
        message("\nAverage component importance:")
        for (i in seq_along(comp_cols)) {
          comp_name <- substring(comp_cols[i], 1, 1)
          message("  ", comp_name, ": ", round(avg_importance[i], 3))
        }
      }
    }
    
    # Calculate some example transformations
    message("\nCalculating example metric transformations...")
    
    # Get Shannon diversity for first few samples
    div_results <- calculate_diversity(physeq_subset, metrics = "shannon")
    shannon_values <- div_results$shannon
    example_transform <- universal_diversity_transform(
      source_metrics = c(shannon = mean(shannon_values, na.rm = TRUE)),
      target_metrics = c("simpson", "observed", "chao1"),
      transformation_matrix = universal_info$transformation_matrix
    )
    
    message("Example: Shannon = ", round(mean(shannon_values, na.rm = TRUE), 2), " predicts:")
    for (metric in names(example_transform$predictions)) {
      message("  ", metric, " = ", round(example_transform$predictions[[metric]], 2))
    }
    
    # Create comprehensive results object
    results <- list(
      dataset_info = list(
        name = dataset_name,
        output_name = output_name,
        processing_date = Sys.Date(),
        package_version = "0.1.0",  # packageVersion("diversityGPT"),
        n_samples = nsamples(physeq_subset),
        n_taxa = ntaxa(physeq_subset),
        has_tree = has_tree,
        group_var = group_var,
        metadata_vars = names(sample_data(physeq_subset))
      ),
      phyloseq = physeq_subset,
      universal_info = universal_info,
      example_transformations = example_transform,
      summary_stats = list(
        mean_r_squared = universal_info$deconvolution_quality$mean_r_squared,
        quality = universal_info$deconvolution_quality$overall_quality,
        components_present = universal_info$deconvolution_quality$components_present
      )
    )
    
    # Save results
    output_file <- file.path(output_dir, paste0(output_name, "_universal.rds"))
    saveRDS(results, output_file)
    
    file_size_mb <- round(file.size(output_file) / 1024^2, 2)
    message("\nSaved to: ", output_file)
    message("File size: ", file_size_mb, " MB")
    
    return(list(
      success = TRUE,
      name = dataset_name,
      output_file = output_file,
      n_samples = nsamples(physeq_subset),
      n_taxa = ntaxa(physeq_subset),
      mean_r2 = round(universal_info$deconvolution_quality$mean_r_squared, 3),
      quality = universal_info$deconvolution_quality$overall_quality,
      file_size_mb = file_size_mb
    ))
    
  }, error = function(e) {
    message("\nERROR: Failed to process ", dataset_name)
    message("Error message: ", e$message)
    return(list(
      success = FALSE,
      name = dataset_name,
      error = e$message
    ))
  })
}

# Load and process built-in datasets
message("STARTING PRECOMPUTED DATASET GENERATION")
message("=====================================\n")

results_summary <- list()

# 1. GlobalPatterns - diverse environmental samples
message("Loading GlobalPatterns...")
data("GlobalPatterns")
results_summary$globalpatterns <- process_and_save_dataset(
  dataset_name = "GlobalPatterns",
  physeq = GlobalPatterns,
  output_name = "globalpatterns_demo",
  subset_samples = 26,  # Keep all samples, they're diverse
  subset_taxa = 2000,
  stratify_by = "SampleType"
)

# 2. Enterotype - human gut microbiome
message("\nLoading enterotype...")
data("enterotype")
results_summary$enterotype <- process_and_save_dataset(
  dataset_name = "enterotype",
  physeq = enterotype,
  output_name = "enterotype_demo",
  subset_samples = 100,
  subset_taxa = 553,  # Keep all taxa, not many
  stratify_by = "Enterotype"
)

# 3. soilrep - soil microbiome experiment
message("\nLoading soilrep...")
data("soilrep")
results_summary$soilrep <- process_and_save_dataset(
  dataset_name = "soilrep",
  physeq = soilrep,
  output_name = "soilrep_demo",
  subset_samples = 56,  # Keep all samples
  subset_taxa = 2000,
  stratify_by = "Treatment"
)

# Print final summary
message("\n", paste(rep("=", 60), collapse = ""))
message("PROCESSING SUMMARY")
message(paste(rep("=", 60), collapse = ""))

success_count <- sum(sapply(results_summary, function(x) x$success))
total_count <- length(results_summary)

message(sprintf("\nSuccessfully processed %d/%d datasets\n", success_count, total_count))

# Create summary table
for (result in results_summary) {
  if (result$success) {
    message(sprintf("✓ %-20s: %4d samples, %5d taxa, R² = %.3f (%s), %.1f MB",
                    result$name, 
                    result$n_samples, 
                    result$n_taxa, 
                    result$mean_r2, 
                    result$quality,
                    result$file_size_mb))
  } else {
    message(sprintf("✗ %-20s: ERROR - %s",
                    result$name,
                    result$error))
  }
}

message("\nPrecomputed datasets saved in: ", output_dir)
message("These can now be loaded instantly in the Shiny app!")

# Update the dataset registry to mark these as available
message("\nTo use these precomputed datasets:")
message("1. Update the registry to change 'source' from 'precomputed' to 'builtin' for testing")
message("2. Or implement the load_precomputed() function to load these .rds files")
message("3. The Shiny app will show them in the Dataset Browser")