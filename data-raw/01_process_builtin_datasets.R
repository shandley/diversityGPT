# Process built-in phyloseq datasets and create precomputed universal transformations
# This creates smaller, pre-analyzed versions for quick demos

library(phyloseq)

# Source required functions
source("R/universal_transformations.R")
source("R/calculate_diversity.R")

# Create output directory
output_dir <- "inst/data"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Function to process a dataset
process_dataset <- function(dataset_name, output_name, subset_samples = NULL) {
  
  message("\n", paste(rep("=", 50), collapse = ""))
  message("Processing: ", dataset_name)
  message(paste(rep("=", 50), collapse = ""))
  
  # Load dataset
  data(list = dataset_name, package = "phyloseq", envir = environment())
  physeq <- get(dataset_name, envir = environment())
  
  message("Original size: ", nsamples(physeq), " samples, ", ntaxa(physeq), " taxa")
  
  # Create a reasonable subset for demo
  if (!is.null(subset_samples) && nsamples(physeq) > subset_samples) {
    # Keep diverse samples
    set.seed(123)
    
    # Try to keep samples from different groups if metadata exists
    if (ncol(sample_data(physeq)) > 0) {
      # Use first metadata column for stratified sampling
      meta_var <- names(sample_data(physeq))[1]
      groups <- sample_data(physeq)[[meta_var]]
      
      # Sample proportionally from each group
      keep_samples <- c()
      for (grp in unique(groups)) {
        grp_samples <- sample_names(physeq)[groups == grp]
        n_keep <- max(1, round(subset_samples * length(grp_samples) / nsamples(physeq)))
        n_keep <- min(n_keep, length(grp_samples))
        keep_samples <- c(keep_samples, sample(grp_samples, n_keep))
      }
      
      # Ensure we have the right number
      if (length(keep_samples) > subset_samples) {
        keep_samples <- sample(keep_samples, subset_samples)
      } else if (length(keep_samples) < subset_samples) {
        remaining <- setdiff(sample_names(physeq), keep_samples)
        n_more <- subset_samples - length(keep_samples)
        keep_samples <- c(keep_samples, sample(remaining, min(n_more, length(remaining))))
      }
    } else {
      # Random sampling
      keep_samples <- sample(sample_names(physeq), subset_samples)
    }
    
    physeq <- prune_samples(keep_samples, physeq)
    
    # Also reduce taxa to most abundant
    max_taxa <- 1000
    if (ntaxa(physeq) > max_taxa) {
      taxon_sums <- taxa_sums(physeq)
      keep_taxa <- names(sort(taxon_sums, decreasing = TRUE)[1:max_taxa])
      physeq <- prune_taxa(keep_taxa, physeq)
    }
    
    message("Subset size: ", nsamples(physeq), " samples, ", ntaxa(physeq), " taxa")
  }
  
  # Check if tree exists
  has_tree <- !is.null(phy_tree(physeq, errorIfNULL = FALSE))
  
  # Extract universal information
  message("\nExtracting universal information...")
  message("Include phylogenetic: ", has_tree)
  
  tryCatch({
    universal_info <- extract_universal_information(
      physeq,
      include_phylogenetic = has_tree,
      verbose = TRUE
    )
    
    message("\nResults:")
    message("Mean R²: ", round(universal_info$deconvolution_quality$mean_r_squared, 3))
    message("Overall quality: ", universal_info$deconvolution_quality$overall_quality)
    
    # Create results object
    results <- list(
      dataset_name = dataset_name,
      processing_date = Sys.Date(),
      diversityGPT_version = packageVersion("diversityGPT"),
      phyloseq = physeq,
      universal_info = universal_info,
      dataset_info = list(
        original_samples = nsamples(get(dataset_name, envir = environment())),
        original_taxa = ntaxa(get(dataset_name, envir = environment())),
        subset_samples = nsamples(physeq),
        subset_taxa = ntaxa(physeq),
        has_tree = has_tree
      )
    )
    
    # Save results
    output_file <- file.path(output_dir, paste0(output_name, "_universal.rds"))
    saveRDS(results, output_file)
    message("\nSaved to: ", output_file)
    message("File size: ", round(file.size(output_file) / 1024^2, 2), " MB")
    
    return(results)
    
  }, error = function(e) {
    warning("Failed to process ", dataset_name, ": ", e$message)
    return(NULL)
  })
}

# Process each built-in dataset
datasets_to_process <- list(
  list(name = "GlobalPatterns", output = "globalpatterns_demo", subset = 20),
  list(name = "enterotype", output = "enterotype_demo", subset = 100),
  list(name = "soilrep", output = "soilrep_demo", subset = 40)
)

results_summary <- list()

for (dataset in datasets_to_process) {
  result <- process_dataset(
    dataset_name = dataset$name,
    output_name = dataset$output,
    subset_samples = dataset$subset
  )
  
  if (!is.null(result)) {
    results_summary[[dataset$name]] <- list(
      samples = result$dataset_info$subset_samples,
      taxa = result$dataset_info$subset_taxa,
      mean_r2 = round(result$universal_info$deconvolution_quality$mean_r_squared, 3),
      quality = result$universal_info$deconvolution_quality$overall_quality
    )
  }
}

# Print summary
message("\n", paste(rep("=", 50), collapse = ""))
message("PROCESSING SUMMARY")
message(paste(rep("=", 50), collapse = ""))

for (name in names(results_summary)) {
  info <- results_summary[[name]]
  message(sprintf("%-20s: %4d samples, %5d taxa, R² = %.3f (%s)",
                  name, info$samples, info$taxa, info$mean_r2, info$quality))
}

message("\nAll datasets processed successfully!")
message("Precomputed results saved in: ", output_dir)