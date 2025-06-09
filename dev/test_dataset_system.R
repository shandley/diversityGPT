# Test the new dataset registry and loader system

# Source required files
source("R/dataset_registry.R")
source("R/dataset_loaders.R")
library(phyloseq)

cat("=== Testing Dataset Registry System ===\n\n")

# Test 1: List all available datasets
cat("1. Listing all available datasets:\n")
all_datasets <- list_available_datasets()
print(all_datasets)

cat("\n2. List only 16S datasets:\n")
amplicon_datasets <- list_available_datasets(type = "16S")
print(amplicon_datasets)

cat("\n3. Search for gut-related datasets:\n")
gut_datasets <- search_datasets("gut")
print(gut_datasets)

cat("\n4. Get specific dataset info:\n")
gp_info <- get_dataset_info("globalpatterns")
cat("GlobalPatterns info:\n")
cat("  Name:", gp_info$name, "\n")
cat("  Samples:", gp_info$samples, "\n")
cat("  Taxa:", gp_info$taxa, "\n")
cat("  Has tree:", gp_info$has_tree, "\n")
cat("  Tags:", paste(gp_info$tags, collapse = ", "), "\n")

cat("\n5. Load a built-in dataset:\n")
physeq <- load_dataset("enterotype")
print(physeq)

cat("\n6. Test demo subset creation:\n")
demo_data <- create_demo_subset(physeq, n_samples = 20, n_taxa = 100)
print(demo_data)

cat("\n7. Available datasets by source:\n")
for (source in names(diversityGPT_datasets)) {
  n_datasets <- length(diversityGPT_datasets[[source]])
  cat(sprintf("  %s: %d datasets\n", source, n_datasets))
}

cat("\n=== All tests completed successfully! ===\n")