# Test loading precomputed datasets

# List generated files
cat("Generated precomputed files:\n")
files <- list.files("inst/data", pattern = "\\.rds$", full.names = TRUE)
for (f in files) {
  size_mb <- round(file.size(f) / 1024^2, 2)
  cat(sprintf("  %s (%.2f MB)\n", basename(f), size_mb))
}

# Test loading each file
cat("\nTesting loading of precomputed datasets:\n\n")

for (f in files) {
  cat("Loading:", basename(f), "\n")
  
  tryCatch({
    data <- readRDS(f)
    
    # Check structure
    cat("  Dataset info:\n")
    cat("    Name:", data$dataset_info$name, "\n")
    cat("    Samples:", data$dataset_info$n_samples, "\n")
    cat("    Taxa:", data$dataset_info$n_taxa, "\n")
    cat("    Processing date:", as.character(data$dataset_info$processing_date), "\n")
    cat("    Has tree:", data$dataset_info$has_tree, "\n")
    
    # Check components
    cat("  Components present:\n")
    cat("    phyloseq:", !is.null(data$phyloseq), "\n")
    cat("    universal_info:", !is.null(data$universal_info), "\n")
    cat("    Mean R²:", data$summary_stats$mean_r_squared, "\n")
    cat("    Quality:", data$summary_stats$quality, "\n")
    
    cat("  ✓ Successfully loaded!\n\n")
    
  }, error = function(e) {
    cat("  ✗ ERROR:", e$message, "\n\n")
  })
}

# Test using with load_precomputed function
cat("Testing load_precomputed function:\n")
source("R/dataset_loaders.R")
source("R/dataset_registry.R")

# Try to load one of the precomputed datasets
dataset_info <- list(
  file = "globalpatterns_demo_universal.rds"
)

tryCatch({
  result <- load_precomputed(dataset_info, return_physeq = FALSE)
  cat("✓ Successfully loaded GlobalPatterns universal info\n")
  cat("  Mean R²:", round(result$deconvolution_quality$mean_r_squared, 3), "\n")
}, error = function(e) {
  cat("✗ Error:", e$message, "\n")
})