# Test all phyloseq datasets
library(phyloseq)
# Source functions instead of loading package
source("R/universal_transformations.R")
source("R/calculate_diversity.R")

# Load datasets
cat("Loading datasets...\n")
data(GlobalPatterns)
data(enterotype) 
data(soilrep)

# Test each dataset
cat("\n=== GlobalPatterns ===\n")
print(GlobalPatterns)
cat("Sample variables:", names(sample_data(GlobalPatterns)), "\n")
cat("Has tree:", !is.null(phy_tree(GlobalPatterns, errorIfNULL = FALSE)), "\n")

cat("\n=== Enterotype ===\n")
print(enterotype)
cat("Sample variables:", names(sample_data(enterotype)), "\n")
cat("Has tree:", !is.null(phy_tree(enterotype, errorIfNULL = FALSE)), "\n")

cat("\n=== Soilrep ===\n")
print(soilrep)
cat("Sample variables:", names(sample_data(soilrep)), "\n")
cat("Has tree:", !is.null(phy_tree(soilrep, errorIfNULL = FALSE)), "\n")

# Test universal framework on each
cat("\n\nTesting Universal Framework...\n")

for (dataset_name in c("GlobalPatterns", "enterotype", "soilrep")) {
  cat("\n--- Testing", dataset_name, "---\n")
  physeq <- get(dataset_name)
  
  # Subset for faster testing
  if (nsamples(physeq) > 20) {
    physeq <- prune_samples(sample_names(physeq)[1:20], physeq)
  }
  
  tryCatch({
    result <- extract_universal_information(
      physeq,
      include_phylogenetic = !is.null(phy_tree(physeq, errorIfNULL = FALSE))
    )
    cat("Success! Mean R²:", round(result$deconvolution_quality$mean_r_squared, 3), "\n")
  }, error = function(e) {
    cat("ERROR:", e$message, "\n")
  })
}