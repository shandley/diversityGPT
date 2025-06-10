# Debug script to identify the exact error location

library(devtools)
load_all()

# Create demo data
set.seed(42)
demo <- create_demo_phyloseq(n_samples = 20, n_taxa = 100)

# Check what we have
cat("Demo data structure:\n")
cat("- Samples:", phyloseq::nsamples(demo), "\n")
cat("- Taxa:", phyloseq::ntaxa(demo), "\n")
cat("- Has tree?", !is.null(phyloseq::phy_tree(demo, errorIfNULL = FALSE)), "\n")
cat("- Has tax_table?", !is.null(phyloseq::tax_table(demo, errorIfNULL = FALSE)), "\n")

# Check taxonomy table structure
if (!is.null(phyloseq::tax_table(demo, errorIfNULL = FALSE))) {
  tax_tab <- phyloseq::tax_table(demo)
  cat("\nTaxonomy table info:\n")
  cat("- Class:", class(tax_tab), "\n")
  cat("- Dimensions:", dim(tax_tab), "\n")
  cat("- Column names:", colnames(tax_tab), "\n")
  
  # Check first few entries
  cat("\nFirst 3 rows of taxonomy:\n")
  print(head(tax_tab, 3))
}

# Test the individual function that's failing
cat("\n\nTesting calculate_taxonomic_uniqueness directly:\n")
tryCatch({
  uniqueness <- diversityGPT:::calculate_taxonomic_uniqueness(demo)
  cat("Success! Uniqueness vector length:", length(uniqueness), "\n")
  cat("First few values:", head(uniqueness), "\n")
}, error = function(e) {
  cat("ERROR in calculate_taxonomic_uniqueness:", e$message, "\n")
  print(traceback())
})

# Try identify_phylogenetic_drivers directly
cat("\n\nTesting identify_phylogenetic_drivers directly:\n")
otu_mat <- as.matrix(phyloseq::otu_table(demo))
if (!phyloseq::taxa_are_rows(demo)) {
  otu_mat <- t(otu_mat)
}

# Mock components (not used in contribution method)
components <- list()

tryCatch({
  phylo_drivers <- diversityGPT:::identify_phylogenetic_drivers(
    demo, components, top_n = 10,
    method = "contribution", groups = NULL, normalize = TRUE
  )
  cat("Success! Found", nrow(phylo_drivers), "phylogenetic drivers\n")
}, error = function(e) {
  cat("ERROR in identify_phylogenetic_drivers:", e$message, "\n")
  # More detailed error info
  cat("\nDetailed error info:\n")
  print(str(e))
  
  # Try to pinpoint the exact comparison causing issues
  cat("\nChecking taxonomy table structure in detail:\n")
  tax_tab <- phyloseq::tax_table(demo)
  cat("Storage mode:", storage.mode(tax_tab), "\n")
  cat("Typeof:", typeof(tax_tab), "\n")
  
  # Check if it's the S4 object comparison
  if (methods::is(tax_tab, "taxonomyTable")) {
    cat("It's a taxonomyTable S4 object\n")
    tax_mat <- as(tax_tab, "matrix")
    cat("Converted to matrix, dimensions:", dim(tax_mat), "\n")
    cat("Matrix class:", class(tax_mat), "\n")
  }
})