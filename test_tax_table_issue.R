# Test to identify the taxonomyTable issue

library(devtools)
load_all()

# Test 1: Direct comparison issue
cat("=== Testing S4 object comparison ===\n")
demo <- create_demo_phyloseq(n_samples = 5, n_taxa = 10)
tax_tab <- phyloseq::tax_table(demo)

# This might be the issue - comparing S4 objects
tryCatch({
  result <- tax_tab == tax_tab  # This could throw the error
  cat("Direct S4 comparison works\n")
}, error = function(e) {
  cat("ERROR in S4 comparison:", e$message, "\n")
})

# Test 2: Element access
cat("\n=== Testing element access ===\n")
tryCatch({
  # Access as matrix
  tax_mat <- as(tax_tab, "matrix")
  val1 <- tax_mat[1, 1]
  val2 <- tax_mat[2, 1]
  cat("Matrix access works. Values:", val1, val2, "\n")
  
  # Try comparison
  comp <- tax_mat[, 1] == tax_mat[1, 1]
  cat("Column comparison works. Results:", head(comp), "\n")
}, error = function(e) {
  cat("ERROR in matrix operations:", e$message, "\n")
})

# Test 3: Run the full analysis with tryCatch at each step
cat("\n=== Testing full analysis with debugging ===\n")

# Manually step through identify_taxa_drivers
tryCatch({
  cat("1. Extracting universal information...\n")
  info <- extract_universal_information(demo)
  cat("   Success!\n")
  
  cat("2. Getting OTU matrix...\n")
  otu_mat <- as.matrix(phyloseq::otu_table(demo))
  if (!phyloseq::taxa_are_rows(demo)) {
    otu_mat <- t(otu_mat)
  }
  cat("   OTU matrix dimensions:", dim(otu_mat), "\n")
  
  cat("3. Identifying richness drivers...\n")
  richness_drivers <- diversityGPT:::identify_richness_drivers(
    otu_mat, info, top_n = 5, method = "contribution", 
    groups = NULL, normalize = TRUE
  )
  cat("   Found", nrow(richness_drivers), "richness drivers\n")
  
  cat("4. Identifying evenness drivers...\n")
  evenness_drivers <- diversityGPT:::identify_evenness_drivers(
    otu_mat, info, top_n = 5, method = "contribution",
    groups = NULL, normalize = TRUE
  )
  cat("   Found", nrow(evenness_drivers), "evenness drivers\n")
  
  cat("5. Identifying phylogenetic drivers...\n")
  phylo_drivers <- diversityGPT:::identify_phylogenetic_drivers(
    demo, info, top_n = 5, method = "contribution",
    groups = NULL, normalize = TRUE
  )
  cat("   Found", nrow(phylo_drivers), "phylogenetic drivers\n")
  
  cat("\nSUCCESS! All components work individually.\n")
  
}, error = function(e) {
  cat("\nERROR at step:", e$message, "\n")
  cat("Call stack:\n")
  print(sys.calls())
})