# Final test to identify the exact issue

library(devtools)
load_all()

# Create demo data
demo <- create_demo_phyloseq(n_samples = 20, n_taxa = 100)

# Test with explicit options
cat("=== Test 1: With verbose=FALSE ===\n")
tryCatch({
  drivers1 <- identify_taxa_drivers(demo, top_n = 10, verbose = FALSE)
  cat("Success!\n")
  print(drivers1)
}, error = function(e) {
  cat("ERROR:", e$message, "\n")
  cat("\nTraceback:\n")
  traceback()
})

# Test with components pre-extracted
cat("\n=== Test 2: With pre-extracted components ===\n")
info <- extract_universal_information(demo)
tryCatch({
  drivers2 <- identify_taxa_drivers(demo, components = info, top_n = 10, verbose = TRUE)
  cat("Success!\n")
}, error = function(e) {
  cat("ERROR:", e$message, "\n")
})

# Test with minimal data
cat("\n=== Test 3: With minimal data ===\n")
mini_demo <- create_demo_phyloseq(n_samples = 3, n_taxa = 5)
tryCatch({
  drivers3 <- identify_taxa_drivers(mini_demo, top_n = 3, verbose = TRUE)
  cat("Success!\n")
}, error = function(e) {
  cat("ERROR:", e$message, "\n")
})

# Test just phylogenetic component
cat("\n=== Test 4: Direct phylogenetic test ===\n")
otu_mat <- as.matrix(phyloseq::otu_table(demo))
if (!phyloseq::taxa_are_rows(demo)) otu_mat <- t(otu_mat)

# Create a dummy results object
results <- list(
  richness_drivers = data.frame(taxon = "test", contribution = 1),
  evenness_drivers = data.frame(taxon = "test", contribution = 1)
)

# Now test phylogenetic
tryCatch({
  cat("About to call identify_phylogenetic_drivers...\n")
  results$phylogenetic_drivers <- diversityGPT:::identify_phylogenetic_drivers(
    demo, info, top_n = 10, method = "contribution", groups = NULL, normalize = TRUE
  )
  cat("Success! Found", nrow(results$phylogenetic_drivers), "drivers\n")
}, error = function(e) {
  cat("ERROR in phylogenetic drivers:", e$message, "\n")
  cat("Error class:", class(e), "\n")
  
  # Get more details
  if (exists("last.warning")) {
    cat("Last warning:", last.warning, "\n")
  }
})