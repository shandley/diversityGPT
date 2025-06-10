# Create realistic microbiome data to test taxa drivers

library(devtools)
load_all()

# Function to create realistic microbiome data
create_realistic_microbiome <- function(n_samples = 30, n_taxa = 200, seed = 123) {
  set.seed(seed)
  
  # Create more realistic abundance patterns
  otu_mat <- matrix(0, nrow = n_taxa, ncol = n_samples)
  
  # 1. Core microbiome (10% of taxa, present in >80% samples, high abundance)
  n_core <- round(n_taxa * 0.1)
  for (i in 1:n_core) {
    # Present in 80-100% of samples
    presence_prob <- runif(1, 0.8, 1.0)
    present_samples <- sample(n_samples, round(n_samples * presence_prob))
    # High abundances
    otu_mat[i, present_samples] <- rpois(length(present_samples), lambda = 50)
  }
  
  # 2. Common taxa (30% of taxa, present in 20-80% samples, medium abundance)
  n_common <- round(n_taxa * 0.3)
  for (i in (n_core + 1):(n_core + n_common)) {
    presence_prob <- runif(1, 0.2, 0.8)
    present_samples <- sample(n_samples, round(n_samples * presence_prob))
    otu_mat[i, present_samples] <- rpois(length(present_samples), lambda = 15)
  }
  
  # 3. Rare taxa (60% of taxa, present in <20% samples, low abundance)
  n_rare <- n_taxa - n_core - n_common
  for (i in (n_core + n_common + 1):n_taxa) {
    presence_prob <- runif(1, 0.01, 0.2)
    present_samples <- sample(n_samples, max(1, round(n_samples * presence_prob)))
    otu_mat[i, present_samples] <- rpois(length(present_samples), lambda = 3)
  }
  
  # Add some dominant blooms in specific samples
  bloom_taxa <- sample(n_taxa, 5)
  bloom_samples <- sample(n_samples, 8)
  for (i in bloom_taxa) {
    bloom_subset <- sample(bloom_samples, 3)
    otu_mat[i, bloom_subset] <- otu_mat[i, bloom_subset] + rpois(length(bloom_subset), lambda = 100)
  }
  
  # Create sample metadata with environmental gradients
  sample_df <- data.frame(
    Environment = rep(c("Forest", "Grassland", "Urban"), length.out = n_samples),
    pH = rnorm(n_samples, mean = 6.5, sd = 1),
    Temperature = rnorm(n_samples, mean = 20, sd = 5),
    Moisture = runif(n_samples, 10, 80),
    row.names = paste0("Sample", 1:n_samples)
  )
  
  # Create more realistic taxonomy
  tax_mat <- matrix(NA, nrow = n_taxa, ncol = 7)
  phyla <- c("Proteobacteria", "Bacteroidetes", "Firmicutes", "Actinobacteria", "Acidobacteria")
  
  for (i in 1:n_taxa) {
    phylum <- sample(phyla, 1, prob = c(0.3, 0.25, 0.2, 0.15, 0.1))
    tax_mat[i, ] <- c("Bacteria", phylum, 
                     paste0(phylum, "_class_", sample(1:10, 1)),
                     paste0(phylum, "_order_", sample(1:20, 1)),
                     paste0(phylum, "_family_", sample(1:50, 1)),
                     paste0("Genus_", i),
                     paste0("Species_", i))
  }
  
  rownames(otu_mat) <- paste0("OTU", 1:n_taxa)
  colnames(otu_mat) <- rownames(sample_df)
  rownames(tax_mat) <- rownames(otu_mat)
  colnames(tax_mat) <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
  
  # Create phyloseq object
  phyloseq::phyloseq(
    phyloseq::otu_table(otu_mat, taxa_are_rows = TRUE),
    phyloseq::sample_data(sample_df),
    phyloseq::tax_table(tax_mat)
  )
}

# Create realistic data
realistic_data <- create_realistic_microbiome(n_samples = 30, n_taxa = 200)

cat("Realistic microbiome data created:\n")
cat("- Samples:", phyloseq::nsamples(realistic_data), "\n")
cat("- Taxa:", phyloseq::ntaxa(realistic_data), "\n")
cat("- Sample sums range:", range(phyloseq::sample_sums(realistic_data)), "\n")
cat("- Taxa sums range:", range(phyloseq::taxa_sums(realistic_data)), "\n")

# Analyze with taxa drivers
cat("\n=== Taxa Driver Analysis ===\n")
realistic_drivers <- identify_taxa_drivers(
  realistic_data,
  top_n = 15,
  method = "contribution",
  verbose = TRUE
)

# Print results
print(realistic_drivers)

# Create visualizations
cat("\n=== Creating Visualizations ===\n")

# Bar plot - should show more variation now
p1 <- plot(realistic_drivers, type = "bar", top_n = 15)
print(p1)

# Check contribution ranges for each component
cat("\nContribution ranges by component:\n")
components <- c("richness", "evenness", "phylogenetic", "spatial")
for (comp in components) {
  comp_name <- paste0(comp, "_drivers")
  if (!is.null(realistic_drivers[[comp_name]])) {
    contrib_range <- range(realistic_drivers[[comp_name]]$contribution)
    cat(sprintf("  %s: %.3f - %.3f (%.1fx difference)\n", 
               comp, contrib_range[1], contrib_range[2], 
               contrib_range[2]/contrib_range[1]))
  }
}

# Test with different methods
cat("\n=== Testing Different Methods ===\n")

drivers_var <- identify_taxa_drivers(realistic_data, method = "variance", top_n = 10, verbose = FALSE)
drivers_cor <- identify_taxa_drivers(realistic_data, method = "correlation", top_n = 10, verbose = FALSE)

# Compare top richness drivers across methods
cat("\nTop 5 richness drivers by method:\n")
comparison <- data.frame(
  Rank = 1:5,
  Contribution = realistic_drivers$richness_drivers$taxon[1:5],
  Variance = drivers_var$richness_drivers$taxon[1:5],
  Correlation = drivers_cor$richness_drivers$taxon[1:5],
  stringsAsFactors = FALSE
)
print(comparison)

# Show actual contribution values
cat("\nRichness driver contributions:\n")
cat("Contribution method:\n")
print(realistic_drivers$richness_drivers[1:5, c("taxon", "contribution", "presence_frequency")])

cat("\nVariance method:\n") 
print(drivers_var$richness_drivers[1:5, c("taxon", "contribution")])

cat("\n=== Test Complete ===\n")
cat("Notice how realistic data shows much more variation in contributions!\n")