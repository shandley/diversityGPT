# Create example dataset for diversityGPT

library(phyloseq)

# Create a small example phyloseq object
set.seed(42)

# Create OTU table (50 taxa, 20 samples)
n_taxa <- 50
n_samples <- 20

# Simulate count data with different diversity patterns
otu_mat <- matrix(0, nrow = n_taxa, ncol = n_samples)

# Group 1: High diversity (samples 1-10)
for (i in 1:10) {
  # More even distribution
  otu_mat[, i] <- rpois(n_taxa, lambda = rep(c(10, 8, 6, 4, 2), each = 10))
}

# Group 2: Low diversity (samples 11-20)  
for (i in 11:20) {
  # Dominated by few taxa
  otu_mat[1:5, i] <- rpois(5, lambda = 50)
  otu_mat[6:n_taxa, i] <- rpois(n_taxa - 5, lambda = 1)
}

# Add noise
otu_mat <- otu_mat + rpois(length(otu_mat), lambda = 0.5)

# Set names
rownames(otu_mat) <- paste0("OTU", 1:n_taxa)
colnames(otu_mat) <- paste0("Sample", 1:n_samples)

# Create taxonomy table
tax_mat <- matrix(
  paste0("Taxa_", rep(1:n_taxa, each = 7), "_", 
         rep(c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"), n_taxa)),
  nrow = n_taxa, 
  ncol = 7,
  byrow = TRUE
)
rownames(tax_mat) <- rownames(otu_mat)
colnames(tax_mat) <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")

# Create sample data
sample_df <- data.frame(
  SampleID = colnames(otu_mat),
  Group = rep(c("High_Diversity", "Low_Diversity"), each = 10),
  Treatment = rep(c("Control", "Treatment"), 10),
  Timepoint = rep(c("T0", "T1"), each = 10),
  row.names = colnames(otu_mat)
)

# Create phyloseq object
example_physeq <- phyloseq(
  otu_table(otu_mat, taxa_are_rows = TRUE),
  tax_table(tax_mat),
  sample_data(sample_df)
)

# Save the data
usethis::use_data(example_physeq, overwrite = TRUE)

# Also create a simple diversity results example
diversity_example <- data.frame(
  sample = paste0("Sample", 1:20),
  shannon = c(rnorm(10, mean = 3, sd = 0.3), rnorm(10, mean = 1.5, sd = 0.3)),
  simpson = c(rnorm(10, mean = 0.9, sd = 0.05), rnorm(10, mean = 0.6, sd = 0.05)),
  chao1 = c(rnorm(10, mean = 45, sd = 5), rnorm(10, mean = 20, sd = 5)),
  group = rep(c("High_Diversity", "Low_Diversity"), each = 10)
)

attr(diversity_example, "metrics") <- c("shannon", "simpson", "chao1")
attr(diversity_example, "n_samples") <- 20
attr(diversity_example, "n_taxa") <- 50
class(diversity_example) <- c("diversity_results", "data.frame")

usethis::use_data(diversity_example, overwrite = TRUE)

cli::cli_alert_success("Example data created and saved to data/")