# Quick check of transformation matrix structure

library(phyloseq)
source("R/calculate_diversity.R")
source("R/universal_transformations.R")
source("R/universal_information.R")

# Load small dataset
data("enterotype")
physeq <- prune_samples(sample_names(enterotype)[1:20], enterotype)

# Extract universal info
universal_info <- extract_universal_information(physeq, verbose = FALSE)

# Check transformation matrix structure
tm <- universal_info$transformation_matrix
cat("Transformation matrix columns:\n")
print(names(tm))

cat("\nFirst few rows:\n")
print(head(tm))

cat("\nTransformation matrix class:\n")
print(class(tm))