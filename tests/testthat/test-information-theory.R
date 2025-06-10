# Tests for Information Theory Module

library(testthat)
library(diversityGPT)

# Test data setup
test_that("Information theory functions work with demo data", {
  
  # Create test data
  demo_data <- create_demo_phyloseq(n_samples = 10, n_taxa = 20)
  info <- extract_universal_information(demo_data)
  
  # Test main function
  expect_no_error({
    mi_results <- calculate_taxa_mutual_information(
      demo_data, 
      info, 
      verbose = FALSE
    )
  })
  
  # Check structure
  expect_s3_class(mi_results, "mutual_information")
  expect_true(all(c("mi_matrix", "normalized_mi", "information_gain", 
                   "taxa_rankings", "summary_stats") %in% names(mi_results)))
  
  # Check dimensions
  expect_equal(nrow(mi_results$mi_matrix), phyloseq::ntaxa(demo_data))
  expect_equal(ncol(mi_results$mi_matrix), 4)  # R, E, P, S components
  
  # Check that values are non-negative
  expect_true(all(mi_results$mi_matrix >= 0))
  expect_true(all(mi_results$normalized_mi >= 0))
  expect_true(all(mi_results$normalized_mi <= 1))
})

test_that("Discretization methods work correctly", {
  
  # Create test abundance data
  otu_mat <- matrix(c(
    c(0, 5, 10, 15, 20),  # Taxon 1
    c(1, 1, 1, 1, 1),     # Taxon 2 - uniform
    c(0, 0, 100, 0, 0)    # Taxon 3 - sparse
  ), nrow = 3, byrow = TRUE)
  
  rownames(otu_mat) <- paste0("OTU", 1:3)
  colnames(otu_mat) <- paste0("Sample", 1:5)
  
  # Test equal width discretization
  discrete_equal <- diversityGPT:::discretize_taxa_abundances(
    otu_mat, "equal_width", 3
  )
  expect_equal(dim(discrete_equal), dim(otu_mat))
  expect_true(all(discrete_equal >= 1))
  expect_true(all(discrete_equal <= 3))
  
  # Test equal frequency discretization
  discrete_freq <- diversityGPT:::discretize_taxa_abundances(
    otu_mat, "equal_freq", 3
  )
  expect_equal(dim(discrete_freq), dim(otu_mat))
  expect_true(all(discrete_freq >= 1))
  
  # Test k-means discretization
  discrete_kmeans <- diversityGPT:::discretize_taxa_abundances(
    otu_mat, "kmeans", 3
  )
  expect_equal(dim(discrete_kmeans), dim(otu_mat))
  expect_true(all(discrete_kmeans >= 1))
})

test_that("Mutual information calculation is correct", {
  
  # Test with known relationship
  x <- c(1, 1, 2, 2, 3, 3)
  y <- c(1, 1, 2, 2, 3, 3)  # Perfect correlation
  
  mi_perfect <- diversityGPT:::calculate_mutual_information(x, y, "emp")
  expect_gt(mi_perfect, 0)
  
  # Test with independent variables
  x <- c(1, 1, 2, 2, 3, 3)
  y <- c(1, 2, 1, 2, 1, 2)  # Independent
  
  mi_independent <- diversityGPT:::calculate_mutual_information(x, y, "emp")
  expect_gte(mi_independent, 0)
  expect_lt(mi_independent, mi_perfect)
})

test_that("Entropy calculation is correct", {
  
  # Test uniform distribution
  uniform <- c(1, 1, 1, 1)
  entropy_uniform <- diversityGPT:::calculate_entropy(uniform)
  expect_equal(entropy_uniform, 0)  # No uncertainty
  
  # Test maximum entropy (uniform over multiple values)
  max_entropy <- c(1, 2, 3, 4)
  entropy_max <- diversityGPT:::calculate_entropy(max_entropy)
  expect_equal(entropy_max, 2)  # log2(4) = 2
  
  # Test with missing values
  with_na <- c(1, 2, NA, 3)
  entropy_na <- diversityGPT:::calculate_entropy(with_na)
  expect_gte(entropy_na, 0)
})

test_that("Conditional entropy calculation works", {
  
  # Test case where Y is completely determined by X
  x <- c(1, 1, 2, 2, 3, 3)
  y <- c(1, 1, 2, 2, 3, 3)
  
  cond_entropy <- diversityGPT:::calculate_conditional_entropy(y, x)
  expect_equal(cond_entropy, 0)  # No uncertainty given X
  
  # Test with independent variables
  x <- c(1, 1, 2, 2, 3, 3)
  y <- c(1, 2, 1, 2, 1, 2)
  
  cond_entropy_indep <- diversityGPT:::calculate_conditional_entropy(y, x)
  unconditional_entropy <- diversityGPT:::calculate_entropy(y)
  expect_equal(cond_entropy_indep, unconditional_entropy, tolerance = 0.01)
})

test_that("Information gain calculation works", {
  
  demo_data <- create_demo_phyloseq(n_samples = 8, n_taxa = 5)
  otu_mat <- as.matrix(phyloseq::otu_table(demo_data))
  if (!phyloseq::taxa_are_rows(demo_data)) otu_mat <- t(otu_mat)
  
  # Create mock component data
  comp_data <- matrix(rnorm(8 * 4), nrow = 8, ncol = 4)
  colnames(comp_data) <- c("richness", "evenness", "phylogenetic", "spatial")
  
  # Discretize taxa
  discrete_taxa <- diversityGPT:::discretize_taxa_abundances(otu_mat, "equal_width", 3)
  
  # Calculate information gain
  info_gain <- diversityGPT:::calculate_information_gain(discrete_taxa, comp_data, "emp")
  
  # Check structure
  expect_equal(dim(info_gain), c(phyloseq::ntaxa(demo_data), 4))
  expect_true(all(info_gain >= 0))  # Information gain should be non-negative
})

test_that("Different estimation methods work", {
  
  demo_data <- create_demo_phyloseq(n_samples = 15, n_taxa = 10)
  info <- extract_universal_information(demo_data)
  
  # Test empirical method
  mi_emp <- calculate_taxa_mutual_information(
    demo_data, info, method = "emp", verbose = FALSE
  )
  expect_s3_class(mi_emp, "mutual_information")
  
  # Test Miller-Madow method
  mi_mm <- calculate_taxa_mutual_information(
    demo_data, info, method = "mm", verbose = FALSE
  )
  expect_s3_class(mi_mm, "mutual_information")
  
  # Test shrinkage method
  mi_shrink <- calculate_taxa_mutual_information(
    demo_data, info, method = "shrink", verbose = FALSE
  )
  expect_s3_class(mi_shrink, "mutual_information")
  
  # Check that different methods give different results
  expect_false(identical(mi_emp$mi_matrix, mi_mm$mi_matrix))
})

test_that("Print method works", {
  
  demo_data <- create_demo_phyloseq(n_samples = 8, n_taxa = 10)
  info <- extract_universal_information(demo_data)
  
  mi_results <- calculate_taxa_mutual_information(
    demo_data, info, verbose = FALSE
  )
  
  # Test print method
  expect_output(print(mi_results), "Mutual Information Analysis Results")
  expect_output(print(mi_results), "Method:")
  expect_output(print(mi_results), "OVERALL STATISTICS:")
})

test_that("Plot methods work", {
  
  demo_data <- create_demo_phyloseq(n_samples = 8, n_taxa = 15)
  info <- extract_universal_information(demo_data)
  
  mi_results <- calculate_taxa_mutual_information(
    demo_data, info, verbose = FALSE
  )
  
  # Test heatmap plot
  expect_no_error({
    p1 <- plot(mi_results, type = "heatmap", top_n = 10)
  })
  expect_s3_class(p1, "ggplot")
  
  # Test ranking plot
  expect_no_error({
    p2 <- plot(mi_results, type = "ranking", top_n = 8)
  })
  expect_s3_class(p2, "ggplot")
  
  # Test comparison plot
  expect_no_error({
    p3 <- plot(mi_results, type = "comparison")
  })
  expect_s3_class(p3, "ggplot")
  
  # Test network plot
  expect_no_error({
    p4 <- plot(mi_results, type = "network", top_n = 5)
  })
  # Network plot might return different types depending on data
})

test_that("Edge cases are handled correctly", {
  
  # Test with minimal data
  mini_data <- create_demo_phyloseq(n_samples = 3, n_taxa = 2)
  info <- extract_universal_information(mini_data)
  
  expect_no_error({
    mi_results <- calculate_taxa_mutual_information(
      mini_data, info, verbose = FALSE
    )
  })
  
  # Test with single taxon
  single_taxon_data <- create_demo_phyloseq(n_samples = 5, n_taxa = 1)
  info_single <- extract_universal_information(single_taxon_data)
  
  expect_no_error({
    mi_single <- calculate_taxa_mutual_information(
      single_taxon_data, info_single, verbose = FALSE
    )
  })
  
  expect_equal(nrow(mi_single$mi_matrix), 1)
})

test_that("Taxa rankings are consistent", {
  
  demo_data <- create_demo_phyloseq(n_samples = 12, n_taxa = 20)
  info <- extract_universal_information(demo_data)
  
  mi_results <- calculate_taxa_mutual_information(
    demo_data, info, verbose = FALSE
  )
  
  # Check that rankings have correct structure
  expect_equal(length(mi_results$taxa_rankings), 4)  # R, E, P, S
  expect_true(all(c("richness", "evenness", "phylogenetic", "spatial") %in% 
                 names(mi_results$taxa_rankings)))
  
  # Check that each ranking has correct columns
  for (comp in names(mi_results$taxa_rankings)) {
    ranking <- mi_results$taxa_rankings[[comp]]
    expect_true(all(c("taxon", "mutual_information", "normalized_mi", 
                     "final_rank") %in% names(ranking)))
    expect_equal(nrow(ranking), phyloseq::ntaxa(demo_data))
    
    # Check that rankings are sorted correctly
    expect_true(all(diff(ranking$normalized_mi) <= 0))  # Decreasing order
  }
})

test_that("Summary statistics are reasonable", {
  
  demo_data <- create_demo_phyloseq(n_samples = 10, n_taxa = 15)
  info <- extract_universal_information(demo_data)
  
  mi_results <- calculate_taxa_mutual_information(
    demo_data, info, verbose = FALSE
  )
  
  stats <- mi_results$summary_stats
  
  # Check overall statistics
  expect_equal(stats$overall$n_taxa, 15)
  expect_equal(stats$overall$n_components, 4)
  expect_gte(stats$overall$mean_mi, 0)
  expect_gte(stats$overall$mean_normalized_mi, 0)
  expect_lte(stats$overall$mean_normalized_mi, 1)
  
  # Check component statistics
  components <- c("richness", "evenness", "phylogenetic", "spatial")
  for (comp in components) {
    comp_stats <- stats[[comp]]
    expect_gte(comp_stats$max_mi, 0)
    expect_gte(comp_stats$max_normalized_mi, 0)
    expect_lte(comp_stats$max_normalized_mi, 1)
    expect_gte(comp_stats$n_significant, 0)
  }
  
  # Check correlations
  expect_true(is.numeric(stats$correlations$mi_vs_normalized))
  expect_true(is.numeric(stats$correlations$mi_vs_info_gain))
})