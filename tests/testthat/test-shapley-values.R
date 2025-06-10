# Tests for Shapley Values Module

library(testthat)
library(diversityGPT)

# Test data setup
test_that("Shapley value functions work with demo data", {
  
  # Create test data
  demo_data <- create_demo_phyloseq(n_samples = 8, n_taxa = 10)
  info <- extract_universal_information(demo_data)
  
  # Test main function with exact method
  expect_no_error({
    shapley_results <- calculate_taxa_shapley_values(
      demo_data, 
      info,
      top_n = 6,
      method = "exact",
      verbose = FALSE
    )
  })
  
  # Check structure
  expect_s3_class(shapley_results, "shapley_values")
  expect_true(all(c("shapley_matrix", "coalitional_values", "marginal_contributions",
                   "fairness_properties", "taxa_rankings", "summary_stats") %in% names(shapley_results)))
  
  # Check dimensions
  expect_equal(nrow(shapley_results$shapley_matrix), 6)  # top_n taxa
  expect_equal(ncol(shapley_results$shapley_matrix), 4)  # R, E, P, S components
  
  # Check that Shapley values exist
  expect_true(all(is.finite(shapley_results$shapley_matrix)))
  expect_true(any(shapley_results$shapley_matrix != 0))
})

test_that("Different Shapley calculation methods work", {
  
  demo_data <- create_demo_phyloseq(n_samples = 6, n_taxa = 8)
  info <- extract_universal_information(demo_data)
  
  # Test exact method
  expect_no_error({
    exact_results <- calculate_taxa_shapley_values(
      demo_data, info,
      top_n = 5,
      method = "exact",
      verbose = FALSE
    )
  })
  expect_equal(exact_results$method, "exact")
  
  # Test sampling method
  expect_no_error({
    sampling_results <- calculate_taxa_shapley_values(
      demo_data, info,
      top_n = 5,
      method = "sampling",
      n_samples = 100,
      verbose = FALSE
    )
  })
  expect_equal(sampling_results$method, "sampling")
  expect_equal(sampling_results$n_samples, 100)
  
  # Test permutation method
  expect_no_error({
    perm_results <- calculate_taxa_shapley_values(
      demo_data, info,
      top_n = 5,
      method = "permutation",
      n_samples = 50,
      verbose = FALSE
    )
  })
  expect_equal(perm_results$method, "permutation")
  
  # Test marginal method
  expect_no_error({
    marginal_results <- calculate_taxa_shapley_values(
      demo_data, info,
      top_n = 5,
      method = "marginal",
      verbose = FALSE
    )
  })
  expect_equal(marginal_results$method, "marginal")
})

test_that("Characteristic function works correctly", {
  
  # Create test OTU matrix
  otu_mat <- matrix(c(
    c(10, 5, 0, 15),   # Taxon 1
    c(0, 10, 20, 5),   # Taxon 2
    c(5, 5, 5, 5)      # Taxon 3
  ), nrow = 3, byrow = TRUE)
  rownames(otu_mat) <- paste0("OTU", 1:3)
  
  # Create test component data
  comp_data <- matrix(rnorm(4 * 4), nrow = 4, ncol = 4)
  colnames(comp_data) <- c("richness", "evenness", "phylogenetic", "spatial")
  
  # Create characteristic function
  char_func <- diversityGPT:::create_characteristic_function(otu_mat, comp_data, verbose = FALSE)
  
  # Test empty coalition
  expect_equal(char_func(integer(0), "richness"), 0)
  
  # Test single taxon coalitions
  val1 <- char_func(1, "richness")
  val2 <- char_func(2, "richness")
  val3 <- char_func(3, "richness")
  
  expect_gte(val1, 0)
  expect_gte(val2, 0)
  expect_gte(val3, 0)
  
  # Test that different coalitions give different values
  val12 <- char_func(c(1, 2), "richness")
  val13 <- char_func(c(1, 3), "richness")
  
  expect_gte(val12, 0)
  expect_gte(val13, 0)
  
  # Test all components
  for (comp in c("richness", "evenness", "phylogenetic", "spatial")) {
    val <- char_func(c(1, 2), comp)
    expect_gte(val, 0)
    expect_true(is.finite(val))
  }
})

test_that("Coalition value calculation is sensible", {
  
  # Create simple test case
  coalition_otu <- matrix(c(
    c(10, 0, 5),   # Taxon 1 - present in samples 1,3
    c(0, 15, 0)    # Taxon 2 - present in sample 2
  ), nrow = 2, byrow = TRUE)
  
  comp_data <- matrix(c(0.5, 0.6, 0.4, 0.7), nrow = 3, ncol = 4)
  colnames(comp_data) <- c("richness", "evenness", "phylogenetic", "spatial")
  
  # Test richness calculation
  richness_val <- diversityGPT:::calculate_coalition_value(
    coalition_otu, comp_data, "richness"
  )
  expect_gte(richness_val, 0)
  
  # Test evenness calculation
  evenness_val <- diversityGPT:::calculate_coalition_value(
    coalition_otu, comp_data, "evenness"
  )
  expect_gte(evenness_val, 0)
  
  # Empty coalition should give 0
  empty_val <- diversityGPT:::calculate_coalition_value(
    matrix(nrow = 0, ncol = 3), comp_data, "richness"
  )
  expect_equal(empty_val, 0)
})

test_that("Shapley axioms are validated correctly", {
  
  demo_data <- create_demo_phyloseq(n_samples = 6, n_taxa = 6)
  info <- extract_universal_information(demo_data)
  
  shapley_results <- calculate_taxa_shapley_values(
    demo_data, info,
    top_n = 4,
    method = "exact",
    verbose = FALSE
  )
  
  fairness <- shapley_results$fairness_properties
  
  # Check that fairness properties are calculated for all components
  components <- c("richness", "evenness", "phylogenetic", "spatial")
  for (comp in components) {
    expect_true(comp %in% names(fairness))
    
    # Check efficiency axiom structure
    expect_true("efficiency" %in% names(fairness[[comp]]))
    expect_true("satisfied" %in% names(fairness[[comp]]$efficiency))
    expect_true("error" %in% names(fairness[[comp]]$efficiency))
    
    # Check symmetry axiom structure
    expect_true("symmetry" %in% names(fairness[[comp]]))
    expect_true("n_groups" %in% names(fairness[[comp]]$symmetry))
    
    # Check dummy axiom structure
    expect_true("dummy" %in% names(fairness[[comp]]))
    expect_true("n_dummy_taxa" %in% names(fairness[[comp]]$dummy))
  }
})

test_that("Generate all coalitions works correctly", {
  
  # Test small cases
  coalitions_2 <- diversityGPT:::generate_all_coalitions(2)
  expect_equal(length(coalitions_2), 4)  # 2^2 = 4 coalitions
  
  # Check that we have empty set, singletons, and full set
  coalition_sizes <- sapply(coalitions_2, length)
  expect_true(0 %in% coalition_sizes)  # Empty set
  expect_true(1 %in% coalition_sizes)  # Singletons
  expect_true(2 %in% coalition_sizes)  # Full set
  
  coalitions_3 <- diversityGPT:::generate_all_coalitions(3)
  expect_equal(length(coalitions_3), 8)  # 2^3 = 8 coalitions
  
  # Test that all coalitions are unique
  coalition_strings <- sapply(coalitions_3, function(x) paste(sort(x), collapse = ","))
  expect_equal(length(unique(coalition_strings)), length(coalition_strings))
})

test_that("Exact Shapley calculation works for small problems", {
  
  demo_data <- create_demo_phyloseq(n_samples = 5, n_taxa = 4)
  info <- extract_universal_information(demo_data)
  
  # Get subset data
  otu_mat <- as.matrix(phyloseq::otu_table(demo_data))
  if (!phyloseq::taxa_are_rows(demo_data)) otu_mat <- t(otu_mat)
  
  taxa_names <- rownames(otu_mat)[1:3]  # Use first 3 taxa
  comp_data <- diversityGPT:::prepare_component_data_for_shapley(info, ncol(otu_mat))
  char_func <- diversityGPT:::create_characteristic_function(otu_mat[1:3, ], comp_data, verbose = FALSE)
  
  # Calculate exact Shapley values
  exact_results <- diversityGPT:::calculate_exact_shapley_values(
    taxa_names, char_func, verbose = FALSE, parallel = FALSE
  )
  
  # Check structure
  expect_true("shapley_matrix" %in% names(exact_results))
  expect_true("coalitional_values" %in% names(exact_results))
  expect_true("marginal_contributions" %in% names(exact_results))
  
  # Check dimensions
  expect_equal(nrow(exact_results$shapley_matrix), 3)
  expect_equal(ncol(exact_results$shapley_matrix), 4)
  
  # Check that all values are finite
  expect_true(all(is.finite(exact_results$shapley_matrix)))
})

test_that("Sampling Shapley approximation works", {
  
  demo_data <- create_demo_phyloseq(n_samples = 5, n_taxa = 6)
  info <- extract_universal_information(demo_data)
  
  otu_mat <- as.matrix(phyloseq::otu_table(demo_data))
  if (!phyloseq::taxa_are_rows(demo_data)) otu_mat <- t(otu_mat)
  
  taxa_names <- rownames(otu_mat)[1:4]
  comp_data <- diversityGPT:::prepare_component_data_for_shapley(info, ncol(otu_mat))
  char_func <- diversityGPT:::create_characteristic_function(otu_mat[1:4, ], comp_data, verbose = FALSE)
  
  # Calculate sampling-based Shapley values
  sampling_results <- diversityGPT:::calculate_sampling_shapley_values(
    taxa_names, char_func, n_samples = 100, verbose = FALSE
  )
  
  # Check structure
  expect_true("shapley_matrix" %in% names(sampling_results))
  expect_true("marginal_contributions" %in% names(sampling_results))
  
  # Check dimensions
  expect_equal(nrow(sampling_results$shapley_matrix), 4)
  expect_equal(ncol(sampling_results$shapley_matrix), 4)
  expect_equal(dim(sampling_results$marginal_contributions), c(4, 100, 4))
  
  # Check that all values are finite
  expect_true(all(is.finite(sampling_results$shapley_matrix)))
})

test_that("Approximation quality assessment works", {
  
  # Create mock shapley results
  shapley_matrix <- matrix(rnorm(12), nrow = 3, ncol = 4)
  marginal_contributions <- array(rnorm(3 * 50 * 4), dim = c(3, 50, 4))
  
  shapley_results <- list(
    shapley_matrix = shapley_matrix,
    marginal_contributions = marginal_contributions,
    selected_taxa = paste0("OTU", 1:3)
  )
  
  # Test sampling quality assessment
  quality_sampling <- diversityGPT:::assess_approximation_quality(
    shapley_results, "sampling", 50
  )
  
  expect_true("method" %in% names(quality_sampling))
  expect_true("n_samples" %in% names(quality_sampling))
  expect_true("variance_estimates" %in% names(quality_sampling))
  expect_true("margin_of_error" %in% names(quality_sampling))
  expect_equal(quality_sampling$method, "sampling")
  expect_equal(quality_sampling$n_samples, 50)
  
  # Test marginal quality assessment
  quality_marginal <- diversityGPT:::assess_approximation_quality(
    shapley_results, "marginal", 0
  )
  
  expect_equal(quality_marginal$method, "marginal")
  expect_true("approximation_type" %in% names(quality_marginal))
})

test_that("Find symmetric taxa works", {
  
  # Test with some symmetric values
  shapley_values <- c(0.1, 0.1, 0.2, 0.15, 0.15, 0.3)
  
  groups <- diversityGPT:::find_symmetric_taxa(shapley_values, tolerance = 0.01)
  
  # Should find two groups: (1,2) and (4,5)
  expect_equal(length(groups), 2)
  expect_true(all(c(1, 2) %in% groups[[1]] | c(1, 2) %in% groups[[2]]))
  expect_true(all(c(4, 5) %in% groups[[1]] | c(4, 5) %in% groups[[2]]))
  
  # Test with no symmetric values
  unique_values <- c(0.1, 0.2, 0.3, 0.4)
  groups_unique <- diversityGPT:::find_symmetric_taxa(unique_values, tolerance = 0.01)
  expect_equal(length(groups_unique), 0)
})

test_that("Taxa ranking works correctly", {
  
  # Create test Shapley matrix
  shapley_matrix <- matrix(c(
    0.3, 0.1, 0.2, 0.0,  # Taxon 1
    0.1, 0.4, 0.1, 0.3,  # Taxon 2
    0.2, 0.2, 0.3, 0.1   # Taxon 3
  ), nrow = 3, byrow = TRUE)
  
  rownames(shapley_matrix) <- paste0("OTU", 1:3)
  colnames(shapley_matrix) <- c("richness", "evenness", "phylogenetic", "spatial")
  
  taxa_names <- rownames(shapley_matrix)
  
  rankings <- diversityGPT:::rank_taxa_by_shapley_values(shapley_matrix, taxa_names)
  
  # Check structure
  expect_equal(length(rankings), 4)  # One ranking per component
  expect_true(all(c("richness", "evenness", "phylogenetic", "spatial") %in% names(rankings)))
  
  # Check richness ranking (OTU1 should be first with 0.3)
  richness_ranking <- rankings$richness
  expect_equal(richness_ranking$taxon[1], "OTU1")
  expect_equal(richness_ranking$shapley_value[1], 0.3)
  expect_equal(nrow(richness_ranking), 3)
  
  # Check that rankings are sorted correctly
  for (comp in names(rankings)) {
    ranking <- rankings[[comp]]
    expect_true(all(diff(ranking$shapley_value) <= 0))  # Decreasing order
    expect_equal(ranking$final_rank, 1:nrow(ranking))
  }
})

test_that("Print method works", {
  
  demo_data <- create_demo_phyloseq(n_samples = 6, n_taxa = 8)
  info <- extract_universal_information(demo_data)
  
  shapley_results <- calculate_taxa_shapley_values(
    demo_data, info,
    top_n = 5,
    method = "marginal",
    verbose = FALSE
  )
  
  # Test print method
  expect_output(print(shapley_results), "Shapley Value Attribution Results")
  expect_output(print(shapley_results), "Method:")
  expect_output(print(shapley_results), "OVERALL STATISTICS:")
  expect_output(print(shapley_results), "FAIRNESS PROPERTIES:")
})

test_that("Plot methods work", {
  
  demo_data <- create_demo_phyloseq(n_samples = 6, n_taxa = 10)
  info <- extract_universal_information(demo_data)
  
  shapley_results <- calculate_taxa_shapley_values(
    demo_data, info,
    top_n = 6,
    method = "marginal",
    verbose = FALSE
  )
  
  # Test waterfall plot
  expect_no_error({
    p1 <- plot(shapley_results, type = "waterfall", component = "richness")
  })
  expect_s3_class(p1, "ggplot")
  
  # Test contribution plot
  expect_no_error({
    p2 <- plot(shapley_results, type = "contribution", top_n = 5)
  })
  expect_s3_class(p2, "ggplot")
  
  # Test comparison plot
  expect_no_error({
    p3 <- plot(shapley_results, type = "comparison", top_n = 5)
  })
  expect_s3_class(p3, "ggplot")
  
  # Test network plot
  expect_no_error({
    p4 <- plot(shapley_results, type = "network", top_n = 4)
  })
  # Network plot might return different types depending on data
})

test_that("Edge cases are handled correctly", {
  
  # Test with minimal data
  mini_data <- create_demo_phyloseq(n_samples = 3, n_taxa = 2)
  info <- extract_universal_information(mini_data)
  
  expect_no_error({
    shapley_results <- calculate_taxa_shapley_values(
      mini_data, info,
      top_n = 2,
      method = "exact",
      verbose = FALSE
    )
  })
  
  expect_equal(nrow(shapley_results$shapley_matrix), 2)
  
  # Test with single taxon
  single_taxon_data <- create_demo_phyloseq(n_samples = 4, n_taxa = 1)
  info_single <- extract_universal_information(single_taxon_data)
  
  expect_no_error({
    shapley_single <- calculate_taxa_shapley_values(
      single_taxon_data, info_single,
      top_n = 1,
      method = "exact",
      verbose = FALSE
    )
  })
  
  expect_equal(nrow(shapley_single$shapley_matrix), 1)
})

test_that("Computational complexity estimates work", {
  
  # Test complexity estimates
  exact_complexity <- diversityGPT:::get_complexity_estimate("exact", 5)
  expect_true(grepl("O\\(2\\^5\\)", exact_complexity))
  
  sampling_complexity <- diversityGPT:::get_complexity_estimate("sampling", 10)
  expect_true(grepl("O\\(n \\* k\\)", sampling_complexity))
  
  marginal_complexity <- diversityGPT:::get_complexity_estimate("marginal", 8)
  expect_true(grepl("O\\(n\\^2\\)", marginal_complexity))
})

test_that("Summary statistics are reasonable", {
  
  demo_data <- create_demo_phyloseq(n_samples = 6, n_taxa = 8)
  info <- extract_universal_information(demo_data)
  
  shapley_results <- calculate_taxa_shapley_values(
    demo_data, info,
    top_n = 5,
    method = "sampling",
    n_samples = 50,
    verbose = FALSE
  )
  
  stats <- shapley_results$summary_stats
  
  # Check overall statistics
  expect_equal(stats$overall$n_taxa, 5)
  expect_equal(stats$overall$n_components, 4)
  expect_equal(stats$overall$method, "sampling")
  expect_true(is.logical(stats$overall$efficiency_satisfied))
  
  # Check component statistics
  components <- c("richness", "evenness", "phylogenetic", "spatial")
  for (comp in components) {
    comp_stats <- stats[[comp]]
    expect_true(is.character(comp_stats$top_contributor))
    expect_true(is.numeric(comp_stats$max_shapley_value))
    expect_gte(comp_stats$positive_contributors, 0)
    expect_gte(comp_stats$negative_contributors, 0)
    expect_lte(comp_stats$concentration_index, 1)
  }
  
  # Check fairness statistics
  expect_true("fairness" %in% names(stats))
  expect_true(all(is.numeric(stats$fairness$efficiency_errors)))
  expect_true(all(stats$fairness$symmetry_groups >= 0))
  expect_true(all(stats$fairness$dummy_taxa_detected >= 0))
})