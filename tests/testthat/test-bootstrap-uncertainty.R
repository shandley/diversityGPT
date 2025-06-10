# Tests for Bootstrap Uncertainty Module

library(testthat)
library(diversityGPT)

# Test data setup
test_that("Bootstrap uncertainty functions work with demo data", {
  
  # Create test data
  demo_data <- create_demo_phyloseq(n_samples = 12, n_taxa = 15)
  info <- extract_universal_information(demo_data)
  
  # Test main function with minimal bootstrap
  expect_no_error({
    boot_results <- bootstrap_taxa_indicators(
      demo_data, 
      info,
      indicator_methods = c("mutual_info"),
      n_bootstrap = 19,  # Small for testing speed
      confidence_level = 0.95,
      verbose = FALSE
    )
  })
  
  # Check structure
  expect_s3_class(boot_results, "bootstrap_indicators")
  expect_true(all(c("confidence_intervals", "bootstrap_distributions", "uncertainty_metrics",
                   "consensus_rankings", "reliability_assessment", "method_comparison") %in% names(boot_results)))
  
  # Check bootstrap configuration
  expect_equal(boot_results$n_bootstrap, 19)
  expect_equal(boot_results$confidence_level, 0.95)
  expect_true("mutual_info" %in% boot_results$indicator_methods)
})

test_that("Multiple indicator methods work in bootstrap", {
  
  demo_data <- create_demo_phyloseq(n_samples = 10, n_taxa = 12)
  info <- extract_universal_information(demo_data)
  
  # Test with multiple methods
  expect_no_error({
    boot_results <- bootstrap_taxa_indicators(
      demo_data, 
      info,
      indicator_methods = c("mutual_info", "shapley"),
      n_bootstrap = 9,  # Very small for speed
      verbose = FALSE
    )
  })
  
  # Check that both methods have results
  expect_true("mutual_info" %in% names(boot_results$bootstrap_distributions))
  expect_true("shapley" %in% names(boot_results$bootstrap_distributions))
  
  # Check that confidence intervals exist for both
  expect_true("mutual_info" %in% names(boot_results$confidence_intervals))
  expect_true("shapley" %in% names(boot_results$confidence_intervals))
})

test_that("Confidence interval calculation works", {
  
  # Create test distribution array
  set.seed(123)
  test_distributions <- array(
    rnorm(100 * 5 * 2),  # 100 bootstrap x 5 taxa x 2 components
    dim = c(100, 5, 2),
    dimnames = list(NULL, paste0("OTU", 1:5), c("richness", "evenness"))
  )
  
  # Calculate confidence intervals
  ci_results <- diversityGPT:::calculate_confidence_intervals(test_distributions, 0.95)
  
  # Check structure
  expect_true(all(c("lower", "upper", "median", "confidence_level") %in% names(ci_results)))
  expect_equal(dim(ci_results$lower), c(5, 2))
  expect_equal(dim(ci_results$upper), c(5, 2))
  expect_equal(dim(ci_results$median), c(5, 2))
  expect_equal(ci_results$confidence_level, 0.95)
  
  # Check that intervals make sense
  expect_true(all(ci_results$lower <= ci_results$median, na.rm = TRUE))
  expect_true(all(ci_results$median <= ci_results$upper, na.rm = TRUE))
})

test_that("Uncertainty metrics calculation works", {
  
  # Create test distribution array
  set.seed(456)
  test_distributions <- array(
    rnorm(50 * 4 * 3, mean = 0.5, sd = 0.1),
    dim = c(50, 4, 3),
    dimnames = list(NULL, paste0("Taxa", 1:4), c("richness", "evenness", "phylogenetic"))
  )
  
  # Calculate uncertainty metrics
  uncertainty <- diversityGPT:::calculate_uncertainty_metrics(test_distributions, "test_method")
  
  # Check structure
  expect_true(all(c("means", "variances", "std_devs", "cv", "reliability", 
                   "stability", "rank_stability", "method") %in% names(uncertainty)))
  
  # Check dimensions
  expect_equal(dim(uncertainty$means), c(4, 3))
  expect_equal(dim(uncertainty$cv), c(4, 3))
  expect_equal(uncertainty$method, "test_method")
  
  # Check that CV and stability are reasonable
  expect_true(all(uncertainty$cv >= 0, na.rm = TRUE))
  expect_true(all(uncertainty$stability >= 0 & uncertainty$stability <= 1, na.rm = TRUE))
  expect_true(all(uncertainty$reliability >= 0 & uncertainty$reliability <= 1, na.rm = TRUE))
})

test_that("Bootstrap sampling methods work", {
  
  demo_data <- create_demo_phyloseq(n_samples = 8, n_taxa = 6)
  info <- extract_universal_information(demo_data)
  
  # Test nonparametric bootstrap
  expect_no_error({
    boot_nonparam <- bootstrap_taxa_indicators(
      demo_data, info,
      indicator_methods = "mutual_info",
      n_bootstrap = 5,
      bootstrap_method = "nonparametric",
      verbose = FALSE
    )
  })
  expect_equal(boot_nonparam$bootstrap_method, "nonparametric")
  
  # Test parametric bootstrap
  expect_no_error({
    boot_param <- bootstrap_taxa_indicators(
      demo_data, info,
      indicator_methods = "mutual_info",
      n_bootstrap = 5,
      bootstrap_method = "parametric",
      verbose = FALSE
    )
  })
  expect_equal(boot_param$bootstrap_method, "parametric")
})

test_that("Parametric bootstrap generation works", {
  
  # Create test OTU matrix
  set.seed(789)
  otu_mat <- matrix(
    rpois(20, lambda = 10),
    nrow = 4, ncol = 5,
    dimnames = list(paste0("OTU", 1:4), paste0("Sample", 1:5))
  )
  
  # Generate parametric bootstrap
  boot_otu <- diversityGPT:::generate_parametric_bootstrap_microbiome(otu_mat)
  
  # Check structure
  expect_equal(dim(boot_otu), dim(otu_mat))
  expect_equal(rownames(boot_otu), rownames(otu_mat))
  expect_equal(colnames(boot_otu), colnames(otu_mat))
  
  # Check that values are non-negative integers
  expect_true(all(boot_otu >= 0))
  expect_true(all(boot_otu == as.integer(boot_otu)))
})

test_that("Bootstrap phyloseq creation works", {
  
  demo_data <- create_demo_phyloseq(n_samples = 6, n_taxa = 8)
  
  # Get original OTU matrix
  otu_mat <- as.matrix(phyloseq::otu_table(demo_data))
  if (!phyloseq::taxa_are_rows(demo_data)) otu_mat <- t(otu_mat)
  
  # Create bootstrap OTU matrix (just resample columns)
  boot_indices <- sample(1:ncol(otu_mat), ncol(otu_mat), replace = TRUE)
  boot_otu <- otu_mat[, boot_indices, drop = FALSE]
  
  # Create bootstrap phyloseq
  boot_physeq <- diversityGPT:::create_bootstrap_phyloseq(boot_otu, demo_data)
  
  # Check structure
  expect_s4_class(boot_physeq, "phyloseq")
  expect_equal(phyloseq::ntaxa(boot_physeq), phyloseq::ntaxa(demo_data))
  expect_equal(phyloseq::nsamples(boot_physeq), phyloseq::nsamples(demo_data))
})

test_that("Rank stability calculation works", {
  
  # Create test distribution with known rank patterns
  set.seed(999)
  n_bootstrap <- 20
  n_taxa <- 5
  n_components <- 2
  
  # Create distributions where first taxon is consistently highest
  test_distributions <- array(NA, dim = c(n_bootstrap, n_taxa, n_components))
  
  for (i in 1:n_bootstrap) {
    # First taxon always highest, others random
    test_distributions[i, 1, ] <- rnorm(n_components, mean = 10, sd = 1)
    test_distributions[i, 2:n_taxa, ] <- rnorm((n_taxa-1) * n_components, mean = 5, sd = 2)
  }
  
  # Calculate rank stability
  rank_stability <- diversityGPT:::calculate_rank_stability(test_distributions)
  
  # Check structure
  expect_equal(dim(rank_stability), c(n_taxa, n_components))
  
  # First taxon should have highest stability
  expect_true(all(rank_stability[1, ] >= rank_stability[2, ], na.rm = TRUE))
  
  # All stability values should be between 0 and 1
  expect_true(all(rank_stability >= 0 & rank_stability <= 1, na.rm = TRUE))
})

test_that("Consensus rankings work", {
  
  # Create mock bootstrap results for multiple methods
  set.seed(111)
  
  method1_dist <- array(
    rnorm(30 * 4 * 2),
    dim = c(30, 4, 2),
    dimnames = list(NULL, paste0("OTU", 1:4), c("richness", "evenness"))
  )
  
  method2_dist <- array(
    rnorm(30 * 4 * 2, mean = 0.5),
    dim = c(30, 4, 2),
    dimnames = list(NULL, paste0("OTU", 1:4), c("richness", "evenness"))
  )
  
  bootstrap_results <- list(
    method1 = method1_dist,
    method2 = method2_dist
  )
  
  # Create confidence intervals
  confidence_intervals <- list(
    method1 = diversityGPT:::calculate_confidence_intervals(method1_dist),
    method2 = diversityGPT:::calculate_confidence_intervals(method2_dist)
  )
  
  # Create consensus rankings
  consensus <- diversityGPT:::create_consensus_rankings_with_uncertainty(
    bootstrap_results, confidence_intervals, c("method1", "method2")
  )
  
  # Check structure
  expect_true("richness" %in% names(consensus))
  expect_true("evenness" %in% names(consensus))
  
  richness_consensus <- consensus$richness
  expect_true(all(c("taxon", "consensus_score", "consensus_rank", "uncertainty") %in% names(richness_consensus)))
  expect_equal(nrow(richness_consensus), 4)  # 4 taxa
})

test_that("Bootstrap convergence assessment works", {
  
  # Create test distribution with good convergence
  set.seed(222)
  stable_series <- rnorm(100, mean = 0.5, sd = 0.05)  # Low variance
  
  stable_distributions <- array(
    stable_series, 
    dim = c(100, 1, 1),
    dimnames = list(NULL, "OTU1", "richness")
  )
  
  convergence <- diversityGPT:::assess_bootstrap_convergence(stable_distributions)
  
  # Check structure
  expect_true(all(c("converged", "cv_final", "n_valid", "running_means") %in% names(convergence)))
  expect_true(is.logical(convergence$converged))
  expect_true(is.numeric(convergence$cv_final))
  
  # With low variance, should converge
  expect_true(convergence$converged)
  expect_true(convergence$cv_final < 0.1)
})

test_that("Method correlation calculation works", {
  
  # Create bootstrap results for two methods with some correlation
  set.seed(333)
  
  # Correlated data
  base_values <- rnorm(4, mean = 0.5, sd = 0.2)
  
  method1_dist <- array(NA, dim = c(20, 4, 1), dimnames = list(NULL, paste0("OTU", 1:4), "richness"))
  method2_dist <- array(NA, dim = c(20, 4, 1), dimnames = list(NULL, paste0("OTU", 1:4), "richness"))
  
  for (i in 1:20) {
    method1_dist[i, , 1] <- base_values + rnorm(4, sd = 0.1)
    method2_dist[i, , 1] <- base_values + rnorm(4, sd = 0.1)  # Similar to method1
  }
  
  bootstrap_results <- list(
    method1 = method1_dist,
    method2 = method2_dist
  )
  
  # Calculate correlations
  correlations <- diversityGPT:::calculate_method_correlations(bootstrap_results)
  
  # Check structure
  expect_true("richness" %in% names(correlations))
  
  richness_cor <- correlations$richness
  expect_equal(dim(richness_cor), c(2, 2))
  expect_equal(rownames(richness_cor), c("method1", "method2"))
  expect_equal(colnames(richness_cor), c("method1", "method2"))
  
  # Diagonal should be 1
  expect_equal(diag(richness_cor), c(1, 1))
  
  # Should be positive correlation
  expect_true(richness_cor[1, 2] > 0)
})

test_that("Print method works", {
  
  demo_data <- create_demo_phyloseq(n_samples = 8, n_taxa = 10)
  info <- extract_universal_information(demo_data)
  
  boot_results <- bootstrap_taxa_indicators(
    demo_data, info,
    indicator_methods = "mutual_info",
    n_bootstrap = 9,
    verbose = FALSE
  )
  
  # Test print method
  expect_output(print(boot_results), "Bootstrap Uncertainty Analysis Results")
  expect_output(print(boot_results), "BOOTSTRAP CONFIGURATION:")
  expect_output(print(boot_results), "Replicates:")
  expect_output(print(boot_results), "RELIABILITY SUMMARY:")
})

test_that("Plot methods work", {
  
  demo_data <- create_demo_phyloseq(n_samples = 8, n_taxa = 10)
  info <- extract_universal_information(demo_data)
  
  boot_results <- bootstrap_taxa_indicators(
    demo_data, info,
    indicator_methods = c("mutual_info"),
    n_bootstrap = 9,
    verbose = FALSE
  )
  
  # Test confidence intervals plot
  expect_no_error({
    p1 <- plot(boot_results, type = "confidence_intervals", component = "richness")
  })
  
  # Test uncertainty plot
  expect_no_error({
    p2 <- plot(boot_results, type = "uncertainty", component = "richness")
  })
  
  # Test consensus plot
  expect_no_error({
    p3 <- plot(boot_results, type = "consensus", top_n = 5)
  })
  
  # Some plots might return NULL if no data, but should not error
})

test_that("Edge cases are handled correctly", {
  
  # Test with minimal data
  mini_data <- create_demo_phyloseq(n_samples = 4, n_taxa = 3)
  info <- extract_universal_information(mini_data)
  
  expect_no_error({
    boot_results <- bootstrap_taxa_indicators(
      mini_data, info,
      indicator_methods = "mutual_info",
      n_bootstrap = 5,
      verbose = FALSE
    )
  })
  
  expect_s3_class(boot_results, "bootstrap_indicators")
  
  # Test with single method
  expect_no_error({
    boot_single <- bootstrap_taxa_indicators(
      mini_data, info,
      indicator_methods = "mutual_info",
      n_bootstrap = 3,
      verbose = FALSE
    )
  })
  
  expect_equal(length(boot_single$indicator_methods), 1)
})

test_that("Different confidence levels work", {
  
  demo_data <- create_demo_phyloseq(n_samples = 6, n_taxa = 8)
  info <- extract_universal_information(demo_data)
  
  # Test different confidence levels
  expect_no_error({
    boot_90 <- bootstrap_taxa_indicators(
      demo_data, info,
      indicator_methods = "mutual_info",
      n_bootstrap = 9,
      confidence_level = 0.90,
      verbose = FALSE
    )
  })
  expect_equal(boot_90$confidence_level, 0.90)
  
  expect_no_error({
    boot_99 <- bootstrap_taxa_indicators(
      demo_data, info,
      indicator_methods = "mutual_info",
      n_bootstrap = 9,
      confidence_level = 0.99,
      verbose = FALSE
    )
  })
  expect_equal(boot_99$confidence_level, 0.99)
})

test_that("Seed setting ensures reproducibility", {
  
  demo_data <- create_demo_phyloseq(n_samples = 6, n_taxa = 8)
  info <- extract_universal_information(demo_data)
  
  # Run bootstrap twice with same seed
  boot1 <- bootstrap_taxa_indicators(
    demo_data, info,
    indicator_methods = "mutual_info",
    n_bootstrap = 9,
    seed = 12345,
    verbose = FALSE
  )
  
  boot2 <- bootstrap_taxa_indicators(
    demo_data, info,
    indicator_methods = "mutual_info",
    n_bootstrap = 9,
    seed = 12345,
    verbose = FALSE
  )
  
  # Results should be identical
  expect_equal(boot1$confidence_intervals, boot2$confidence_intervals)
})