# test-robust-taxa-indicators.R
# Comprehensive tests for robust taxa indicator analysis

test_that("robust_taxa_indicators works with all methods", {
  # Create test data
  set.seed(123)
  test_physeq <- create_demo_phyloseq(n_samples = 10, n_taxa = 20)
  
  # Test with all methods
  result <- robust_taxa_indicators(
    test_physeq,
    methods = c("null_models", "mutual_info", "shapley", "bootstrap"),
    top_n = 5,
    n_permutations = 10,  # Small for testing
    n_bootstrap = 10,
    verbose = FALSE
  )
  
  # Check structure
  expect_s3_class(result, "robust_taxa_indicators")
  expect_named(result, c("consensus_indicators", "null_model_results", 
                        "mutual_info_results", "shapley_results",
                        "bootstrap_results", "method_agreement",
                        "reliability_scores", "summary_report",
                        "methods_used", "call", "n_samples", "n_taxa",
                        "top_n", "confidence_level"))
  
  # Check consensus indicators
  expect_type(result$consensus_indicators, "list")
  expect_true(all(c("richness", "evenness") %in% names(result$consensus_indicators)))
  
  # Check each consensus has required columns
  for (comp in names(result$consensus_indicators)) {
    if (nrow(result$consensus_indicators[[comp]]) > 0) {
      expect_true(all(c("taxon", "consensus_score", "n_methods", 
                       "reliability", "rank") %in% 
                     names(result$consensus_indicators[[comp]])))
    }
  }
  
  # Check method agreement
  expect_type(result$method_agreement, "list")
  expect_true("overall" %in% names(result$method_agreement))
  
  # Check reliability scores
  expect_type(result$reliability_scores, "list")
  expect_true("overall_score" %in% names(result$reliability_scores))
  expect_true(result$reliability_scores$overall_score >= 0 && 
              result$reliability_scores$overall_score <= 1)
})

test_that("robust_taxa_indicators works with subset of methods", {
  test_physeq <- create_demo_phyloseq(n_samples = 8, n_taxa = 15)
  
  # Test with only two methods
  result <- robust_taxa_indicators(
    test_physeq,
    methods = c("mutual_info", "shapley"),
    top_n = 3,
    verbose = FALSE
  )
  
  expect_s3_class(result, "robust_taxa_indicators")
  expect_equal(result$methods_used, c("mutual_info", "shapley"))
  expect_null(result$null_model_results)
  expect_null(result$bootstrap_results)
})

test_that("robust_taxa_indicators handles single method", {
  test_physeq <- create_demo_phyloseq(n_samples = 5, n_taxa = 10)
  
  result <- robust_taxa_indicators(
    test_physeq,
    methods = "mutual_info",
    top_n = 3,
    verbose = FALSE
  )
  
  expect_s3_class(result, "robust_taxa_indicators")
  expect_equal(length(result$methods_used), 1)
})

test_that("create_robust_consensus works correctly", {
  # Create mock results
  mock_results <- list(
    null_model_results = list(
      effect_sizes = list(
        richness = c(taxon1 = 2.5, taxon2 = 1.8, taxon3 = 1.2),
        evenness = c(taxon1 = 0.5, taxon2 = 1.0, taxon3 = 0.8)
      ),
      p_values = list(
        richness = c(taxon1 = 0.01, taxon2 = 0.03, taxon3 = 0.1),
        evenness = c(taxon1 = 0.2, taxon2 = 0.04, taxon3 = 0.05)
      )
    ),
    mutual_info_results = list(
      taxa_rankings = list(
        richness = data.frame(
          taxon = c("taxon1", "taxon2", "taxon3"),
          normalized_mi = c(0.8, 0.6, 0.4),
          stringsAsFactors = FALSE
        ),
        evenness = data.frame(
          taxon = c("taxon2", "taxon3", "taxon1"),
          normalized_mi = c(0.7, 0.5, 0.3),
          stringsAsFactors = FALSE
        )
      )
    )
  )
  
  components <- list(richness = 1, evenness = 1)
  
  consensus <- create_robust_consensus(mock_results, components, top_n = 3)
  
  expect_type(consensus, "list")
  expect_true("richness" %in% names(consensus))
  expect_true("evenness" %in% names(consensus))
  
  # Check richness consensus
  rich_consensus <- consensus$richness
  expect_s3_class(rich_consensus, "data.frame")
  expect_true("taxon" %in% names(rich_consensus))
  expect_true("consensus_score" %in% names(rich_consensus))
  expect_true("reliability" %in% names(rich_consensus))
})

test_that("assess_method_agreement_robust calculates correctly", {
  # Create test results with known agreement
  test_results <- list(
    null_model_results = list(
      effect_sizes = list(
        richness = c(taxon1 = 2.0, taxon2 = 1.5, taxon3 = 1.0)
      ),
      p_values = list(
        richness = c(taxon1 = 0.01, taxon2 = 0.02, taxon3 = 0.03)
      )
    ),
    mutual_info_results = list(
      taxa_rankings = list(
        richness = data.frame(
          taxon = c("taxon1", "taxon2", "taxon3"),
          normalized_mi = c(0.9, 0.7, 0.5)
        )
      )
    )
  )
  
  agreement <- assess_method_agreement_robust(test_results)
  
  expect_type(agreement, "list")
  expect_true("richness" %in% names(agreement))
  expect_true("overall" %in% names(agreement))
  
  # Check Jaccard calculation
  if (!is.null(agreement$richness$jaccard_matrix)) {
    jaccard <- agreement$richness$jaccard_matrix
    expect_true(all(diag(jaccard) == 1))  # Self-agreement is 1
    expect_true(all(jaccard >= 0 & jaccard <= 1))  # Valid range
  }
})

test_that("calculate_reliability_scores works", {
  mock_results <- list(
    null_model_results = list(
      p_values = list(
        richness = c(0.01, 0.02, 0.5, 0.8),
        evenness = c(0.03, 0.04, 0.05, 0.1)
      ),
      effect_sizes = list(
        richness = c(2.0, 1.5, 0.5, 0.2),
        evenness = c(1.8, 1.2, 0.8, 0.6)
      )
    ),
    mutual_info_results = list(
      summary_stats = list(
        total_information = list(richness = 2.5, evenness = 2.0)
      ),
      taxa_rankings = list(
        richness = data.frame(normalized_mi = c(0.8, 0.6, 0.4)),
        evenness = data.frame(normalized_mi = c(0.7, 0.5, 0.3))
      )
    )
  )
  
  reliability <- calculate_reliability_scores(mock_results)
  
  expect_type(reliability, "list")
  expect_true("overall_score" %in% names(reliability))
  expect_true(reliability$overall_score >= 0 && reliability$overall_score <= 1)
  
  # Check components
  if (!is.null(reliability$null_models)) {
    expect_true("significance_rate" %in% names(reliability$null_models))
    expect_true("mean_effect_size" %in% names(reliability$null_models))
  }
})

test_that("interpret_reliability_score provides correct interpretations", {
  expect_match(interpret_reliability_score(0.9), "Very high reliability")
  expect_match(interpret_reliability_score(0.7), "High reliability")
  expect_match(interpret_reliability_score(0.5), "Moderate reliability")
  expect_match(interpret_reliability_score(0.2), "Low reliability")
})

test_that("print.robust_taxa_indicators works", {
  test_physeq <- create_demo_phyloseq(n_samples = 5, n_taxa = 10)
  result <- robust_taxa_indicators(
    test_physeq,
    methods = "mutual_info",
    top_n = 3,
    verbose = FALSE
  )
  
  # Capture print output
  output <- capture.output(print(result))
  
  expect_true(any(grepl("Robust Taxa Indicator Analysis Results", output)))
  expect_true(any(grepl("DATASET:", output)))
  expect_true(any(grepl("METHODS:", output)))
})

test_that("plot.robust_taxa_indicators creates plots", {
  test_physeq <- create_demo_phyloseq(n_samples = 5, n_taxa = 10)
  result <- robust_taxa_indicators(
    test_physeq,
    methods = c("mutual_info", "shapley"),
    top_n = 3,
    verbose = FALSE
  )
  
  # Test consensus plot
  p1 <- plot(result, type = "consensus", component = "richness")
  expect_s3_class(p1, "gg")
  
  # Test other plot types should exist but may error with mock data
  expect_error(plot(result, type = "invalid"))
})

test_that("robust_taxa_indicators handles errors gracefully", {
  # Test with invalid phyloseq
  expect_error(robust_taxa_indicators("not_phyloseq"))
  
  # Test with invalid methods
  test_physeq <- create_demo_phyloseq(n_samples = 5, n_taxa = 10)
  expect_error(robust_taxa_indicators(test_physeq, methods = "invalid_method"))
  
  # Test with invalid parameters
  expect_error(robust_taxa_indicators(test_physeq, top_n = -1))
  expect_error(robust_taxa_indicators(test_physeq, n_permutations = 0))
})

test_that("robust_taxa_indicators works with custom components", {
  test_physeq <- create_demo_phyloseq(n_samples = 8, n_taxa = 15)
  
  # Extract components first
  components <- extract_universal_information(test_physeq)
  
  # Use pre-extracted components
  result <- robust_taxa_indicators(
    test_physeq,
    components = components,
    methods = "mutual_info",
    top_n = 3,
    verbose = FALSE
  )
  
  expect_s3_class(result, "robust_taxa_indicators")
})

test_that("generate_robust_summary creates proper summary", {
  test_physeq <- create_demo_phyloseq(n_samples = 10, n_taxa = 20)
  
  mock_results <- list(
    methods_used = c("mutual_info", "shapley"),
    consensus_indicators = list(
      richness = data.frame(
        taxon = c("taxon1", "taxon2"),
        consensus_score = c(0.9, 0.7),
        reliability = c(0.85, 0.75),
        n_methods = c(2, 2)
      )
    ),
    method_agreement = list(
      overall = list(mean_agreement = 0.8)
    ),
    reliability_scores = list(
      overall_score = 0.75
    )
  )
  
  summary <- generate_robust_summary(mock_results, test_physeq)
  
  expect_type(summary, "list")
  expect_true("dataset" %in% names(summary))
  expect_true("methods" %in% names(summary))
  expect_true("key_findings" %in% names(summary))
  expect_equal(summary$dataset$n_samples, 10)
  expect_equal(summary$dataset$n_taxa, 20)
})