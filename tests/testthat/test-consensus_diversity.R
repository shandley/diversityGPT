test_that("consensus_diversity works with basic input", {
  # Create mock diversity results
  mock_results <- structure(
    data.frame(
      sample = paste0("Sample", 1:10),
      shannon = c(rnorm(5, 3, 0.3), rnorm(5, 1.5, 0.3)),
      simpson = c(rnorm(5, 0.9, 0.05), rnorm(5, 0.6, 0.05)),
      chao1 = c(rnorm(5, 45, 5), rnorm(5, 20, 5)),
      group = rep(c("High", "Low"), each = 5)
    ),
    metrics = c("shannon", "simpson", "chao1"),
    n_samples = 10,
    n_taxa = 50,
    class = c("diversity_results", "data.frame")
  )
  
  # Test weighted mean consensus
  consensus <- consensus_diversity(mock_results, method = "weighted_mean")
  
  expect_s3_class(consensus, "consensus_results")
  expect_length(consensus$consensus_scores, 10)
  expect_length(consensus$method_weights, 3)
  expect_equal(names(consensus$method_weights), c("shannon", "simpson", "chao1"))
  expect_equal(sum(consensus$method_weights), 1, tolerance = 1e-10)
})

test_that("consensus_diversity handles groups correctly", {
  # Create mock data with clear group differences
  mock_results <- structure(
    data.frame(
      sample = paste0("Sample", 1:20),
      shannon = c(rnorm(10, 3, 0.1), rnorm(10, 1, 0.1)),  # Clear difference
      simpson = c(rnorm(10, 0.9, 0.01), rnorm(10, 0.5, 0.01)),  # Clear difference
      group = rep(c("Treatment", "Control"), each = 10)
    ),
    metrics = c("shannon", "simpson"),
    n_samples = 20,
    n_taxa = 50,
    class = c("diversity_results", "data.frame")
  )
  
  # Test majority vote with groups
  consensus <- consensus_diversity(mock_results, 
                                 method = "majority_vote", 
                                 groups = "group")
  
  expect_false(is.null(consensus$conflict_analysis))
  expect_true("metric_pvalues" %in% names(consensus$conflict_analysis))
  expect_equal(consensus$conflict_analysis$n_metrics_total, 2)
})

test_that("consensus_diversity validates input correctly", {
  # Test with non-diversity_results object
  expect_error(
    consensus_diversity(data.frame(x = 1:5)),
    "must be a diversity_results object"
  )
  
  # Test with insufficient metrics
  single_metric <- structure(
    data.frame(sample = 1:5, shannon = 1:5),
    metrics = "shannon",
    class = c("diversity_results", "data.frame")
  )
  
  expect_error(
    consensus_diversity(single_metric),
    "Need at least 2 diversity metrics"
  )
})

test_that("different consensus methods work", {
  mock_results <- structure(
    data.frame(
      sample = paste0("Sample", 1:8),
      shannon = rnorm(8, 2, 0.5),
      simpson = rnorm(8, 0.7, 0.1),
      chao1 = rnorm(8, 30, 8)
    ),
    metrics = c("shannon", "simpson", "chao1"),
    class = c("diversity_results", "data.frame")
  )
  
  # Test all methods
  methods <- c("weighted_mean", "correlation_weighted")
  
  for (method in methods) {
    consensus <- consensus_diversity(mock_results, method = method)
    expect_s3_class(consensus, "consensus_results")
    expect_equal(consensus$method, method)
    expect_length(consensus$consensus_scores, 8)
  }
})

test_that("print method works for consensus results", {
  mock_consensus <- structure(
    list(
      method = "weighted_mean",
      n_samples = 5,
      metrics_used = c("shannon", "simpson"),
      method_weights = c(shannon = 0.6, simpson = 0.4),
      interpretation = list(
        dominant_metric = "shannon",
        weight_distribution = "dominated",
        conflict_status = "consensus",
        conflict_details = "All metrics agree"
      )
    ),
    class = c("consensus_results", "list")
  )
  
  # Test that print doesn't error
  expect_no_error(print(mock_consensus))
})