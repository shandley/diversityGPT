# test-meta-analysis.R
# Tests for cross-study meta-analysis functions

test_that("cross_study_meta_analysis works with basic parameters", {
  # Create multiple test datasets
  study1 <- create_demo_phyloseq(n_samples = 10, n_taxa = 20)
  study2 <- create_demo_phyloseq(n_samples = 15, n_taxa = 25)
  study3 <- create_demo_phyloseq(n_samples = 8, n_taxa = 18)
  
  # Create study list
  studies <- list(
    study1 = study1,
    study2 = study2,
    study3 = study3
  )
  
  # Run meta-analysis
  result <- cross_study_meta_analysis(
    studies = studies,
    effect_metric = "shannon",
    moderators = NULL
  )
  
  # Check structure
  expect_s3_class(result, "meta_analysis")
  expect_type(result, "list")
  expect_true(all(c("effect_sizes", "pooled_effect", "heterogeneity", 
                   "study_weights", "forest_plot_data") %in% names(result)))
  
  # Check effect sizes
  expect_length(result$effect_sizes, 3)
  expect_true(all(!is.na(result$effect_sizes)))
  
  # Check pooled effect
  expect_type(result$pooled_effect, "list")
  expect_true(all(c("estimate", "se", "ci_lower", "ci_upper") %in% 
                 names(result$pooled_effect)))
  
  # Check heterogeneity
  expect_true(all(c("Q", "p_value", "I2") %in% names(result$heterogeneity)))
  expect_true(result$heterogeneity$I2 >= 0 && result$heterogeneity$I2 <= 100)
})

test_that("cross_study_meta_analysis handles different effect metrics", {
  # Create test datasets
  studies <- list(
    study1 = create_demo_phyloseq(n_samples = 10, n_taxa = 20),
    study2 = create_demo_phyloseq(n_samples = 12, n_taxa = 22)
  )
  
  # Test different metrics
  metrics <- c("shannon", "simpson", "richness", "evenness")
  
  for (metric in metrics) {
    result <- cross_study_meta_analysis(
      studies = studies,
      effect_metric = metric
    )
    
    expect_s3_class(result, "meta_analysis")
    expect_equal(result$effect_metric, metric)
    expect_true(all(!is.na(result$effect_sizes)))
  }
})

test_that("cross_study_meta_analysis with moderators", {
  # Create studies with metadata
  study1 <- create_demo_phyloseq(n_samples = 10, n_taxa = 20)
  study2 <- create_demo_phyloseq(n_samples = 15, n_taxa = 25)
  study3 <- create_demo_phyloseq(n_samples = 8, n_taxa = 18)
  
  studies <- list(
    study1 = study1,
    study2 = study2,
    study3 = study3
  )
  
  # Add moderators
  moderators <- data.frame(
    study = c("study1", "study2", "study3"),
    environment = c("gut", "soil", "gut"),
    sample_size = c(10, 15, 8)
  )
  
  result <- cross_study_meta_analysis(
    studies = studies,
    effect_metric = "shannon",
    moderators = moderators
  )
  
  expect_s3_class(result, "meta_analysis")
  expect_true("moderator_analysis" %in% names(result))
  
  # Check moderator analysis
  if (!is.null(result$moderator_analysis)) {
    expect_true("environment" %in% names(result$moderator_analysis))
  }
})

test_that("cross_study_meta_analysis handles universal transformation", {
  # Create studies with different metrics
  study1 <- create_demo_phyloseq(n_samples = 10, n_taxa = 20)
  study2 <- create_demo_phyloseq(n_samples = 12, n_taxa = 22)
  
  studies <- list(
    study1 = study1,
    study2 = study2
  )
  
  # Request transformation to common metric
  result <- cross_study_meta_analysis(
    studies = studies,
    effect_metric = "shannon",
    transform_to_common = TRUE
  )
  
  expect_s3_class(result, "meta_analysis")
  expect_true("transformation_quality" %in% names(result) ||
              "common_metric" %in% names(result))
})

test_that("cross_study_meta_analysis calculates weights correctly", {
  # Create studies with different sample sizes
  large_study <- create_demo_phyloseq(n_samples = 50, n_taxa = 100)
  small_study <- create_demo_phyloseq(n_samples = 5, n_taxa = 10)
  
  studies <- list(
    large = large_study,
    small = small_study
  )
  
  result <- cross_study_meta_analysis(
    studies = studies,
    effect_metric = "shannon",
    weight_by = "sample_size"
  )
  
  # Larger study should have higher weight
  expect_true(result$study_weights["large"] > result$study_weights["small"])
})

test_that("cross_study_meta_analysis detects heterogeneity", {
  # Create heterogeneous studies
  set.seed(123)
  study1 <- create_demo_phyloseq(n_samples = 10, n_taxa = 20)
  
  # Make study2 very different
  set.seed(456)
  study2_otu <- matrix(rpois(15 * 50, lambda = 100), nrow = 50)
  study2 <- phyloseq(
    otu_table(study2_otu, taxa_are_rows = TRUE),
    sample_data(data.frame(sample = paste0("S", 1:15)))
  )
  
  studies <- list(study1 = study1, study2 = study2)
  
  result <- cross_study_meta_analysis(
    studies = studies,
    effect_metric = "shannon"
  )
  
  # Should detect heterogeneity
  expect_true(result$heterogeneity$I2 > 0)
})

test_that("print.meta_analysis works", {
  studies <- list(
    study1 = create_demo_phyloseq(n_samples = 10, n_taxa = 20),
    study2 = create_demo_phyloseq(n_samples = 12, n_taxa = 22)
  )
  
  result <- cross_study_meta_analysis(studies, effect_metric = "shannon")
  
  # Capture print output
  output <- capture.output(print(result))
  
  expect_true(any(grepl("Cross-Study Meta-Analysis Results", output)))
  expect_true(any(grepl("Pooled effect", output)))
  expect_true(any(grepl("Heterogeneity", output)))
})

test_that("plot.meta_analysis creates forest plot", {
  studies <- list(
    study1 = create_demo_phyloseq(n_samples = 10, n_taxa = 20),
    study2 = create_demo_phyloseq(n_samples = 12, n_taxa = 22),
    study3 = create_demo_phyloseq(n_samples = 8, n_taxa = 15)
  )
  
  result <- cross_study_meta_analysis(studies, effect_metric = "shannon")
  
  # Create forest plot
  p <- plot(result, type = "forest")
  expect_s3_class(p, "gg")
  
  # Create funnel plot
  p2 <- plot(result, type = "funnel")
  expect_s3_class(p2, "gg")
})

test_that("generate_meta_analysis_report creates output", {
  studies <- list(
    study1 = create_demo_phyloseq(n_samples = 10, n_taxa = 20),
    study2 = create_demo_phyloseq(n_samples = 12, n_taxa = 22)
  )
  
  meta_result <- cross_study_meta_analysis(studies, effect_metric = "shannon")
  
  temp_file <- tempfile(fileext = ".html")
  
  result <- generate_meta_analysis_report(
    meta_result,
    output_file = temp_file,
    title = "Test Meta-Analysis"
  )
  
  expect_true(file.exists(temp_file))
  expect_equal(result, temp_file)
  
  unlink(temp_file)
})

test_that("cross_study_meta_analysis handles missing data", {
  # Create study with some missing samples
  study1 <- create_demo_phyloseq(n_samples = 10, n_taxa = 20)
  study2 <- create_demo_phyloseq(n_samples = 8, n_taxa = 15)
  
  # Remove some samples from study2
  study2 <- prune_samples(sample_names(study2)[1:5], study2)
  
  studies <- list(study1 = study1, study2 = study2)
  
  result <- cross_study_meta_analysis(
    studies = studies,
    effect_metric = "shannon",
    handle_missing = "complete"
  )
  
  expect_s3_class(result, "meta_analysis")
  expect_true(all(!is.na(result$effect_sizes)))
})

test_that("cross_study_meta_analysis random effects model", {
  studies <- list(
    study1 = create_demo_phyloseq(n_samples = 10, n_taxa = 20),
    study2 = create_demo_phyloseq(n_samples = 15, n_taxa = 25),
    study3 = create_demo_phyloseq(n_samples = 8, n_taxa = 18)
  )
  
  # Fixed effects
  result_fixed <- cross_study_meta_analysis(
    studies = studies,
    effect_metric = "shannon",
    model = "fixed"
  )
  
  # Random effects
  result_random <- cross_study_meta_analysis(
    studies = studies,
    effect_metric = "shannon",
    model = "random"
  )
  
  # Both should work
  expect_s3_class(result_fixed, "meta_analysis")
  expect_s3_class(result_random, "meta_analysis")
  
  # Random effects should have tau-squared
  expect_true("tau2" %in% names(result_random$heterogeneity))
})

test_that("cross_study_meta_analysis sensitivity analysis", {
  studies <- list(
    study1 = create_demo_phyloseq(n_samples = 10, n_taxa = 20),
    study2 = create_demo_phyloseq(n_samples = 15, n_taxa = 25),
    study3 = create_demo_phyloseq(n_samples = 8, n_taxa = 18),
    study4 = create_demo_phyloseq(n_samples = 12, n_taxa = 22)
  )
  
  result <- cross_study_meta_analysis(
    studies = studies,
    effect_metric = "shannon",
    sensitivity_analysis = TRUE
  )
  
  expect_true("sensitivity" %in% names(result))
  
  if (!is.null(result$sensitivity)) {
    # Should have leave-one-out results
    expect_length(result$sensitivity$leave_one_out, length(studies))
  }
})

test_that("cross_study_meta_analysis handles errors", {
  # Empty studies
  expect_error(cross_study_meta_analysis(list()))
  
  # Single study
  expect_error(cross_study_meta_analysis(
    list(study1 = create_demo_phyloseq())
  ))
  
  # Invalid metric
  studies <- list(
    study1 = create_demo_phyloseq(),
    study2 = create_demo_phyloseq()
  )
  
  expect_error(cross_study_meta_analysis(
    studies = studies,
    effect_metric = "invalid_metric"
  ))
})