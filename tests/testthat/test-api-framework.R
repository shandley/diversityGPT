# Test suite for diversityGPT API Framework

library(testthat)
library(diversityGPT)

# Test API configuration
test_that("API configuration works correctly", {
  # Test default configuration
  config <- create_api_config()
  
  expect_s3_class(config, "api_config")
  expect_true(config$cache$enabled)
  expect_true(config$parallel$enabled)
  expect_equal(config$llm$provider, "none")
  
  # Test custom configuration
  custom_config <- create_api_config(
    cache = FALSE,
    parallel = FALSE,
    llm_provider = "openai",
    api_key = "test-key",
    memory_limit = 8192
  )
  
  expect_false(custom_config$cache$enabled)
  expect_false(custom_config$parallel$enabled)
  expect_equal(custom_config$llm$provider, "openai")
  expect_equal(custom_config$llm$api_key, "test-key")
  expect_equal(custom_config$resources$memory_limit, 8192)
})

test_that("API response creation works", {
  # Test success response
  response <- .create_api_response(
    status = "success",
    data = list(result = "test"),
    metadata = list(operation = "test_op")
  )
  
  expect_s3_class(response, "api_response")
  expect_equal(response$status, "success")
  expect_equal(response$data$result, "test")
  expect_equal(response$metadata$operation, "test_op")
  
  # Test error response
  error_response <- .create_error_response(
    "Test error",
    "TEST_ERROR",
    list(detail = "Additional info")
  )
  
  expect_s3_class(error_response, "api_response")
  expect_equal(error_response$status, "error")
  expect_equal(error_response$error$message, "Test error")
  expect_equal(error_response$error$code, "TEST_ERROR")
})

test_that("API analyze operation works", {
  skip_if_not_installed("phyloseq")
  data(GlobalPatterns)
  
  # Create small test dataset
  test_data <- create_demo_subset(GlobalPatterns, n_samples = 5, n_taxa = 20)
  
  # Test successful analysis
  result <- diversityGPT_api(
    "analyze",
    phyloseq_obj = test_data,
    components = "universal",
    verbose = FALSE
  )
  
  expect_s3_class(result, "api_response")
  expect_equal(result$status, "success")
  expect_true("universal" %in% names(result$data))
  expect_s3_class(result$data$universal, "universal_information")
})

test_that("API transform operation works", {
  # Create test metrics
  source_metrics <- data.frame(
    shannon = c(2.1, 2.5, 1.8),
    row.names = c("S1", "S2", "S3")
  )
  
  # Create reference data for transformation matrix
  data(GlobalPatterns)
  test_data <- create_demo_subset(GlobalPatterns, n_samples = 10, n_taxa = 30)
  
  # Test transformation
  result <- diversityGPT_api(
    "transform",
    source_metrics = source_metrics,
    target_metrics = c("simpson"),
    phyloseq_reference = test_data,
    verbose = FALSE
  )
  
  expect_s3_class(result, "api_response")
  expect_equal(result$status, "success")
  expect_true("predictions" %in% names(result$data))
  expect_true("simpson" %in% names(result$data$predictions))
})

test_that("API validation operation works", {
  skip_if_not_installed("phyloseq")
  data(GlobalPatterns)
  
  test_data <- create_demo_subset(GlobalPatterns, n_samples = 5, n_taxa = 20)
  universal_info <- extract_universal_information(test_data)
  
  # Test validation
  result <- diversityGPT_api(
    "validate",
    universal_info = universal_info,
    phyloseq_obj = test_data,
    validation_type = "quick",
    verbose = FALSE
  )
  
  expect_s3_class(result, "api_response")
  expect_equal(result$status, "success")
  expect_true("overall_assessment" %in% names(result$data))
})

test_that("API error handling works", {
  # Test invalid operation
  result <- diversityGPT_api(
    "invalid_operation",
    verbose = FALSE
  )
  
  expect_s3_class(result, "api_response")
  expect_equal(result$status, "error")
  expect_equal(result$error$code, "INVALID_OPERATION")
  
  # Test missing required parameter
  result <- diversityGPT_api(
    "analyze",
    # Missing phyloseq_obj
    verbose = FALSE
  )
  
  expect_equal(result$status, "error")
  
  # Test invalid input type
  result <- diversityGPT_api(
    "analyze",
    phyloseq_obj = "not a phyloseq object",
    verbose = FALSE
  )
  
  expect_equal(result$status, "error")
  expect_equal(result$error$code, "INVALID_INPUT")
})

test_that("API helper functions work", {
  # Test is_api_success
  success_response <- .create_api_response(status = "success")
  error_response <- .create_error_response("Test", "TEST")
  
  expect_true(is_api_success(success_response))
  expect_false(is_api_success(error_response))
  
  # Test get_api_data
  data_response <- .create_api_response(
    status = "success",
    data = list(a = 1, b = 2)
  )
  
  expect_equal(get_api_data(data_response), list(a = 1, b = 2))
  expect_equal(get_api_data(data_response, "a"), 1)
  expect_error(get_api_data(data_response, "c"))
  
  # Test get_api_error
  expect_null(get_api_error(success_response))
  expect_equal(get_api_error(error_response)$message, "Test")
  expect_equal(get_api_error(error_response)$code, "TEST")
})

test_that("API batch processing works", {
  skip_if_not_installed("phyloseq")
  data(GlobalPatterns)
  
  # Create test datasets
  dataset1 <- create_demo_subset(GlobalPatterns, n_samples = 5, n_taxa = 20, seed = 1)
  dataset2 <- create_demo_subset(GlobalPatterns, n_samples = 5, n_taxa = 20, seed = 2)
  
  # Test batch analysis
  result <- diversityGPT_api(
    "batch_analyze",
    dataset_list = list(dataset1, dataset2),
    dataset_names = c("Test1", "Test2"),
    analysis_steps = "universal",
    verbose = FALSE
  )
  
  expect_s3_class(result, "api_response")
  expect_equal(result$status, "success")
  expect_s3_class(result$data, "batch_processing")
})

test_that("API meta-analysis works", {
  skip_if_not_installed("phyloseq")
  data(GlobalPatterns)
  
  # Create test studies
  study1 <- create_demo_subset(GlobalPatterns, n_samples = 10, n_taxa = 30, seed = 1)
  study2 <- create_demo_subset(GlobalPatterns, n_samples = 10, n_taxa = 30, seed = 2)
  
  # Test meta-analysis
  result <- diversityGPT_api(
    "meta_analyze",
    study_list = list(study1, study2),
    study_names = c("Study1", "Study2"),
    meta_method = "fixed_effects",
    verbose = FALSE
  )
  
  expect_s3_class(result, "api_response")
  expect_equal(result$status, "success")
  expect_s3_class(result$data, "meta_analysis")
})

test_that("API report generation works", {
  skip_if_not_installed("phyloseq")
  skip_if_not_installed("rmarkdown")
  
  data(GlobalPatterns)
  test_data <- create_demo_subset(GlobalPatterns, n_samples = 5, n_taxa = 20)
  
  # Run analysis first
  analysis_result <- diversityGPT_api(
    "analyze",
    phyloseq_obj = test_data,
    components = "universal",
    verbose = FALSE
  )
  
  # Generate report
  result <- diversityGPT_api(
    "generate_report",
    analysis_results = analysis_result$data,
    phyloseq_obj = test_data,
    output_format = "html",
    template = "summary",
    verbose = FALSE
  )
  
  expect_s3_class(result, "api_response")
  
  # Clean up
  if (result$status == "success" && !is.null(result$data$report_path)) {
    unlink(result$data$report_path)
  }
})

test_that("API async operations work", {
  skip("Async operations require callr package and background processing")
  
  # This test would require callr package and proper async setup
  # Placeholder for async testing
})

test_that("API configuration merging works", {
  default_config <- create_api_config()
  
  # Test with NULL config
  merged <- .merge_api_config(NULL)
  expect_equal(merged$cache$enabled, default_config$cache$enabled)
  
  # Test with partial config
  partial_config <- list(
    cache = list(enabled = FALSE),
    llm = list(provider = "anthropic")
  )
  
  merged <- .merge_api_config(partial_config)
  expect_false(merged$cache$enabled)
  expect_equal(merged$llm$provider, "anthropic")
  expect_equal(merged$parallel$enabled, default_config$parallel$enabled)
})

test_that("API print methods work", {
  # Test api_response print
  response <- .create_api_response(
    status = "success",
    data = list(test = "data"),
    metadata = list(operation = "test")
  )
  
  expect_output(print(response), "diversityGPT API Response")
  expect_output(print(response), "Status: success")
  
  # Test error response print
  error_response <- .create_error_response("Test error", "TEST_CODE")
  expect_output(print(error_response), "Error Code: TEST_CODE")
  expect_output(print(error_response), "Error Message: Test error")
})

test_that("API literature search works", {
  # Mock universal info
  mock_universal <- list(
    information_components = data.frame(
      R_component = runif(5),
      E_component = runif(5)
    )
  )
  class(mock_universal) <- c("universal_information", "list")
  
  # Test with query
  result <- diversityGPT_api(
    "search_literature",
    query = "microbiome diversity",
    databases = "biorxiv",
    max_papers = 5,
    verbose = FALSE
  )
  
  expect_s3_class(result, "api_response")
  # Note: Actual success depends on external API availability
})

test_that("API caching functionality works", {
  skip_if_not_installed("phyloseq")
  data(GlobalPatterns)
  
  test_data <- create_demo_subset(GlobalPatterns, n_samples = 5, n_taxa = 20)
  
  # Create config with caching enabled
  cache_dir <- tempdir()
  config <- create_api_config(
    cache = TRUE,
    cache_dir = cache_dir
  )
  
  # First run - should create cache
  result1 <- diversityGPT_api(
    "analyze",
    phyloseq_obj = test_data,
    components = "universal",
    config = config,
    verbose = FALSE
  )
  
  expect_equal(result1$status, "success")
  
  # Check cache files exist
  cache_files <- list.files(cache_dir, pattern = "diversityGPT_cache", 
                           recursive = TRUE)
  expect_true(length(cache_files) > 0)
  
  # Clean cache
  clean_result <- diversityGPT_api(
    "clean_cache",
    age_days = 0,  # Remove all
    config = config,
    verbose = FALSE
  )
  
  expect_equal(clean_result$status, "success")
})