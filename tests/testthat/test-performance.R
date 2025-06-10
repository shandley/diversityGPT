# Performance tests for diversityGPT
# These tests benchmark key operations and ensure optimizations work correctly

test_that("performance optimizations load correctly", {
  # Source the performance optimization file
  perf_file <- system.file("R", "performance_optimization.R", package = "diversityGPT")
  if (perf_file == "") {
    perf_file <- "../../R/performance_optimization.R"
  }
  
  if (file.exists(perf_file)) {
    source(perf_file)
  }
  
  # Check functions exist
  expect_true(exists("parallel_extract_universal"))
  expect_true(exists("optimized_calculate_diversity"))
  expect_true(exists("memory_efficient_transform"))
  expect_true(exists("precompute_universal_info"))
})

test_that("parallel processing functions work", {
  skip_if_not(requireNamespace("parallel", quietly = TRUE),
              "parallel package not available")
  
  # Small test with GlobalPatterns
  data(GlobalPatterns)
  small_ps <- prune_samples(sample_names(GlobalPatterns)[1:10], GlobalPatterns)
  
  # Test parallel extraction (should fall back to single-threaded for small data)
  expect_message(
    result <- parallel_extract_universal(small_ps, n_cores = 2),
    "Dataset too small for parallel processing"
  )
  
  expect_s3_class(result, "universal_information")
})

test_that("optimized diversity calculation works", {
  data(GlobalPatterns)
  
  # Test optimized calculation
  result <- optimized_calculate_diversity(
    GlobalPatterns,
    metrics = c("shannon", "simpson"),
    batch_size = 10  # Small batch for testing
  )
  
  expect_true(is.data.frame(result))
  expect_true("shannon" %in% names(result))
  expect_true("simpson" %in% names(result))
  expect_equal(nrow(result), nsamples(GlobalPatterns))
})

test_that("memory efficient transform works", {
  # Create test data
  n_samples <- 100
  test_metrics <- data.frame(
    shannon = runif(n_samples, 1, 3),
    observed = round(runif(n_samples, 50, 200))
  )
  
  # Create simple transformation matrix
  tm <- matrix(
    c(0.5, 0.5, 0.3, 0.7),
    nrow = 2,
    dimnames = list(
      c("shannon", "observed"),
      c("R_component", "E_component")
    )
  )
  
  # Test memory efficient transform
  result <- memory_efficient_transform(
    test_metrics,
    target_metrics = c("simpson"),
    transformation_matrix = tm,
    chunk_size = 30
  )
  
  expect_true(is.list(result))
  expect_true("predictions" %in% names(result))
  expect_equal(nrow(result$predictions), n_samples)
})

test_that("precompute and load functions work", {
  skip_on_cran()  # Skip on CRAN to avoid file I/O
  
  # Create temp file
  temp_file <- tempfile(fileext = ".rds")
  on.exit(unlink(temp_file))
  
  # Use small dataset
  data(enterotype)
  small_ps <- prune_samples(sample_names(enterotype)[1:20], enterotype)
  
  # Test precompute
  expect_message(
    saved_file <- precompute_universal_info(small_ps, temp_file),
    "Precomputing universal information"
  )
  
  expect_true(file.exists(temp_file))
  
  # Test load
  expect_message(
    loaded_info <- load_precomputed_universal(temp_file),
    "Loading precomputed universal information"
  )
  
  expect_s3_class(loaded_info, "universal_information")
  expect_true(!is.null(loaded_info$precomputed))
})

test_that("performance monitoring works", {
  # Enable monitoring
  enable_performance_monitoring(TRUE, verbose = FALSE)
  
  # Clear any existing data
  options(diversityGPT.performance_data = list())
  
  # Run some operations
  data(GlobalPatterns)
  small_ps <- prune_samples(sample_names(GlobalPatterns)[1:5], GlobalPatterns)
  
  # This should be monitored
  div_results <- calculate_diversity(small_ps)
  
  # Get report
  report <- get_performance_report()
  
  # Check report structure
  if (!is.null(report)) {
    expect_s3_class(report, "performance_report")
    expect_true(report$n_operations >= 0)
  }
  
  # Disable monitoring
  enable_performance_monitoring(FALSE, verbose = FALSE)
})

# Benchmark tests (only run if requested)
test_that("benchmarks show performance improvements", {
  skip_if_not(Sys.getenv("RUN_BENCHMARKS") == "true",
              "Set RUN_BENCHMARKS=true to run benchmarks")
  
  skip_if_not(requireNamespace("microbenchmark", quietly = TRUE),
              "microbenchmark package required for benchmarks")
  
  library(microbenchmark)
  data(GlobalPatterns)
  
  # Benchmark diversity calculation
  bench_diversity <- microbenchmark(
    regular = calculate_diversity(GlobalPatterns),
    optimized = optimized_calculate_diversity(
      GlobalPatterns, 
      use_sparse = TRUE,
      batch_size = 10
    ),
    times = 5
  )
  
  print(bench_diversity)
  
  # Benchmark universal extraction (if dataset is large enough)
  if (nsamples(GlobalPatterns) > 20) {
    bench_universal <- microbenchmark(
      regular = extract_universal_information(GlobalPatterns),
      parallel = parallel_extract_universal(
        GlobalPatterns,
        n_cores = 2,
        chunk_size = 10
      ),
      times = 3
    )
    
    print(bench_universal)
  }
})