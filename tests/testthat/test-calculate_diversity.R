test_that("calculate_diversity works with basic metrics", {
  # Create mock data
  mock_physeq <- create_mock_phyloseq(n_samples = 5, n_taxa = 20)
  
  # Mock the phyloseq functions
  mockery::stub(calculate_diversity, "phyloseq::sample_names", 
                paste0("Sample", 1:5))
  mockery::stub(calculate_diversity, "phyloseq::nsamples", 5)
  mockery::stub(calculate_diversity, "phyloseq::ntaxa", 20)
  mockery::stub(calculate_diversity, "phyloseq::otu_table", 
                mock_physeq$otu_table)
  mockery::stub(calculate_diversity, "inherits", TRUE)
  
  # Test basic calculation
  result <- calculate_diversity(
    mock_physeq,
    metrics = c("shannon", "simpson")
  )
  
  # Check structure
  expect_s3_class(result, "diversity_results")
  expect_equal(nrow(result), 5)
  expect_true("shannon" %in% names(result))
  expect_true("simpson" %in% names(result))
  
  # Check attributes
  expect_equal(attr(result, "n_samples"), 5)
  expect_equal(attr(result, "n_taxa"), 20)
})

test_that("calculate_diversity handles invalid metrics gracefully", {
  mock_physeq <- create_mock_phyloseq(n_samples = 3, n_taxa = 10)
  
  mockery::stub(calculate_diversity, "phyloseq::sample_names", 
                paste0("Sample", 1:3))
  mockery::stub(calculate_diversity, "inherits", TRUE)
  
  # Expect warning for invalid metric
  expect_warning(
    result <- calculate_diversity(
      mock_physeq,
      metrics = c("shannon", "invalid_metric")
    ),
    "Unknown metrics"
  )
})

test_that("calculate_diversity validates input", {
  # Test with non-phyloseq object
  expect_error(
    calculate_diversity(list()),
    "must be a phyloseq object"
  )
})

test_that("print method works correctly", {
  # Create mock diversity results
  mock_results <- structure(
    data.frame(
      sample = paste0("Sample", 1:10),
      shannon = runif(10, 1, 3),
      simpson = runif(10, 0.5, 0.9)
    ),
    metrics = c("shannon", "simpson"),
    n_samples = 10,
    n_taxa = 50,
    class = c("diversity_results", "data.frame")
  )
  
  # Capture print output
  output <- capture.output(print(mock_results))
  
  # Check output contains expected elements
  expect_true(any(grepl("Diversity Analysis Results", output)))
  expect_true(any(grepl("Samples: 10", output)))
  expect_true(any(grepl("Taxa: 50", output)))
  expect_true(any(grepl("shannon, simpson", output)))
})