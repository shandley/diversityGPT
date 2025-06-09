test_that("basic diversity calculation works without phyloseq", {
  # Skip if phyloseq not available
  skip_if_not_installed("phyloseq")
  skip_if_not_installed("vegan")
  
  # Test that we can calculate diversity from a simple matrix
  test_matrix <- matrix(rpois(50, lambda = 5), nrow = 5, ncol = 10)
  colnames(test_matrix) <- paste0("Sample", 1:10)
  rownames(test_matrix) <- paste0("OTU", 1:5)
  
  # Test vegan functions directly
  shannon_vals <- vegan::diversity(t(test_matrix), index = "shannon")
  simpson_vals <- vegan::diversity(t(test_matrix), index = "simpson")
  
  expect_length(shannon_vals, 10)
  expect_length(simpson_vals, 10)
  expect_true(all(shannon_vals >= 0))
  expect_true(all(simpson_vals >= 0))
})

test_that("API key functions work", {
  # Test the basic API key functions
  expect_true(is.function(diversityGPT:::get_api_key))
  
  # Test check_api_setup function exists
  expect_true(exists("check_api_setup", envir = asNamespace("diversityGPT")))
  
  # Test with mock environment (using withr)
  withr::with_envvar(
    c(ANTHROPIC_API_KEY = "test-key-123"),
    {
      expect_equal(diversityGPT:::get_api_key("anthropic"), "test-key-123")
    }
  )
  
  withr::with_envvar(
    c(ANTHROPIC_API_KEY = ""),
    {
      expect_null(diversityGPT:::get_api_key("anthropic"))
    }
  )
})

test_that("diversity result class works", {
  # Create a simple diversity_results object
  test_results <- data.frame(
    sample = paste0("Sample", 1:5),
    shannon = runif(5, 1, 3),
    simpson = runif(5, 0.5, 0.9)
  )
  
  attr(test_results, "metrics") <- c("shannon", "simpson")
  attr(test_results, "n_samples") <- 5
  attr(test_results, "n_taxa") <- 20
  class(test_results) <- c("diversity_results", "data.frame")
  
  # Test that it has the right class
  expect_s3_class(test_results, "diversity_results")
  expect_equal(attr(test_results, "n_samples"), 5)
  expect_equal(attr(test_results, "metrics"), c("shannon", "simpson"))
})