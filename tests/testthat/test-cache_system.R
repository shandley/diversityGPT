test_that("cache system initializes correctly", {
  # Use temporary directory for testing
  temp_cache <- tempfile("test_cache")
  
  # Initialize cache
  init_cache_system(cache_dir = temp_cache)
  
  # Check initialization
  expect_true(cache_is_initialized())
  expect_true(dir.exists(temp_cache))
  
  # Check cache environment exists
  expect_true(exists(".diversityGPT_cache", envir = .GlobalEnv))
  
  # Clean up
  cache_clear()
  unlink(temp_cache, recursive = TRUE)
})

test_that("cache store and retrieve works", {
  # Initialize test cache
  temp_cache <- tempfile("test_cache")
  init_cache_system(cache_dir = temp_cache)
  
  # Store simple object
  test_data <- list(a = 1:10, b = "test", c = matrix(1:9, 3, 3))
  cache_store("test_key", test_data)
  
  # Check exists
  expect_true(cache_exists("test_key"))
  
  # Retrieve
  retrieved <- cache_get("test_key")
  expect_identical(retrieved, test_data)
  
  # Non-existent key
  expect_null(cache_get("nonexistent_key"))
  expect_false(cache_exists("nonexistent_key"))
  
  # Clean up
  cache_clear()
  unlink(temp_cache, recursive = TRUE)
})

test_that("phyloseq hashing is consistent", {
  # Load test data
  data(GlobalPatterns, package = "phyloseq")
  
  # Generate hash multiple times
  hash1 <- generate_phyloseq_hash(GlobalPatterns)
  hash2 <- generate_phyloseq_hash(GlobalPatterns)
  
  expect_identical(hash1, hash2)
  expect_type(hash1, "character")
  expect_true(nchar(hash1) > 0)
  
  # Different objects should have different hashes
  gp_subset <- phyloseq::prune_taxa(
    phyloseq::taxa_names(GlobalPatterns)[1:100], 
    GlobalPatterns
  )
  hash3 <- generate_phyloseq_hash(gp_subset)
  expect_false(hash1 == hash3)
})

test_that("cache_with_key works correctly", {
  temp_cache <- tempfile("test_cache")
  init_cache_system(cache_dir = temp_cache)
  
  # Counter to track computation
  compute_count <- 0
  
  # First call - computes
  result1 <- cache_with_key("test_computation", {
    compute_count <- compute_count + 1
    expensive_result <- sum(1:1000000)
    expensive_result
  })
  
  expect_equal(compute_count, 1)
  expect_equal(result1, sum(1:1000000))
  
  # Second call - retrieves from cache
  result2 <- cache_with_key("test_computation", {
    compute_count <- compute_count + 1
    sum(1:1000000)
  })
  
  expect_equal(compute_count, 1)  # Not incremented
  expect_identical(result1, result2)
  
  # Clean up
  cache_clear()
  unlink(temp_cache, recursive = TRUE)
})

test_that("cache_remove works", {
  temp_cache <- tempfile("test_cache")
  init_cache_system(cache_dir = temp_cache)
  
  # Store multiple items
  cache_store("key1", 1:10)
  cache_store("key2", letters[1:5])
  cache_store("key3", list(a = 1, b = 2))
  
  expect_true(cache_exists("key1"))
  expect_true(cache_exists("key2"))
  expect_true(cache_exists("key3"))
  
  # Remove one
  cache_remove("key2")
  
  expect_true(cache_exists("key1"))
  expect_false(cache_exists("key2"))
  expect_true(cache_exists("key3"))
  
  # Clean up
  cache_clear()
  unlink(temp_cache, recursive = TRUE)
})

test_that("cache_stats provides accurate information", {
  temp_cache <- tempfile("test_cache")
  init_cache_system(cache_dir = temp_cache)
  
  # Empty cache
  stats1 <- cache_stats()
  expect_equal(stats1$n_entries, 0)
  expect_equal(stats1$total_size, 0)
  
  # Add items
  cache_store("small", 1:10)
  cache_store("medium", matrix(1:10000, 100, 100))
  cache_store("large", list(data = rnorm(100000)))
  
  stats2 <- cache_stats()
  expect_equal(stats2$n_entries, 3)
  expect_gt(stats2$total_size, 0)
  expect_type(stats2$entries, "character")
  expect_length(stats2$entries, 3)
  
  # Clean up
  cache_clear()
  unlink(temp_cache, recursive = TRUE)
})

test_that("cache_cleanup removes old entries", {
  temp_cache <- tempfile("test_cache")
  init_cache_system(cache_dir = temp_cache)
  
  # Store items
  cache_store("item1", 1:100)
  cache_store("item2", 1:200)
  
  # Cleanup with very recent cutoff (should remove all)
  cache_cleanup(max_age_days = 0)
  
  stats <- cache_stats()
  expect_equal(stats$n_entries, 0)
  
  # Clean up
  unlink(temp_cache, recursive = TRUE)
})

test_that("cached analysis functions work", {
  temp_cache <- tempfile("test_cache")
  init_cache_system(cache_dir = temp_cache)
  
  # Load test data
  data(GlobalPatterns, package = "phyloseq")
  gp_small <- phyloseq::prune_taxa(
    phyloseq::taxa_names(GlobalPatterns)[1:50], 
    GlobalPatterns
  )
  
  # First call - computes
  start_time1 <- Sys.time()
  result1 <- cached_extract_universal_information(gp_small)
  time1 <- as.numeric(Sys.time() - start_time1)
  
  expect_s3_class(result1, "universal_information")
  
  # Second call - from cache (should be faster)
  start_time2 <- Sys.time()
  result2 <- cached_extract_universal_information(gp_small)
  time2 <- as.numeric(Sys.time() - start_time2)
  
  expect_identical(result1$transformation_matrix, result2$transformation_matrix)
  # Cache should be faster (but might not be in test environment)
  # expect_lt(time2, time1)
  
  # Clean up
  cache_clear()
  unlink(temp_cache, recursive = TRUE)
})

test_that("cache handles errors gracefully", {
  temp_cache <- tempfile("test_cache")
  init_cache_system(cache_dir = temp_cache)
  
  # Try to cache an error
  expect_error({
    cache_with_key("error_test", {
      stop("Intentional error")
    })
  }, "Intentional error")
  
  # Key should not exist
  expect_false(cache_exists("error_test"))
  
  # Clean up
  cache_clear()
  unlink(temp_cache, recursive = TRUE)
})

test_that("generate_universal_cache_key creates unique keys", {
  data(GlobalPatterns, package = "phyloseq")
  
  # Same data, same parameters
  key1 <- generate_universal_cache_key(GlobalPatterns, groups = "SampleType", 
                                      include_phylogenetic = TRUE)
  key2 <- generate_universal_cache_key(GlobalPatterns, groups = "SampleType", 
                                      include_phylogenetic = TRUE)
  expect_identical(key1, key2)
  
  # Different groups
  key3 <- generate_universal_cache_key(GlobalPatterns, groups = "Primer", 
                                      include_phylogenetic = TRUE)
  expect_false(key1 == key3)
  
  # Different phylogenetic setting
  key4 <- generate_universal_cache_key(GlobalPatterns, groups = "SampleType", 
                                      include_phylogenetic = FALSE)
  expect_false(key1 == key4)
})