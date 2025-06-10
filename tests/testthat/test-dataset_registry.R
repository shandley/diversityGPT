test_that("dataset registry is properly structured", {
  # Check registry exists
  expect_true(exists("diversityGPT_datasets"))
  
  # Check main categories
  expect_named(diversityGPT_datasets, c("builtin", "precomputed", "external"))
  
  # Check at least one dataset in each category
  expect_true(length(diversityGPT_datasets$builtin) > 0)
  expect_true(length(diversityGPT_datasets$precomputed) > 0)
  expect_true(length(diversityGPT_datasets$external) > 0)
})

test_that("get_dataset_info works correctly", {
  # Test with known dataset
  info <- get_dataset_info("globalpatterns")
  expect_type(info, "list")
  expect_equal(info$id, "globalpatterns")
  expect_equal(info$type, "16S")
  expect_true(info$has_tree)
  
  # Test with specific source
  info2 <- get_dataset_info("globalpatterns", source = "builtin")
  expect_equal(info$id, info2$id)
  
  # Test with non-existent dataset
  expect_null(get_dataset_info("nonexistent"))
  
  # Test case insensitive
  info3 <- get_dataset_info("GLOBALPATTERNS")
  expect_equal(info3$id, "globalpatterns")
})

test_that("list_available_datasets works correctly", {
  # List all datasets
  all_datasets <- list_available_datasets()
  expect_s3_class(all_datasets, "data.frame")
  expect_true(nrow(all_datasets) > 0)
  
  # Expected columns
  expected_cols <- c("id", "name", "type", "source", "samples", "taxa", 
                     "has_tree", "description")
  expect_true(all(expected_cols %in% colnames(all_datasets)))
  
  # Filter by type
  amplicon_only <- list_available_datasets(type = "16S")
  expect_true(all(amplicon_only$type == "16S"))
  
  # Filter by source
  builtin_only <- list_available_datasets(source = "builtin")
  expect_true(all(builtin_only$source == "builtin"))
  
  # Multiple filters
  filtered <- list_available_datasets(type = "16S", source = "builtin")
  expect_true(nrow(filtered) <= nrow(amplicon_only))
  expect_true(nrow(filtered) <= nrow(builtin_only))
})

test_that("search_datasets finds relevant results", {
  # Search for gut datasets
  gut_results <- search_datasets("gut")
  expect_s3_class(gut_results, "data.frame")
  
  # Should find enterotype dataset
  expect_true(any(gut_results$id == "enterotype"))
  
  # Search in specific fields
  name_results <- search_datasets("Global", fields = "name")
  expect_true(any(grepl("Global", name_results$name, ignore.case = TRUE)))
  
  # Empty search returns empty data frame
  no_results <- search_datasets("xyz123nonexistent")
  expect_s3_class(no_results, "data.frame")
  expect_equal(nrow(no_results), 0)
})

test_that("dataset entries have required fields", {
  # Check a few datasets for required fields
  datasets_to_check <- c("globalpatterns", "enterotype", "soilrep")
  
  for (dataset_id in datasets_to_check) {
    info <- get_dataset_info(dataset_id)
    
    # Required fields
    expect_true("id" %in% names(info))
    expect_true("name" %in% names(info))
    expect_true("type" %in% names(info))
    expect_true("description" %in% names(info))
    expect_true("source" %in% names(info))
    
    # Field types
    expect_type(info$id, "character")
    expect_type(info$name, "character")
    expect_type(info$type, "character")
    expect_type(info$description, "character")
    
    # Valid values
    expect_true(info$type %in% c("16S", "shotgun"))
  }
})

test_that("registry tag filtering works", {
  # Get datasets with specific tags
  datasets <- list_available_datasets()
  
  # Manual tag filtering (since it's not in the main function)
  soil_datasets <- list()
  for (i in 1:nrow(datasets)) {
    info <- get_dataset_info(datasets$id[i])
    if (!is.null(info$tags) && any(grepl("soil", info$tags, ignore.case = TRUE))) {
      soil_datasets[[length(soil_datasets) + 1]] <- info
    }
  }
  
  # Should find at least soilrep
  expect_true(length(soil_datasets) > 0)
  expect_true(any(sapply(soil_datasets, function(x) x$id == "soilrep")))
})