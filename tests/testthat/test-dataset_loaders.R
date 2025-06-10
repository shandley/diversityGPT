test_that("load_phyloseq_builtin works correctly", {
  # Test loading GlobalPatterns
  result <- load_phyloseq_builtin(list(data_name = "GlobalPatterns"))
  expect_s3_class(result, "phyloseq")
  expect_equal(phyloseq::nsamples(result), 26)
  expect_equal(phyloseq::ntaxa(result), 19216)
  
  # Test loading enterotype
  result2 <- load_phyloseq_builtin(list(data_name = "enterotype"))
  expect_s3_class(result2, "phyloseq")
  expect_equal(phyloseq::nsamples(result2), 280)
  
  # Test error on missing dataset
  expect_error(
    load_phyloseq_builtin(list(data_name = "nonexistent")),
    "Failed to load"
  )
})

test_that("load_dataset works with different sources", {
  # Test built-in dataset
  gp <- load_dataset("globalpatterns")
  expect_s3_class(gp, "phyloseq")
  expect_equal(phyloseq::nsamples(gp), 26)
  
  # Test with return_info
  result <- load_dataset("enterotype", return_info = TRUE)
  expect_type(result, "list")
  expect_s3_class(result$phyloseq, "phyloseq")
  expect_type(result$info, "list")
  expect_equal(result$info$id, "enterotype")
  
  # Test non-existent dataset
  expect_error(
    load_dataset("nonexistent_dataset"),
    "Dataset.*not found"
  )
})

test_that("load_dataset_with_progress provides progress updates", {
  # This is hard to test directly, but we can check it runs
  skip_if_not(interactive(), "Progress bars only in interactive mode")
  
  # Should complete without error
  result <- load_dataset_with_progress("globalpatterns")
  expect_s3_class(result, "phyloseq")
})

test_that("create_demo_subset works correctly", {
  # Load full dataset
  data(GlobalPatterns, package = "phyloseq")
  
  # Create subset
  demo <- create_demo_subset(GlobalPatterns, max_taxa = 100, max_samples = 10)
  
  expect_s3_class(demo, "phyloseq")
  expect_lte(phyloseq::ntaxa(demo), 100)
  expect_lte(phyloseq::nsamples(demo), 10)
  
  # Check seed reproducibility
  demo2 <- create_demo_subset(GlobalPatterns, max_taxa = 100, max_samples = 10)
  expect_equal(phyloseq::taxa_names(demo), phyloseq::taxa_names(demo2))
})

test_that("load_precomputed handles missing files gracefully", {
  # Test with fake dataset info
  fake_info <- list(
    id = "fake_dataset",
    file = "nonexistent_file.rds"
  )
  
  expect_error(
    load_precomputed(fake_info),
    "Precomputed file not found"
  )
})

test_that("download_dataset placeholder works", {
  # Currently returns FALSE
  expect_false(download_dataset("any_dataset"))
  
  # Test with URL
  expect_false(download_dataset("test", url = "http://example.com/data.zip"))
})

test_that("loader functions handle phyloseq components correctly", {
  # Test that loaded datasets have expected components
  gp <- load_dataset("globalpatterns")
  
  # Check components
  expect_true(!is.null(phyloseq::otu_table(gp)))
  expect_true(!is.null(phyloseq::sample_data(gp)))
  expect_true(!is.null(phyloseq::tax_table(gp)))
  expect_true(!is.null(phyloseq::phy_tree(gp)))
  
  # Load dataset without tree
  et <- load_dataset("enterotype")
  expect_true(!is.null(phyloseq::otu_table(et)))
  expect_true(!is.null(phyloseq::sample_data(et)))
  expect_null(phyloseq::phy_tree(et, errorIfNULL = FALSE))
})