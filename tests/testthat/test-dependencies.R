test_that("check_dependencies works correctly", {
  # Test silent mode
  result <- check_dependencies(verbose = FALSE, check_suggested = FALSE)
  
  expect_type(result, "list")
  expect_true("required" %in% names(result))
  expect_true("critical" %in% names(result))
  
  # Check that core packages are detected as installed
  expect_true(result$required$phyloseq)
  expect_true(result$required$vegan)
})

test_that("check_function_dependencies provides appropriate warnings", {
  # Mock picante not being available
  with_mocked_bindings(
    requireNamespace = function(pkg, ...) {
      if (pkg == "picante") return(FALSE)
      return(TRUE)
    },
    {
      # Should return FALSE and show warning
      expect_false(check_function_dependencies("faith_pd"))
      expect_false(check_function_dependencies("unifrac"))
    }
  )
})

test_that("calculate_diversity handles missing picante gracefully", {
  skip_if_not_installed("phyloseq")
  
  # Create test data
  data(GlobalPatterns, package = "phyloseq")
  ps <- phyloseq::prune_samples(sample_names(GlobalPatterns)[1:5], GlobalPatterns)
  
  # Mock picante not being available
  with_mocked_bindings(
    requireNamespace = function(pkg, ...) {
      if (pkg == "picante") return(FALSE)
      return(TRUE)
    },
    check_function_dependencies = function(...) TRUE,  # Suppress warnings in test
    {
      # Calculate diversity with faith_pd
      result <- calculate_diversity(ps, metrics = c("shannon", "faith_pd"))
      
      # Should have shannon values but NA for faith_pd
      expect_false(any(is.na(result$shannon)))
      expect_true(all(is.na(result$faith_pd)))
    }
  )
})

test_that("setup_diversitygpt doesn't fail", {
  # Just test that it runs without error
  # Don't actually install packages in tests
  with_mocked_bindings(
    install.packages = function(...) NULL,
    interactive = function() FALSE,
    {
      expect_silent(result <- setup_diversitygpt())
      expect_type(result, "logical")
    }
  )
})

test_that(".onAttach message appears for missing picante", {
  # Test the startup message
  with_mocked_bindings(
    requireNamespace = function(pkg, ...) {
      if (pkg == "picante") return(FALSE)
      return(TRUE)
    },
    {
      expect_message(
        .onAttach("lib", "diversityGPT"),
        "picante"
      )
    }
  )
})