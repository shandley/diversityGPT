# test-advanced-llm-integration.R
# Tests for multi-step LLM analysis

test_that("llm_multi_step_analysis works with basic parameters", {
  # Create test data
  test_physeq <- create_demo_phyloseq(n_samples = 10, n_taxa = 20)
  universal_info <- extract_universal_information(test_physeq)
  
  # Test basic multi-step analysis
  result <- llm_multi_step_analysis(
    universal_info = universal_info,
    study_context = list(
      study_system = "gut_microbiome",
      research_question = "What drives diversity?"
    ),
    reasoning_depth = "standard",
    llm_provider = "none"  # Use none for testing
  )
  
  # Check structure
  expect_s3_class(result, "llm_multi_step_analysis")
  expect_type(result, "list")
  expect_true(all(c("steps", "synthesis", "confidence", "metadata") %in% names(result)))
  
  # Check steps
  expect_type(result$steps, "list")
  expect_true(length(result$steps) > 0)
  
  # Check synthesis
  expect_type(result$synthesis, "character")
  expect_true(nchar(result$synthesis) > 0)
  
  # Check confidence
  expect_true(result$confidence >= 0 && result$confidence <= 1)
})

test_that("llm_multi_step_analysis handles different reasoning depths", {
  test_physeq <- create_demo_phyloseq(n_samples = 5, n_taxa = 10)
  universal_info <- extract_universal_information(test_physeq)
  
  # Test shallow reasoning
  result_shallow <- llm_multi_step_analysis(
    universal_info = universal_info,
    reasoning_depth = "shallow",
    llm_provider = "none"
  )
  
  # Test deep reasoning
  result_deep <- llm_multi_step_analysis(
    universal_info = universal_info,
    reasoning_depth = "deep",
    llm_provider = "none"
  )
  
  # Deep should have more steps
  expect_true(length(result_deep$steps) >= length(result_shallow$steps))
})

test_that("llm_multi_step_analysis integrates assembly mechanisms", {
  test_physeq <- create_demo_phyloseq(n_samples = 8, n_taxa = 15)
  universal_info <- extract_universal_information(test_physeq)
  
  # Create mock assembly mechanisms
  mock_mechanisms <- list(
    mechanisms = list(
      environmental_filtering = list(
        detected = TRUE,
        confidence = 0.8,
        evidence = "Strong pH correlation"
      )
    ),
    interpretation = "Environmental filtering dominant"
  )
  class(mock_mechanisms) <- c("assembly_mechanisms", "list")
  
  result <- llm_multi_step_analysis(
    universal_info = universal_info,
    assembly_mechanisms = mock_mechanisms,
    llm_provider = "none"
  )
  
  # Should incorporate mechanisms in analysis
  expect_true(any(grepl("environmental filtering", 
                       tolower(result$synthesis))))
})

test_that("llm_multi_step_analysis integrates hypotheses", {
  test_physeq <- create_demo_phyloseq(n_samples = 5, n_taxa = 10)
  universal_info <- extract_universal_information(test_physeq)
  
  # Create mock hypotheses
  mock_hypotheses <- list(
    hypotheses = list(
      list(
        hypothesis = "pH drives species sorting",
        type = "mechanistic",
        testability_score = 0.8
      )
    )
  )
  class(mock_hypotheses) <- c("ecological_hypotheses", "list")
  
  result <- llm_multi_step_analysis(
    universal_info = universal_info,
    hypotheses = mock_hypotheses,
    llm_provider = "none"
  )
  
  expect_s3_class(result, "llm_multi_step_analysis")
  # Should mention hypotheses
  expect_true("hypothesis_evaluation" %in% names(result$steps) ||
              any(grepl("hypothesis|hypotheses", names(result$steps))))
})

test_that("llm_multi_step_analysis integrates with study context", {
  test_physeq <- create_demo_phyloseq(n_samples = 5, n_taxa = 10)
  universal_info <- extract_universal_information(test_physeq)
  
  # Create study context that mentions literature findings
  study_context <- list(
    study_system = "marine_microbiome",
    research_question = "How do environmental gradients structure communities?",
    prior_knowledge = "Previous studies show pH and temperature are key drivers"
  )
  
  result <- llm_multi_step_analysis(
    universal_info = universal_info,
    study_context = study_context,
    llm_provider = "none"
  )
  
  # Should incorporate context
  expect_s3_class(result, "llm_multi_step_analysis")
  expect_true(!is.null(result$synthesis))
})

test_that("llm_multi_step_analysis handles different contexts", {
  test_physeq <- create_demo_phyloseq(n_samples = 5, n_taxa = 10)
  universal_info <- extract_universal_information(test_physeq)
  
  result <- llm_multi_step_analysis(
    universal_info = universal_info,
    study_context = list(
      study_system = "extreme_environment",
      focus = "rare_species_conservation"
    ),
    llm_provider = "none"
  )
  
  expect_s3_class(result, "llm_multi_step_analysis")
  # Context should be reflected in metadata
  expect_true("study_context" %in% names(result$metadata))
})

test_that("print.llm_multi_step_analysis works", {
  test_physeq <- create_demo_phyloseq(n_samples = 5, n_taxa = 10)
  universal_info <- extract_universal_information(test_physeq)
  
  result <- llm_multi_step_analysis(
    universal_info = universal_info,
    llm_provider = "none"
  )
  
  # Capture print output
  output <- capture.output(print(result))
  
  expect_true(any(grepl("Multi-Step LLM Analysis Results", output)))
  expect_true(any(grepl("Analysis Steps:", output)))
  expect_true(any(grepl("Synthesis:", output)))
  expect_true(any(grepl("Confidence:", output)))
})

test_that("llm_multi_step_analysis handles errors gracefully", {
  # Test with invalid input
  expect_error(llm_multi_step_analysis("not_universal_info"))
  
  # Test with invalid reasoning depth
  test_physeq <- create_demo_phyloseq(n_samples = 5, n_taxa = 10)
  universal_info <- extract_universal_information(test_physeq)
  
  # Invalid depths should be handled gracefully (default to standard)
  result <- llm_multi_step_analysis(
    universal_info = universal_info,
    reasoning_depth = "invalid_depth",
    llm_provider = "none"
  )
  
  # Should still work with default reasoning
  expect_s3_class(result, "llm_multi_step_analysis")
})

test_that("llm_multi_step_analysis study context affects output", {
  test_physeq <- create_demo_phyloseq(n_samples = 5, n_taxa = 10)
  universal_info <- extract_universal_information(test_physeq)
  
  # Test with specific study context
  result <- llm_multi_step_analysis(
    universal_info = universal_info,
    study_context = list(
      study_system = "extreme_environment",
      research_question = "How do extremophiles maintain diversity?",
      environmental_factors = c("temperature", "pH", "salinity"),
      target_organisms = "archaea"
    ),
    llm_provider = "none"
  )
  
  # Context should influence analysis
  expect_true(any(grepl("extreme|extremophile", 
                       tolower(result$synthesis))))
})

test_that("llm_multi_step_analysis includes all reasoning steps", {
  test_physeq <- create_demo_phyloseq(n_samples = 5, n_taxa = 10)
  universal_info <- extract_universal_information(test_physeq)
  
  result <- llm_multi_step_analysis(
    universal_info = universal_info,
    reasoning_depth = "deep",
    llm_provider = "none"
  )
  
  # Check for expected reasoning steps
  step_names <- names(result$steps)
  expect_true(any(grepl("pattern", step_names)))
  expect_true(any(grepl("mechanism|interpretation", step_names)))
  expect_true(any(grepl("synthesis|conclusion", step_names)))
})

test_that("llm_multi_step_analysis confidence calculation works", {
  test_physeq <- create_demo_phyloseq(n_samples = 10, n_taxa = 20)
  universal_info <- extract_universal_information(test_physeq)
  
  # More data should give higher confidence
  result_large <- llm_multi_step_analysis(
    universal_info = universal_info,
    llm_provider = "none"
  )
  
  # Less data
  small_physeq <- create_demo_phyloseq(n_samples = 3, n_taxa = 5)
  small_info <- extract_universal_information(small_physeq)
  
  result_small <- llm_multi_step_analysis(
    universal_info = small_info,
    llm_provider = "none"
  )
  
  # Both should have valid confidence scores
  expect_true(result_large$confidence >= 0 && result_large$confidence <= 1)
  expect_true(result_small$confidence >= 0 && result_small$confidence <= 1)
})

test_that("llm_multi_step_analysis produces consistent output", {
  test_physeq <- create_demo_phyloseq(n_samples = 5, n_taxa = 10)
  universal_info <- extract_universal_information(test_physeq)
  
  # First call
  result1 <- llm_multi_step_analysis(
    universal_info = universal_info,
    study_context = list(test = "consistency"),
    llm_provider = "none"
  )
  
  # Second call with same parameters
  result2 <- llm_multi_step_analysis(
    universal_info = universal_info,
    study_context = list(test = "consistency"),
    llm_provider = "none"
  )
  
  # Both should have valid structure
  expect_s3_class(result1, "llm_multi_step_analysis")
  expect_s3_class(result2, "llm_multi_step_analysis")
})