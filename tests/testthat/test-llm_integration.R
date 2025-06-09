test_that("interpret_diversity validates input correctly", {
  # Test with invalid input
  expect_error(
    interpret_diversity(data.frame(x = 1:5)),
    "must be diversity_results or consensus_results"
  )
})

test_that("interpret_diversity handles missing API keys", {
  mock_results <- structure(
    data.frame(
      sample = paste0("Sample", 1:5),
      shannon = rnorm(5, 2, 0.5)
    ),
    metrics = "shannon",
    class = c("diversity_results", "data.frame")
  )
  
  # Mock missing API key
  withr::with_envvar(
    c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = ""),
    {
      expect_error(
        interpret_diversity(mock_results),
        "No anthropic API key found"
      )
    }
  )
})

test_that("pattern extraction works for diversity results", {
  mock_results <- structure(
    data.frame(
      sample = paste0("Sample", 1:10),
      shannon = rnorm(10, 2, 0.5),
      simpson = rnorm(10, 0.7, 0.1),
      group = rep(c("A", "B"), each = 5)
    ),
    metrics = c("shannon", "simpson"),
    class = c("diversity_results", "data.frame")
  )
  
  patterns <- diversityGPT:::extract_diversity_patterns(mock_results)
  
  expect_true(is.list(patterns))
  expect_true("shannon" %in% names(patterns))
  expect_true("simpson" %in% names(patterns))
  expect_true("group_comparison" %in% names(patterns))
  expect_equal(patterns$n_samples, 10)
  expect_equal(patterns$metrics_calculated, c("shannon", "simpson"))
})

test_that("pattern extraction works for consensus results", {
  mock_consensus <- structure(
    list(
      method = "weighted_mean",
      n_samples = 10,
      interpretation = list(
        dominant_metric = "shannon",
        weight_distribution = "balanced",
        conflict_status = "consensus"
      ),
      method_weights = c(shannon = 0.6, simpson = 0.4),
      conflict_analysis = NULL
    ),
    class = c("consensus_results", "list")
  )
  
  patterns <- diversityGPT:::extract_consensus_patterns(mock_consensus)
  
  expect_equal(patterns$method, "weighted_mean")
  expect_equal(patterns$dominant_metric, "shannon")
  expect_equal(patterns$n_samples, 10)
  expect_length(patterns$metric_weights, 2)
})

test_that("context prompt building works", {
  context <- list(
    environment = "human_gut",
    condition = "antibiotic_treatment",
    organism = "bacteria"
  )
  
  prompt <- diversityGPT:::build_context_prompt(context)
  
  expect_true(is.character(prompt))
  expect_true(grepl("human_gut", prompt))
  expect_true(grepl("antibiotic_treatment", prompt))
  expect_true(grepl("bacteria", prompt))
})

test_that("fallback interpretation works", {
  patterns <- list(
    shannon = list(mean = 2.5, sd = 0.5, cv = 0.2),
    metrics_calculated = "shannon"
  )
  
  context <- list(environment = "test")
  
  result <- diversityGPT:::generate_fallback_interpretation(patterns, context)
  
  expect_true(is.list(result))
  expect_true("interpretation" %in% names(result))
  expect_true("confidence" %in% names(result))
  expect_equal(result$confidence, "Low")
})

test_that("section extraction works", {
  content <- "Some text\nINTERPRETATION: This is the interpretation section.\nBIOLOGICAL_INSIGHTS: These are insights.\nMore text."
  
  interpretation <- diversityGPT:::extract_section(content, "INTERPRETATION")
  insights <- diversityGPT:::extract_section(content, "BIOLOGICAL_INSIGHTS")
  
  expect_true(grepl("interpretation section", interpretation))
  expect_true(grepl("insights", insights))
})

test_that("print method works for interpretation results", {
  mock_interpretation <- structure(
    list(
      provider = "anthropic",
      timestamp = Sys.time(),
      interpretation = "Test interpretation",
      biological_insights = "Test insights",
      confidence = "Medium",
      recommendations = "Test recommendations",
      hypotheses = "Test hypotheses"
    ),
    class = c("diversity_interpretation", "list")
  )
  
  # Test that print doesn't error
  expect_no_error(print(mock_interpretation))
})