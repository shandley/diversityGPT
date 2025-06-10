test_that("detect_assembly_mechanisms works with basic input", {
  # Create minimal universal_info object
  components <- data.frame(
    sample = paste0("S", 1:10),
    R = runif(10, 0, 1),
    E = runif(10, 0, 1),
    stringsAsFactors = FALSE
  )
  
  transformation_matrix <- diag(0.8, 4)
  rownames(transformation_matrix) <- c("shannon", "simpson", "observed", "chao1")
  colnames(transformation_matrix) <- c("shannon", "simpson", "observed", "chao1")
  
  universal_info <- list(
    information_components = components,
    transformation_matrix = transformation_matrix,
    quality_metrics = list(overall_quality = 0.8)
  )
  class(universal_info) <- "universal_information"
  
  # Test basic functionality
  result <- detect_assembly_mechanisms(universal_info)
  
  expect_s3_class(result, "assembly_mechanisms")
  expect_true(is.list(result))
  expect_true(all(c("mechanisms", "evidence", "patterns", "interpretation") %in% names(result)))
  expect_true(is.data.frame(result$mechanisms))
  expect_true(is.list(result$evidence))
  expect_true(is.list(result$patterns))
  expect_true(is.list(result$interpretation))
})

test_that("detect_assembly_mechanisms handles environmental data", {
  # Create test data with environmental variables
  components <- data.frame(
    sample = paste0("S", 1:15),
    R = c(rep(0.8, 5), rep(0.3, 5), rep(0.6, 5)),  # Clear pattern
    E = c(rep(0.2, 5), rep(0.7, 5), rep(0.5, 5)),  # Inverse pattern
    stringsAsFactors = FALSE
  )
  
  transformation_matrix <- diag(0.9, 4)
  rownames(transformation_matrix) <- c("shannon", "simpson", "observed", "chao1")
  colnames(transformation_matrix) <- c("shannon", "simpson", "observed", "chao1")
  
  universal_info <- list(
    information_components = components,
    transformation_matrix = transformation_matrix,
    quality_metrics = list(overall_quality = 0.9)
  )
  class(universal_info) <- "universal_information"
  
  # Environmental data with clear gradients
  env_data <- data.frame(
    sample_names = paste0("S", 1:15),
    pH = c(rep(6.0, 5), rep(7.5, 5), rep(7.0, 5)),
    temperature = c(rep(20, 5), rep(30, 5), rep(25, 5)),
    stringsAsFactors = FALSE
  )
  
  result <- detect_assembly_mechanisms(universal_info, environmental_data = env_data)
  
  expect_s3_class(result, "assembly_mechanisms")
  expect_true(nrow(result$mechanisms) > 0)
  
  # Should detect environmental filtering due to clear correlation
  mechanisms <- result$mechanisms$mechanism
  expect_true(any(grepl("Environmental", mechanisms, ignore.case = TRUE)))
})

test_that("detect_assembly_mechanisms identifies competitive exclusion", {
  # Create data suggesting competitive exclusion (high R, low E)
  components <- data.frame(
    sample = paste0("S", 1:10),
    R = runif(10, 0.8, 1.0),  # High richness
    E = runif(10, 0.1, 0.3),  # Low evenness
    stringsAsFactors = FALSE
  )
  
  transformation_matrix <- diag(0.7, 4)
  rownames(transformation_matrix) <- c("shannon", "simpson", "observed", "chao1")
  colnames(transformation_matrix) <- c("shannon", "simpson", "observed", "chao1")
  
  universal_info <- list(
    information_components = components,
    transformation_matrix = transformation_matrix,
    quality_metrics = list(overall_quality = 0.7)
  )
  class(universal_info) <- "universal_information"
  
  result <- detect_assembly_mechanisms(universal_info)
  
  # Should detect competitive exclusion
  mechanisms <- result$mechanisms$mechanism
  expect_true(any(grepl("Competitive", mechanisms, ignore.case = TRUE)))
  
  # Check that confidence is reasonable
  comp_mech <- result$mechanisms[grepl("Competitive", result$mechanisms$mechanism, ignore.case = TRUE), ]
  if (nrow(comp_mech) > 0) {
    expect_true(comp_mech$confidence[1] > 0.1)
  }
})

test_that("detect_assembly_mechanisms identifies neutral processes", {
  # Create data suggesting neutral processes (high variance, low predictability)
  components <- data.frame(
    sample = paste0("S", 1:20),
    R = rnorm(20, 0.5, 0.3),  # High variance
    E = rnorm(20, 0.5, 0.3),  # High variance
    stringsAsFactors = FALSE
  )
  
  # Low quality transformation matrix (low predictability)
  transformation_matrix <- matrix(runif(16, 0.2, 0.4), 4, 4)
  diag(transformation_matrix) <- runif(4, 0.3, 0.5)
  rownames(transformation_matrix) <- c("shannon", "simpson", "observed", "chao1")
  colnames(transformation_matrix) <- c("shannon", "simpson", "observed", "chao1")
  
  universal_info <- list(
    information_components = components,
    transformation_matrix = transformation_matrix,
    quality_metrics = list(overall_quality = 0.4)
  )
  class(universal_info) <- "universal_information"
  
  result <- detect_assembly_mechanisms(universal_info)
  
  # Should detect neutral drift
  mechanisms <- result$mechanisms$mechanism
  expect_true(any(grepl("Neutral", mechanisms, ignore.case = TRUE)))
})

test_that("generate_ecological_hypotheses works with basic input", {
  # Create test universal_info
  components <- data.frame(
    sample = paste0("S", 1:10),
    R = runif(10, 0.2, 0.8),
    E = runif(10, 0.3, 0.7),
    stringsAsFactors = FALSE
  )
  
  transformation_matrix <- diag(0.8, 4)
  rownames(transformation_matrix) <- c("shannon", "simpson", "observed", "chao1")
  colnames(transformation_matrix) <- c("shannon", "simpson", "observed", "chao1")
  
  universal_info <- list(
    information_components = components,
    transformation_matrix = transformation_matrix,
    quality_metrics = list(overall_quality = 0.8)
  )
  class(universal_info) <- "universal_information"
  
  result <- generate_ecological_hypotheses(universal_info)
  
  expect_s3_class(result, "ecological_hypotheses")
  expect_true(is.list(result))
  expect_true(all(c("hypotheses", "experimental_designs", "predictions", "context_analysis") %in% names(result)))
  expect_true(is.data.frame(result$hypotheses))
})

test_that("generate_ecological_hypotheses creates mechanistic hypotheses", {
  # Create data with clear R dominance
  components <- data.frame(
    sample = paste0("S", 1:10),
    R = runif(10, 0.7, 0.9),  # High R values
    E = runif(10, 0.2, 0.4),  # Low E values
    stringsAsFactors = FALSE
  )
  
  transformation_matrix <- diag(0.8, 4)
  rownames(transformation_matrix) <- c("shannon", "simpson", "observed", "chao1")
  colnames(transformation_matrix) <- c("shannon", "simpson", "observed", "chao1")
  
  universal_info <- list(
    information_components = components,
    transformation_matrix = transformation_matrix,
    quality_metrics = list(overall_quality = 0.8)
  )
  class(universal_info) <- "universal_information"
  
  result <- generate_ecological_hypotheses(
    universal_info,
    hypothesis_types = "mechanistic"
  )
  
  expect_true(nrow(result$hypotheses) > 0)
  expect_true(all(result$hypotheses$type == "mechanistic"))
  
  # Should generate richness-related hypothesis
  hyp_text <- paste(result$hypotheses$hypothesis, collapse = " ")
  expect_true(grepl("richness", hyp_text, ignore.case = TRUE))
})

test_that("generate_ecological_hypotheses creates predictive hypotheses", {
  # Create data with high transformation quality
  components <- data.frame(
    sample = paste0("S", 1:10),
    R = runif(10, 0.4, 0.6),
    E = runif(10, 0.4, 0.6),
    stringsAsFactors = FALSE
  )
  
  transformation_matrix <- diag(0.9, 4)  # High quality
  rownames(transformation_matrix) <- c("shannon", "simpson", "observed", "chao1")
  colnames(transformation_matrix) <- c("shannon", "simpson", "observed", "chao1")
  
  universal_info <- list(
    information_components = components,
    transformation_matrix = transformation_matrix,
    quality_metrics = list(overall_quality = 0.9)
  )
  class(universal_info) <- "universal_information"
  
  result <- generate_ecological_hypotheses(
    universal_info,
    hypothesis_types = "predictive"
  )
  
  expect_true(nrow(result$hypotheses) > 0)
  expect_true(all(result$hypotheses$type == "predictive"))
  
  # Should generate predictability-related hypothesis
  hyp_text <- paste(result$hypotheses$hypothesis, collapse = " ")
  expect_true(grepl("predictable", hyp_text, ignore.case = TRUE))
})

test_that("generate_ecological_hypotheses incorporates study context", {
  components <- data.frame(
    sample = paste0("S", 1:10),
    R = runif(10, 0.4, 0.6),
    E = runif(10, 0.4, 0.6),
    stringsAsFactors = FALSE
  )
  
  transformation_matrix <- diag(0.8, 4)
  rownames(transformation_matrix) <- c("shannon", "simpson", "observed", "chao1")
  colnames(transformation_matrix) <- c("shannon", "simpson", "observed", "chao1")
  
  universal_info <- list(
    information_components = components,
    transformation_matrix = transformation_matrix,
    quality_metrics = list(overall_quality = 0.8)
  )
  class(universal_info) <- "universal_information"
  
  study_context <- list(
    environment = "soil",
    organism = "bacteria",
    condition = "nutrient_gradient"
  )
  
  result <- generate_ecological_hypotheses(
    universal_info,
    study_context = study_context,
    hypothesis_types = c("mechanistic", "predictive")
  )
  
  expect_true(!is.null(result$context_analysis))
  expect_equal(result$context_analysis$environment_type, "soil")
  expect_equal(result$context_analysis$organism_group, "bacteria")
  expect_equal(result$context_analysis$experimental_condition, "nutrient_gradient")
  
  # Should generate soil-specific hypothesis
  hyp_text <- paste(result$hypotheses$hypothesis, collapse = " ")
  expect_true(grepl("pH|nutrient", hyp_text, ignore.case = TRUE))
})

test_that("generate_ecological_hypotheses integrates with assembly mechanisms", {
  # Create universal_info
  components <- data.frame(
    sample = paste0("S", 1:10),
    R = runif(10, 0.4, 0.6),
    E = runif(10, 0.4, 0.6),
    stringsAsFactors = FALSE
  )
  
  transformation_matrix <- diag(0.8, 4)
  rownames(transformation_matrix) <- c("shannon", "simpson", "observed", "chao1")
  colnames(transformation_matrix) <- c("shannon", "simpson", "observed", "chao1")
  
  universal_info <- list(
    information_components = components,
    transformation_matrix = transformation_matrix,
    quality_metrics = list(overall_quality = 0.8)
  )
  class(universal_info) <- "universal_information"
  
  # Create mock assembly mechanisms
  mechanisms_df <- data.frame(
    mechanism = "Environmental Filtering",
    confidence = 0.8,
    p_value = 0.01,
    effect_size = 0.7,
    description = "Test mechanism",
    stringsAsFactors = FALSE
  )
  
  assembly_mechanisms <- list(
    mechanisms = mechanisms_df,
    evidence = list(environmental_filtering = list(method = "correlation")),
    patterns = list(),
    interpretation = list()
  )
  class(assembly_mechanisms) <- "assembly_mechanisms"
  
  result <- generate_ecological_hypotheses(
    universal_info,
    assembly_mechanisms = assembly_mechanisms,
    hypothesis_types = "mechanistic"
  )
  
  # Should incorporate mechanism information
  hyp_text <- paste(result$hypotheses$hypothesis, collapse = " ")
  expect_true(grepl("environmental|gradient", hyp_text, ignore.case = TRUE))
})

test_that("generate_ecological_hypotheses includes experimental designs", {
  components <- data.frame(
    sample = paste0("S", 1:10),
    R = runif(10, 0.4, 0.6),
    E = runif(10, 0.4, 0.6),
    stringsAsFactors = FALSE
  )
  
  transformation_matrix <- diag(0.8, 4)
  rownames(transformation_matrix) <- c("shannon", "simpson", "observed", "chao1")
  colnames(transformation_matrix) <- c("shannon", "simpson", "observed", "chao1")
  
  universal_info <- list(
    information_components = components,
    transformation_matrix = transformation_matrix,
    quality_metrics = list(overall_quality = 0.8)
  )
  class(universal_info) <- "universal_information"
  
  result <- generate_ecological_hypotheses(
    universal_info,
    hypothesis_types = c("mechanistic", "experimental")
  )
  
  if (nrow(result$hypotheses) > 0) {
    expect_true(length(result$experimental_designs) > 0)
    expect_true(length(result$predictions) > 0)
    
    # Check experimental design structure
    design <- result$experimental_designs[[1]]
    expect_true(is.list(design))
    expect_true("approach" %in% names(design))
    expect_true("sample_size" %in% names(design))
    expect_true("variables" %in% names(design))
    
    # Check prediction structure
    prediction <- result$predictions[[1]]
    expect_true(is.list(prediction))
    expect_true("primary_prediction" %in% names(prediction))
    expect_true("measurable_outcomes" %in% names(prediction))
    expect_true("statistical_tests" %in% names(prediction))
  }
})

test_that("hypothesis scoring works correctly", {
  components <- data.frame(
    sample = paste0("S", 1:10),
    R = runif(10, 0.4, 0.6),
    E = runif(10, 0.4, 0.6),
    stringsAsFactors = FALSE
  )
  
  transformation_matrix <- diag(0.8, 4)
  rownames(transformation_matrix) <- c("shannon", "simpson", "observed", "chao1")
  colnames(transformation_matrix) <- c("shannon", "simpson", "observed", "chao1")
  
  universal_info <- list(
    information_components = components,
    transformation_matrix = transformation_matrix,
    quality_metrics = list(overall_quality = 0.8)
  )
  class(universal_info) <- "universal_information"
  
  result <- generate_ecological_hypotheses(
    universal_info,
    hypothesis_types = c("mechanistic", "predictive", "comparative"),
    max_hypotheses = 3
  )
  
  if (nrow(result$hypotheses) > 0) {
    # Check that scores are present and reasonable
    expect_true("novelty" %in% names(result$hypotheses))
    expect_true("testability" %in% names(result$hypotheses))
    expect_true("combined_score" %in% names(result$hypotheses))
    
    expect_true(all(result$hypotheses$novelty >= 0 & result$hypotheses$novelty <= 1))
    expect_true(all(result$hypotheses$testability >= 0 & result$hypotheses$testability <= 1))
    
    # Should be sorted by combined score
    if (nrow(result$hypotheses) > 1) {
      expect_true(all(diff(result$hypotheses$combined_score) <= 0))
    }
  }
})

test_that("print method works for assembly_mechanisms", {
  # Create test assembly mechanisms object
  mechanisms_df <- data.frame(
    mechanism = c("Environmental Filtering", "Competitive Exclusion"),
    confidence = c(0.8, 0.6),
    p_value = c(0.01, 0.05),
    effect_size = c(0.7, 0.5),
    description = c("Abiotic filtering", "Competition effects"),
    stringsAsFactors = FALSE
  )
  
  assembly_result <- list(
    mechanisms = mechanisms_df,
    evidence = list(),
    patterns = list(),
    interpretation = list(
      summary = "Environmental Filtering",
      confidence_level = "High",
      biological_meaning = "Test meaning",
      recommendations = c("Rec 1", "Rec 2")
    )
  )
  class(assembly_result) <- "assembly_mechanisms"
  
  # Test that print doesn't error
  expect_output(print(assembly_result), "Community Assembly Mechanism Analysis")
  expect_output(print(assembly_result), "Environmental Filtering")
})

test_that("print method works for ecological_hypotheses", {
  # Create test hypotheses object
  hypotheses_df <- data.frame(
    type = c("mechanistic", "predictive"),
    hypothesis = c("Test hypothesis 1", "Test hypothesis 2"),
    rationale = c("Test rationale 1", "Test rationale 2"),
    novelty = c(0.7, 0.5),
    testability = c(0.8, 0.9),
    combined_score = c(0.75, 0.72),
    stringsAsFactors = FALSE
  )
  
  hypotheses_result <- list(
    hypotheses = hypotheses_df,
    experimental_designs = list(),
    predictions = list(),
    context_analysis = list(
      environment_type = "soil",
      organism_group = "bacteria",
      experimental_condition = "gradient",
      novelty_potential = 0.6
    )
  )
  class(hypotheses_result) <- "ecological_hypotheses"
  
  # Test that print doesn't error
  expect_output(print(hypotheses_result), "Ecological Hypothesis Generation Results")
  expect_output(print(hypotheses_result), "Test hypothesis 1")
})

test_that("error handling works correctly", {
  # Test with invalid input
  expect_error(
    detect_assembly_mechanisms("not_universal_info"),
    "universal_info must be a universal_information object"
  )
  
  expect_error(
    generate_ecological_hypotheses("not_universal_info"),
    "universal_info must be a universal_information object"
  )
})

test_that("edge cases are handled", {
  # Test with minimal data
  components <- data.frame(
    sample = "S1",
    R = 0.5,
    E = 0.5,
    stringsAsFactors = FALSE
  )
  
  transformation_matrix <- matrix(0.5, 1, 1)
  rownames(transformation_matrix) <- "shannon"
  colnames(transformation_matrix) <- "shannon"
  
  universal_info <- list(
    information_components = components,
    transformation_matrix = transformation_matrix,
    quality_metrics = list(overall_quality = 0.5)
  )
  class(universal_info) <- "universal_information"
  
  # Should not error with minimal data
  result1 <- detect_assembly_mechanisms(universal_info)
  expect_s3_class(result1, "assembly_mechanisms")
  
  result2 <- generate_ecological_hypotheses(universal_info)
  expect_s3_class(result2, "ecological_hypotheses")
})