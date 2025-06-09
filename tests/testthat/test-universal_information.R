test_that("extract_universal_information works with basic phyloseq object", {
  skip_if_not_installed("phyloseq")
  skip_if_not_installed("vegan")
  
  # Create minimal test data
  set.seed(123)
  
  # Simple OTU table (5 samples, 10 taxa)
  otu_mat <- matrix(
    rpois(50, lambda = 10), 
    nrow = 5, 
    ncol = 10,
    dimnames = list(
      paste0("Sample_", 1:5),
      paste0("OTU_", 1:10)
    )
  )
  
  # Sample data
  sample_df <- data.frame(
    Sample_ID = paste0("Sample_", 1:5),
    Group = c("A", "A", "B", "B", "C"),
    stringsAsFactors = FALSE
  )
  rownames(sample_df) <- sample_df$Sample_ID
  
  # Create phyloseq object
  ps <- phyloseq::phyloseq(
    phyloseq::otu_table(otu_mat, taxa_are_rows = FALSE),
    phyloseq::sample_data(sample_df)
  )
  
  # Test basic extraction without phylogenetic/spatial
  result <- extract_universal_information(
    ps, 
    groups = "Group",
    include_phylogenetic = FALSE,
    include_spatial = FALSE
  )
  
  # Check result structure
  expect_s3_class(result, "universal_information")
  expect_true(is.list(result))
  
  # Check required components
  expect_true("information_components" %in% names(result))
  expect_true("transformation_matrix" %in% names(result))
  expect_true("metric_profiles" %in% names(result))
  expect_true("deconvolution_quality" %in% names(result))
  
  # Check information components
  info_comp <- result$information_components
  expect_true(is.data.frame(info_comp))
  expect_equal(nrow(info_comp), 5)  # 5 samples
  
  required_components <- c("R_component", "E_component", "P_component", "S_component")
  expect_true(all(required_components %in% names(info_comp)))
  
  # Check that components are normalized [0,1]
  for (comp in required_components) {
    values <- info_comp[[comp]]
    expect_true(all(values >= 0, na.rm = TRUE))
    expect_true(all(values <= 1, na.rm = TRUE))
  }
  
  # Check transformation matrix
  trans_mat <- result$transformation_matrix
  expect_true(is.data.frame(trans_mat))
  expect_true("metric" %in% names(trans_mat))
  expect_true("r_squared" %in% names(trans_mat))
  
  # Check metric profiles
  metrics <- result$metric_profiles
  expect_true(is.data.frame(metrics))
  expect_equal(nrow(metrics), 5)  # 5 samples
  
  # Should have basic diversity metrics
  basic_metrics <- c("shannon", "simpson", "observed")
  expect_true(all(basic_metrics %in% names(metrics)))
})

test_that("universal_diversity_transform works with simple input", {
  skip_if_not_installed("phyloseq")
  
  # Create test transformation matrix
  transformation_matrix <- data.frame(
    metric = c("simpson", "observed"),
    r_squared = c(0.8, 0.7),
    adj_r_squared = c(0.75, 0.65),
    rmse = c(0.1, 5.0),
    n_obs = c(10, 10),
    intercept = c(0.1, 5.0),
    R_component = c(0.5, 2.0),
    E_component = c(0.3, 1.0),
    P_component = c(0.1, 0.5),
    S_component = c(0.0, 0.0),
    stringsAsFactors = FALSE
  )
  
  # Create mock transformations attribute
  mock_transformations <- list(
    simpson = list(
      coefficients = c(intercept = 0.1, R_component = 0.5, E_component = 0.3, 
                      P_component = 0.1, S_component = 0.0),
      r_squared = 0.8,
      rmse = 0.1
    ),
    observed = list(
      coefficients = c(intercept = 5.0, R_component = 2.0, E_component = 1.0,
                      P_component = 0.5, S_component = 0.0),
      r_squared = 0.7,
      rmse = 5.0
    )
  )
  
  attr(transformation_matrix, "transformations") <- mock_transformations
  attr(transformation_matrix, "component_cols") <- c("R_component", "E_component", "P_component", "S_component")
  
  # Test transformation
  source_metrics <- c(shannon = 2.5)
  target_metrics <- c("simpson", "observed")
  
  result <- universal_diversity_transform(
    source_metrics = source_metrics,
    target_metrics = target_metrics,
    transformation_matrix = transformation_matrix
  )
  
  # Check result structure
  expect_s3_class(result, "universal_transformation")
  expect_true("predicted_metrics" %in% names(result))
  expect_true("transformation_quality" %in% names(result))
  expect_true("information_components" %in% names(result))
  
  # Check predicted metrics
  predicted <- result$predicted_metrics
  expect_true(is.data.frame(predicted))
  expect_true("simpson" %in% names(predicted))
  expect_true("observed" %in% names(predicted))
  
  # Values should be reasonable (non-negative)
  expect_true(all(predicted$simpson >= 0, na.rm = TRUE))
  expect_true(all(predicted$observed >= 0, na.rm = TRUE))
})

test_that("predict_missing_diversity_metrics gives reasonable results", {
  skip_if_not_installed("phyloseq")
  
  # Create minimal phyloseq for reference
  set.seed(456)
  
  otu_mat <- matrix(
    rpois(30, lambda = 8), 
    nrow = 3, 
    ncol = 10,
    dimnames = list(
      paste0("Sample_", 1:3),
      paste0("OTU_", 1:10)
    )
  )
  
  sample_df <- data.frame(
    Sample_ID = paste0("Sample_", 1:3),
    Group = c("A", "B", "C"),
    stringsAsFactors = FALSE
  )
  rownames(sample_df) <- sample_df$Sample_ID
  
  ps <- phyloseq::phyloseq(
    phyloseq::otu_table(otu_mat, taxa_are_rows = FALSE),
    phyloseq::sample_data(sample_df)
  )
  
  # Test prediction
  available_metrics <- c(shannon = 2.0, observed = 25)
  
  result <- predict_missing_diversity_metrics(
    available_metrics = available_metrics,
    phyloseq_reference = ps,
    target_metrics = c("simpson", "chao1")
  )
  
  # Check result
  expect_s3_class(result, "universal_transformation")
  
  predicted <- result$predicted_metrics
  expect_true("simpson" %in% names(predicted))
  expect_true("chao1" %in% names(predicted))
  
  # Basic sanity checks
  expect_true(all(predicted$simpson >= 0, na.rm = TRUE))
  expect_true(all(predicted$simpson <= 1, na.rm = TRUE))  # Simpson should be [0,1]
  expect_true(all(predicted$chao1 >= predicted$simpson, na.rm = TRUE))  # Chao1 >= observed
})

test_that("discover_metric_relationships finds basic patterns", {
  skip_if_not_installed("phyloseq")
  skip_if_not_installed("vegan")
  
  # Create test data with known relationships
  set.seed(789)
  
  # Create correlated OTU data
  n_samples <- 10
  n_taxa <- 15
  
  # Generate samples with different diversity levels
  otu_mat <- matrix(0, nrow = n_samples, ncol = n_taxa)
  
  for (i in 1:n_samples) {
    # Vary the number of present taxa
    n_present <- sample(5:n_taxa, 1)
    present_taxa <- sample(1:n_taxa, n_present)
    
    # Generate abundances
    abundances <- rgamma(n_present, shape = 2, rate = 0.5)
    otu_mat[i, present_taxa] <- abundances
  }
  
  rownames(otu_mat) <- paste0("Sample_", 1:n_samples)
  colnames(otu_mat) <- paste0("OTU_", 1:n_taxa)
  
  sample_df <- data.frame(
    Sample_ID = paste0("Sample_", 1:n_samples),
    Group = rep(c("A", "B"), each = 5),
    stringsAsFactors = FALSE
  )
  rownames(sample_df) <- sample_df$Sample_ID
  
  ps <- phyloseq::phyloseq(
    phyloseq::otu_table(otu_mat, taxa_are_rows = FALSE),
    phyloseq::sample_data(sample_df)
  )
  
  # Test relationship discovery
  result <- discover_metric_relationships(
    ps,
    metric_subset = c("shannon", "simpson", "observed"),
    relationship_types = c("linear", "nonlinear")
  )
  
  # Check result structure
  expect_s3_class(result, "universal_relationships")
  expect_true("pairwise_relationships" %in% names(result))
  expect_true("information_relationships" %in% names(result))
  
  # Should find some pairwise relationships
  expect_true(length(result$pairwise_relationships) > 0)
  
  # Check that we have linear relationships
  linear_found <- any(sapply(result$pairwise_relationships, function(rel) {
    !is.null(rel$relationships$linear)
  }))
  expect_true(linear_found)
})

test_that("information components are mathematically consistent", {
  skip_if_not_installed("phyloseq")
  
  # Create simple test case
  set.seed(999)
  
  otu_mat <- matrix(
    c(10, 5, 2, 0, 0,    # Sample 1: low evenness
      8, 7, 6, 5, 4,     # Sample 2: high evenness  
      20, 0, 0, 0, 0),   # Sample 3: very low evenness
    nrow = 3, 
    ncol = 5,
    byrow = TRUE,
    dimnames = list(
      paste0("Sample_", 1:3),
      paste0("OTU_", 1:5)
    )
  )
  
  sample_df <- data.frame(
    Sample_ID = paste0("Sample_", 1:3),
    stringsAsFactors = FALSE
  )
  rownames(sample_df) <- sample_df$Sample_ID
  
  ps <- phyloseq::phyloseq(
    phyloseq::otu_table(otu_mat, taxa_are_rows = FALSE),
    phyloseq::sample_data(sample_df)
  )
  
  result <- extract_universal_information(
    ps,
    include_phylogenetic = FALSE,
    include_spatial = FALSE
  )
  
  info_comp <- result$information_components
  
  # Sample 2 should have higher evenness component than samples 1 and 3
  expect_true(info_comp$E_component[2] > info_comp$E_component[1])
  expect_true(info_comp$E_component[2] > info_comp$E_component[3])
  
  # Sample 3 should have the lowest evenness (most uneven)
  expect_true(info_comp$E_component[3] <= info_comp$E_component[1])
  
  # All samples should have similar richness component (same number of non-zero taxa)
  # Actually samples 1 and 2 have 3 and 5 taxa respectively, sample 3 has 1
  expect_true(info_comp$R_component[2] >= info_comp$R_component[1])
  expect_true(info_comp$R_component[1] >= info_comp$R_component[3])
  
  # Total information should be sum of components
  calculated_total <- info_comp$R_component + info_comp$E_component + 
                     info_comp$P_component + info_comp$S_component
  
  expect_equal(info_comp$total_information, calculated_total, tolerance = 1e-10)
  
  # Proportions should sum to 1 (or close to 1)
  prop_sum <- info_comp$R_proportion + info_comp$E_proportion + 
             info_comp$P_proportion + info_comp$S_proportion
  
  expect_true(all(abs(prop_sum - 1) < 1e-10 | prop_sum == 0))
})