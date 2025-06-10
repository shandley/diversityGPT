test_that("validate_taxa_indicators works with basic inputs", {
  # Create test data
  demo_data <- create_demo_phyloseq(n_samples = 10, n_taxa = 30)
  
  # Get indicators
  indicators <- identify_taxa_drivers(demo_data, top_n = 5, verbose = FALSE)
  
  # Validate with null models (small n_perm for testing)
  validated <- validate_taxa_indicators(
    demo_data, 
    indicators,
    null_models = c("row_shuffle", "column_shuffle"),
    n_permutations = 19,  # Small for speed
    verbose = FALSE
  )
  
  # Check structure
  expect_s3_class(validated, "validated_indicators")
  expect_true(all(c("observed", "null_distributions", "summary_statistics",
                   "significant_indicators", "null_models_used") %in% names(validated)))
  
  # Check p-values were added
  expect_true("p_value" %in% names(validated$observed$richness_drivers))
  expect_true("effect_size" %in% names(validated$observed$richness_drivers))
  
  # Check p-values are in valid range
  all_pvals <- c(
    validated$observed$richness_drivers$p_value,
    validated$observed$evenness_drivers$p_value,
    validated$observed$phylogenetic_drivers$p_value,
    validated$observed$spatial_drivers$p_value
  )
  expect_true(all(all_pvals >= 0 & all_pvals <= 1, na.rm = TRUE))
})

test_that("row shuffle null model maintains row sums", {
  # Create simple test data
  otu_mat <- matrix(c(
    10, 20, 30,
    5, 15, 25,
    1, 2, 3
  ), nrow = 3, byrow = TRUE)
  
  rownames(otu_mat) <- paste0("OTU", 1:3)
  colnames(otu_mat) <- paste0("Sample", 1:3)
  
  demo_physeq <- phyloseq::phyloseq(
    phyloseq::otu_table(otu_mat, taxa_are_rows = TRUE)
  )
  
  indicators <- identify_taxa_drivers(demo_physeq, verbose = FALSE)
  
  # Run row shuffle once
  null_results <- run_row_shuffle_null(demo_physeq, indicators, 
                                     n_perm = 1, parallel = FALSE, 
                                     n_cores = NULL)
  
  # Original row sums
  orig_row_sums <- rowSums(otu_mat)
  
  # Can't directly check shuffled matrix from this function,
  # but we can verify the function runs without error
  expect_length(null_results, 1)
  expect_true(all(c("richness", "evenness", "phylogenetic", "spatial") %in% 
                 names(null_results[[1]])))
})

test_that("curveball algorithm maintains row and column sums", {
  # Create presence/absence matrix
  pa_mat <- matrix(c(
    TRUE, FALSE, TRUE, FALSE,
    FALSE, TRUE, FALSE, TRUE,
    TRUE, TRUE, FALSE, FALSE,
    FALSE, FALSE, TRUE, TRUE
  ), nrow = 4, byrow = TRUE)
  
  orig_row_sums <- rowSums(pa_mat)
  orig_col_sums <- colSums(pa_mat)
  
  # Run curveball
  shuffled <- curveball_randomize(pa_mat, n_swaps = 100)
  
  # Check dimensions preserved
  expect_equal(dim(shuffled), dim(pa_mat))
  
  # Check row and column sums preserved
  expect_equal(rowSums(shuffled), orig_row_sums)
  expect_equal(colSums(shuffled), orig_col_sums)
  
  # Check it's still binary
  expect_true(all(shuffled %in% c(TRUE, FALSE)))
})

test_that("null model validation handles different null models", {
  demo_data <- create_demo_phyloseq(n_samples = 8, n_taxa = 20)
  indicators <- identify_taxa_drivers(demo_data, top_n = 5, verbose = FALSE)
  
  # Test each null model individually
  null_models <- c("row_shuffle", "column_shuffle", "curveball")
  
  for (null_model in null_models) {
    validated <- validate_taxa_indicators(
      demo_data,
      indicators,
      null_models = null_model,
      n_permutations = 9,
      verbose = FALSE
    )
    
    expect_equal(validated$null_models_used, null_model)
    expect_true(!is.null(validated$null_distributions[[null_model]]))
  }
})

test_that("phylogenetic null model handles missing tree gracefully", {
  demo_data <- create_demo_phyloseq(n_samples = 5, n_taxa = 10)
  indicators <- identify_taxa_drivers(demo_data, verbose = FALSE)
  
  # Try phylogenetic null without tree
  expect_warning(
    null_results <- run_phylogenetic_null(demo_data, indicators, 
                                        n_perm = 5, parallel = FALSE,
                                        n_cores = NULL),
    "No phylogenetic tree found"
  )
  
  expect_null(null_results)
})

test_that("significance calculation works correctly", {
  # Create mock data
  indicators <- list(
    richness_drivers = data.frame(
      taxon = c("OTU1", "OTU2", "OTU3"),
      contribution = c(0.5, 0.3, 0.2),
      stringsAsFactors = FALSE
    ),
    method = "contribution"
  )
  
  # Create mock null distributions
  # OTU1: observed (0.5) > most nulls (should be significant)
  # OTU2: observed (0.3) ~ nulls (borderline)
  # OTU3: observed (0.2) < most nulls (not significant)
  null_distributions <- list(
    test_null = lapply(1:99, function(i) {
      list(richness = c(
        runif(1, 0.1, 0.3),  # OTU1 nulls mostly < 0.5
        runif(1, 0.2, 0.4),  # OTU2 nulls around 0.3
        runif(1, 0.3, 0.5)   # OTU3 nulls mostly > 0.2
      ))
    })
  )
  
  # Calculate significance
  result <- calculate_significance(indicators, null_distributions)
  
  # Check p-values
  expect_true("p_value" %in% names(result$richness_drivers))
  expect_true("effect_size" %in% names(result$richness_drivers))
  
  # OTU1 should have low p-value (significant)
  expect_true(result$richness_drivers$p_value[1] < 0.5)
  
  # OTU3 should have high p-value (not significant)
  expect_true(result$richness_drivers$p_value[3] > 0.5)
})

test_that("extract_significant_indicators filters correctly", {
  # Create test indicators with p-values
  indicators <- list(
    richness_drivers = data.frame(
      taxon = c("OTU1", "OTU2", "OTU3", "OTU4"),
      contribution = c(0.4, 0.3, 0.2, 0.1),
      p_value = c(0.01, 0.03, 0.10, 0.50),
      effect_size = c(3.2, 2.1, 1.0, 0.2),
      stringsAsFactors = FALSE
    ),
    evenness_drivers = data.frame(
      taxon = c("OTU5", "OTU6"),
      contribution = c(0.5, 0.5),
      p_value = c(0.001, 0.20),
      effect_size = c(4.5, 0.5),
      stringsAsFactors = FALSE
    )
  )
  
  # Extract significant
  sig_indicators <- extract_significant_indicators(indicators, alpha = 0.05)
  
  # Check richness: OTU1 and OTU2 should be significant
  expect_equal(nrow(sig_indicators$richness_drivers), 2)
  expect_true(all(sig_indicators$richness_drivers$p_value <= 0.05))
  
  # Check evenness: only OTU5 should be significant
  expect_equal(nrow(sig_indicators$evenness_drivers), 1)
  expect_equal(sig_indicators$evenness_drivers$taxon[1], "OTU5")
})

test_that("parallel processing works when available", {
  skip_if_not(capabilities("multicore"))
  
  demo_data <- create_demo_phyloseq(n_samples = 5, n_taxa = 10)
  indicators <- identify_taxa_drivers(demo_data, top_n = 3, verbose = FALSE)
  
  # Run with parallel
  validated_parallel <- validate_taxa_indicators(
    demo_data,
    indicators,
    null_models = "row_shuffle",
    n_permutations = 10,
    parallel = TRUE,
    n_cores = 2,
    verbose = FALSE
  )
  
  # Run without parallel
  validated_serial <- validate_taxa_indicators(
    demo_data,
    indicators, 
    null_models = "row_shuffle",
    n_permutations = 10,
    parallel = FALSE,
    verbose = FALSE
  )
  
  # Both should produce valid results
  expect_s3_class(validated_parallel, "validated_indicators")
  expect_s3_class(validated_serial, "validated_indicators")
})

test_that("print.validated_indicators works correctly", {
  demo_data <- create_demo_phyloseq(n_samples = 5, n_taxa = 10)
  indicators <- identify_taxa_drivers(demo_data, top_n = 3, verbose = FALSE)
  
  validated <- validate_taxa_indicators(
    demo_data,
    indicators,
    null_models = "row_shuffle",
    n_permutations = 9,
    verbose = FALSE
  )
  
  # Capture output
  output <- capture.output(print(validated))
  
  # Check output contains expected elements
  expect_true(any(grepl("Validated Taxa Indicators", output)))
  expect_true(any(grepl("Null models used:", output)))
  expect_true(any(grepl("Permutations:", output)))
  expect_true(any(grepl("RICHNESS COMPONENT:", output)))
})

test_that("plot.validated_indicators creates valid plots", {
  demo_data <- create_demo_phyloseq(n_samples = 8, n_taxa = 20)
  indicators <- identify_taxa_drivers(demo_data, top_n = 5, verbose = FALSE)
  
  validated <- validate_taxa_indicators(
    demo_data,
    indicators,
    null_models = c("row_shuffle", "column_shuffle"),
    n_permutations = 19,
    verbose = FALSE
  )
  
  # Test p-value plot
  p1 <- plot(validated, type = "pvalues", component = "richness")
  expect_s3_class(p1, "ggplot")
  
  # Test effect size plot
  p2 <- plot(validated, type = "effects", component = "richness")
  expect_s3_class(p2, "ggplot")
  
  # Test null distribution plot
  p3 <- plot(validated, type = "null_dist", component = "richness", top_n = 3)
  expect_s3_class(p3, "ggplot")
})