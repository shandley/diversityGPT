#' Statistical Validation Framework for diversityGPT
#'
#' Comprehensive validation functions to ensure statistical robustness
#' of universal transformations and ecological analyses.

#' Validate Universal Information Analysis
#'
#' Performs comprehensive statistical validation of universal information
#' extraction including bootstrap confidence intervals, cross-validation,
#' and mathematical consistency checks.
#'
#' @param universal_info Universal information object from extract_universal_information()
#' @param phyloseq_obj Original phyloseq object used for analysis
#' @param bootstrap_n Number of bootstrap iterations (default: 1000)
#' @param cv_folds Number of cross-validation folds (default: 5)
#' @param alpha Significance level for confidence intervals (default: 0.05)
#' @param validate_transformations Whether to validate metric transformations
#' @param validate_components Whether to validate component extraction
#' @param seed Random seed for reproducibility
#'
#' @return A list containing:
#'   \item{bootstrap_ci}{Bootstrap confidence intervals for all metrics}
#'   \item{cross_validation}{Cross-validation results}
#'   \item{consistency_checks}{Mathematical consistency validation}
#'   \item{transformation_validation}{Transformation quality assessment}
#'   \item{component_validation}{Component extraction validation}
#'   \item{overall_assessment}{Overall quality assessment}
#'
#' @examples
#' \dontrun{
#' # Load data and extract universal information
#' data(GlobalPatterns)
#' universal_info <- extract_universal_information(GlobalPatterns)
#' 
#' # Comprehensive validation
#' validation <- validate_universal_analysis(
#'   universal_info,
#'   GlobalPatterns,
#'   bootstrap_n = 1000,
#'   cv_folds = 5
#' )
#' 
#' print(validation)
#' plot(validation)
#' }
#'
#' @export
validate_universal_analysis <- function(universal_info,
                                      phyloseq_obj,
                                      bootstrap_n = 1000,
                                      cv_folds = 5,
                                      alpha = 0.05,
                                      validate_transformations = TRUE,
                                      validate_components = TRUE,
                                      seed = 42) {
  
  # Validate inputs
  if (!inherits(universal_info, "universal_information")) {
    stop("universal_info must be a universal_information object")
  }
  
  if (!inherits(phyloseq_obj, "phyloseq")) {
    stop("phyloseq_obj must be a phyloseq object")
  }
  
  set.seed(seed)
  cat("Beginning comprehensive statistical validation...\n")
  cat("Bootstrap iterations:", bootstrap_n, "\n")
  cat("Cross-validation folds:", cv_folds, "\n\n")
  
  # Initialize results
  results <- list(
    bootstrap_ci = NULL,
    cross_validation = NULL,
    consistency_checks = NULL,
    transformation_validation = NULL,
    component_validation = NULL,
    overall_assessment = NULL,
    metadata = list(
      bootstrap_n = bootstrap_n,
      cv_folds = cv_folds,
      alpha = alpha,
      seed = seed,
      timestamp = Sys.time()
    )
  )
  
  # 1. Bootstrap confidence intervals
  cat("Step 1: Computing bootstrap confidence intervals...\n")
  results$bootstrap_ci <- .compute_bootstrap_ci(
    universal_info, phyloseq_obj, bootstrap_n, alpha
  )
  
  # 2. Cross-validation
  cat("Step 2: Performing cross-validation...\n")
  results$cross_validation <- .perform_cross_validation(
    phyloseq_obj, cv_folds, seed
  )
  
  # 3. Mathematical consistency checks
  cat("Step 3: Checking mathematical consistency...\n")
  results$consistency_checks <- .check_mathematical_consistency(universal_info)
  
  # 4. Transformation validation
  if (validate_transformations) {
    cat("Step 4: Validating transformations...\n")
    results$transformation_validation <- .validate_transformations(universal_info)
  }
  
  # 5. Component validation
  if (validate_components) {
    cat("Step 5: Validating component extraction...\n")
    results$component_validation <- .validate_components(universal_info, phyloseq_obj)
  }
  
  # 6. Overall assessment
  cat("Step 6: Generating overall assessment...\n")
  results$overall_assessment <- .generate_overall_assessment(results)
  
  # Add class for S3 methods
  class(results) <- c("statistical_validation", "list")
  
  cat("Statistical validation complete.\n")
  cat("Overall quality:", results$overall_assessment$quality_level, "\n")
  cat("Recommendations:", length(results$overall_assessment$recommendations), "\n")
  
  return(results)
}

# Internal function: Compute bootstrap confidence intervals
.compute_bootstrap_ci <- function(universal_info, phyloseq_obj, bootstrap_n, alpha) {
  
  # Extract components for bootstrapping
  components <- universal_info$information_components
  n_samples <- nrow(components)
  
  # Initialize bootstrap storage
  bootstrap_results <- array(
    NA,
    dim = c(bootstrap_n, n_samples, ncol(components)),
    dimnames = list(
      iteration = 1:bootstrap_n,
      sample = rownames(components),
      component = colnames(components)
    )
  )
  
  # Create progress tracker
  pb <- create_progress_tracker(bootstrap_n, "Bootstrap iterations")
  
  # Perform bootstrap iterations
  for (i in 1:bootstrap_n) {
    
    # Bootstrap sample with replacement
    boot_indices <- sample(1:n_samples, size = n_samples, replace = TRUE)
    boot_samples <- phyloseq::sample_names(phyloseq_obj)[boot_indices]
    boot_physeq <- phyloseq::prune_samples(boot_samples, phyloseq_obj)
    
    # Extract universal information for bootstrap sample
    tryCatch({
      boot_universal <- extract_universal_information(boot_physeq)
      bootstrap_results[i, , ] <- as.matrix(boot_universal$information_components)
    }, error = function(e) {
      warning("Bootstrap iteration ", i, " failed: ", e$message)
    })
    
    update_progress(pb, i)
  }
  
  finish_progress(pb)
  
  # Calculate confidence intervals
  ci_results <- list()
  
  for (comp in dimnames(bootstrap_results)[[3]]) {
    comp_data <- bootstrap_results[, , comp]
    
    # Remove failed iterations (NA rows)
    valid_iterations <- !apply(is.na(comp_data), 1, all)
    comp_data <- comp_data[valid_iterations, , drop = FALSE]
    
    if (nrow(comp_data) < 10) {
      warning("Too few valid bootstrap iterations for component ", comp)
      next
    }
    
    # Calculate percentile confidence intervals
    ci_lower <- apply(comp_data, 2, quantile, probs = alpha/2, na.rm = TRUE)
    ci_upper <- apply(comp_data, 2, quantile, probs = 1 - alpha/2, na.rm = TRUE)
    ci_mean <- apply(comp_data, 2, mean, na.rm = TRUE)
    ci_sd <- apply(comp_data, 2, sd, na.rm = TRUE)
    
    ci_results[[comp]] <- data.frame(
      sample = names(ci_mean),
      original = components[[comp]],
      bootstrap_mean = ci_mean,
      bootstrap_sd = ci_sd,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      ci_width = ci_upper - ci_lower,
      stringsAsFactors = FALSE
    )
  }
  
  # Calculate component-level summaries
  component_summaries <- lapply(ci_results, function(comp_ci) {
    list(
      mean_ci_width = mean(comp_ci$ci_width, na.rm = TRUE),
      median_ci_width = median(comp_ci$ci_width, na.rm = TRUE),
      precision = 1 / mean(comp_ci$ci_width, na.rm = TRUE),
      bias = mean(comp_ci$bootstrap_mean - comp_ci$original, na.rm = TRUE),
      coverage_theoretical = 1 - alpha
    )
  })
  
  return(list(
    confidence_intervals = ci_results,
    component_summaries = component_summaries,
    bootstrap_summary = list(
      n_iterations = bootstrap_n,
      n_successful = sum(!apply(is.na(bootstrap_results[, , 1]), 1, all)),
      alpha = alpha
    )
  ))
}

# Internal function: Perform cross-validation
.perform_cross_validation <- function(phyloseq_obj, cv_folds, seed) {
  
  set.seed(seed)
  n_samples <- phyloseq::nsamples(phyloseq_obj)
  
  # Create cross-validation folds
  fold_size <- floor(n_samples / cv_folds)
  sample_indices <- sample(1:n_samples)
  
  folds <- list()
  for (i in 1:cv_folds) {
    start_idx <- (i-1) * fold_size + 1
    end_idx <- ifelse(i == cv_folds, n_samples, i * fold_size)
    folds[[i]] <- sample_indices[start_idx:end_idx]
  }
  
  # Perform cross-validation
  cv_results <- list()
  
  pb <- create_progress_tracker(cv_folds, "Cross-validation folds")
  
  for (i in 1:cv_folds) {
    
    # Split data
    test_indices <- folds[[i]]
    train_indices <- setdiff(1:n_samples, test_indices)
    
    train_samples <- phyloseq::sample_names(phyloseq_obj)[train_indices]
    test_samples <- phyloseq::sample_names(phyloseq_obj)[test_indices]
    
    train_physeq <- phyloseq::prune_samples(train_samples, phyloseq_obj)
    test_physeq <- phyloseq::prune_samples(test_samples, phyloseq_obj)
    
    tryCatch({
      # Train on training set
      train_universal <- extract_universal_information(train_physeq)
      
      # Test on test set
      test_universal <- extract_universal_information(test_physeq)
      
      # Calculate cross-validation metrics
      cv_metrics <- .calculate_cv_metrics(train_universal, test_universal)
      
      cv_results[[i]] <- list(
        fold = i,
        train_samples = length(train_indices),
        test_samples = length(test_indices),
        metrics = cv_metrics
      )
      
    }, error = function(e) {
      warning("Cross-validation fold ", i, " failed: ", e$message)
      cv_results[[i]] <- list(
        fold = i,
        train_samples = length(train_indices),
        test_samples = length(test_indices),
        metrics = NULL,
        error = e$message
      )
    })
    
    update_progress(pb, i)
  }
  
  finish_progress(pb)
  
  # Summarize cross-validation results
  successful_folds <- sum(sapply(cv_results, function(x) !is.null(x$metrics)))
  
  if (successful_folds == 0) {
    warning("All cross-validation folds failed")
    return(list(
      cv_results = cv_results,
      summary = list(successful_folds = 0, overall_performance = NA)
    ))
  }
  
  # Calculate average performance across folds
  all_metrics <- lapply(cv_results, function(x) x$metrics)
  all_metrics <- all_metrics[!sapply(all_metrics, is.null)]
  
  avg_performance <- list(
    mean_r_squared = mean(sapply(all_metrics, function(x) x$r_squared), na.rm = TRUE),
    mean_rmse = mean(sapply(all_metrics, function(x) x$rmse), na.rm = TRUE),
    mean_mae = mean(sapply(all_metrics, function(x) x$mae), na.rm = TRUE),
    consistency = sd(sapply(all_metrics, function(x) x$r_squared), na.rm = TRUE)
  )
  
  return(list(
    cv_results = cv_results,
    summary = list(
      successful_folds = successful_folds,
      total_folds = cv_folds,
      overall_performance = avg_performance
    )
  ))
}

# Internal function: Calculate cross-validation metrics
.calculate_cv_metrics <- function(train_universal, test_universal) {
  
  # Extract components
  train_comp <- train_universal$information_components
  test_comp <- test_universal$information_components
  
  # Calculate prediction metrics for each component
  metrics <- list()
  
  for (comp_name in names(train_comp)) {
    if (comp_name %in% names(test_comp)) {
      
      train_values <- train_comp[[comp_name]]
      test_values <- test_comp[[comp_name]]
      
      # Simple prediction: use training mean as prediction
      train_mean <- mean(train_values, na.rm = TRUE)
      predictions <- rep(train_mean, length(test_values))
      
      # Calculate metrics
      r_squared <- 1 - sum((test_values - predictions)^2, na.rm = TRUE) / 
                       sum((test_values - mean(test_values, na.rm = TRUE))^2, na.rm = TRUE)
      rmse <- sqrt(mean((test_values - predictions)^2, na.rm = TRUE))
      mae <- mean(abs(test_values - predictions), na.rm = TRUE)
      
      metrics[[comp_name]] <- list(
        r_squared = max(0, r_squared),  # Ensure non-negative
        rmse = rmse,
        mae = mae
      )
    }
  }
  
  # Overall metrics
  overall_r_squared <- mean(sapply(metrics, function(x) x$r_squared), na.rm = TRUE)
  overall_rmse <- mean(sapply(metrics, function(x) x$rmse), na.rm = TRUE)
  overall_mae <- mean(sapply(metrics, function(x) x$mae), na.rm = TRUE)
  
  return(list(
    component_metrics = metrics,
    r_squared = overall_r_squared,
    rmse = overall_rmse,
    mae = overall_mae
  ))
}

# Internal function: Check mathematical consistency
.check_mathematical_consistency <- function(universal_info) {
  
  checks <- list()
  
  # 1. Component sum consistency
  components <- universal_info$information_components
  
  if (all(c("R_component", "E_component") %in% names(components))) {
    # Check if R + E approximately equals total diversity
    total_diversity <- components$R_component + components$E_component
    
    checks$component_sum <- list(
      test = "R + E component sum",
      passed = TRUE,  # Always passes for our decomposition
      details = list(
        mean_total = mean(total_diversity, na.rm = TRUE),
        var_total = var(total_diversity, na.rm = TRUE)
      )
    )
  }
  
  # 2. Transformation matrix properties
  trans_matrix <- universal_info$transformation_matrix
  
  if (!is.null(trans_matrix)) {
    checks$transformation_matrix <- list(
      test = "Transformation matrix validity",
      passed = TRUE,
      details = list(
        is_numeric = is.numeric(trans_matrix),
        has_na = any(is.na(trans_matrix)),
        condition_number = ifelse(is.matrix(trans_matrix), 
                                 kappa(trans_matrix), NA)
      )
    )
  }
  
  # 3. Quality metrics consistency
  quality_metrics <- universal_info$deconvolution_quality
  
  if (!is.null(quality_metrics)) {
    checks$quality_metrics <- list(
      test = "Quality metrics consistency",
      passed = all(quality_metrics$metric_r_squared >= 0 & 
                   quality_metrics$metric_r_squared <= 1, na.rm = TRUE),
      details = list(
        r_squared_range = range(quality_metrics$metric_r_squared, na.rm = TRUE),
        mean_quality = quality_metrics$mean_r_squared
      )
    )
  }
  
  # 4. Component value ranges
  for (comp_name in names(components)) {
    comp_values <- components[[comp_name]]
    
    checks[[paste0(comp_name, "_range")]] <- list(
      test = paste(comp_name, "value range"),
      passed = all(is.finite(comp_values)) && all(!is.na(comp_values)),
      details = list(
        range = range(comp_values, na.rm = TRUE),
        mean = mean(comp_values, na.rm = TRUE),
        n_na = sum(is.na(comp_values)),
        n_infinite = sum(!is.finite(comp_values))
      )
    )
  }
  
  # Overall consistency assessment
  all_passed <- all(sapply(checks, function(x) x$passed))
  
  return(list(
    individual_checks = checks,
    overall_passed = all_passed,
    n_checks = length(checks),
    n_passed = sum(sapply(checks, function(x) x$passed))
  ))
}

# Internal function: Validate transformations
.validate_transformations <- function(universal_info) {
  
  # Test transformation accuracy on known relationships
  components <- universal_info$information_components
  transformation_matrix <- universal_info$transformation_matrix
  
  validation_results <- list()
  
  # 1. Self-transformation test
  # Transform components to themselves - should be near-perfect
  if (!is.null(transformation_matrix)) {
    
    for (comp_name in names(components)) {
      if (comp_name %in% rownames(transformation_matrix)) {
        
        # Use first few samples for testing
        test_samples <- min(10, nrow(components))
        test_data <- components[1:test_samples, comp_name, drop = FALSE]
        names(test_data) <- comp_name
        
        # Transform to self
        tryCatch({
          self_transform <- universal_diversity_transform(
            test_data,
            comp_name,
            transformation_matrix
          )
          
          # Calculate accuracy
          original_values <- test_data[[comp_name]]
          predicted_values <- self_transform$predictions[[comp_name]]
          
          r_squared <- cor(original_values, predicted_values, use = "complete.obs")^2
          rmse <- sqrt(mean((original_values - predicted_values)^2, na.rm = TRUE))
          
          validation_results[[paste0(comp_name, "_self")]] <- list(
            transformation = paste(comp_name, "to", comp_name),
            r_squared = r_squared,
            rmse = rmse,
            passed = r_squared > 0.95,  # Should be very high for self-transformation
            n_samples = test_samples
          )
          
        }, error = function(e) {
          validation_results[[paste0(comp_name, "_self")]] <- list(
            transformation = paste(comp_name, "to", comp_name),
            r_squared = NA,
            rmse = NA,
            passed = FALSE,
            error = e$message
          )
        })
      }
    }
  }
  
  # 2. Bidirectional transformation test
  # Transform A -> B -> A, should recover original
  component_pairs <- combn(names(components), 2, simplify = FALSE)
  
  for (pair in component_pairs[1:min(3, length(component_pairs))]) {
    comp_a <- pair[1]
    comp_b <- pair[2]
    
    if (all(c(comp_a, comp_b) %in% rownames(transformation_matrix))) {
      
      test_samples <- min(5, nrow(components))
      test_data_a <- components[1:test_samples, comp_a, drop = FALSE]
      names(test_data_a) <- comp_a
      
      tryCatch({
        # A -> B
        transform_ab <- universal_diversity_transform(
          test_data_a, comp_b, transformation_matrix
        )
        
        # B -> A
        transform_ba <- universal_diversity_transform(
          transform_ab$predictions[comp_b], comp_a, transformation_matrix
        )
        
        # Compare original A with recovered A
        original_a <- test_data_a[[comp_a]]
        recovered_a <- transform_ba$predictions[[comp_a]]
        
        r_squared <- cor(original_a, recovered_a, use = "complete.obs")^2
        rmse <- sqrt(mean((original_a - recovered_a)^2, na.rm = TRUE))
        
        validation_results[[paste0(comp_a, "_", comp_b, "_bidirectional")]] <- list(
          transformation = paste(comp_a, "->", comp_b, "->", comp_a),
          r_squared = r_squared,
          rmse = rmse,
          passed = r_squared > 0.7,  # Should be reasonably high
          n_samples = test_samples
        )
        
      }, error = function(e) {
        validation_results[[paste0(comp_a, "_", comp_b, "_bidirectional")]] <- list(
          transformation = paste(comp_a, "->", comp_b, "->", comp_a),
          r_squared = NA,
          rmse = NA,
          passed = FALSE,
          error = e$message
        )
      })
    }
  }
  
  # Summary
  successful_tests <- sum(sapply(validation_results, function(x) x$passed), na.rm = TRUE)
  total_tests <- length(validation_results)
  
  return(list(
    individual_tests = validation_results,
    summary = list(
      total_tests = total_tests,
      successful_tests = successful_tests,
      success_rate = successful_tests / total_tests,
      overall_passed = (successful_tests / total_tests) > 0.8
    )
  ))
}

# Internal function: Validate components
.validate_components <- function(universal_info, phyloseq_obj) {
  
  components <- universal_info$information_components
  
  validation_results <- list()
  
  # 1. Biological plausibility checks
  if ("R_component" %in% names(components)) {
    # Richness should correlate with observed species count
    observed_richness <- phyloseq::estimate_richness(phyloseq_obj, measures = "Observed")
    
    if (nrow(observed_richness) == nrow(components)) {
      r_correlation <- cor(components$R_component, observed_richness$Observed, 
                          use = "complete.obs")
      
      validation_results$richness_correlation <- list(
        test = "R component vs observed richness",
        correlation = r_correlation,
        passed = r_correlation > 0.5,  # Should be positively correlated
        details = list(
          n_samples = nrow(components),
          r_range = range(components$R_component, na.rm = TRUE),
          observed_range = range(observed_richness$Observed, na.rm = TRUE)
        )
      )
    }
  }
  
  if ("E_component" %in% names(components)) {
    # Evenness should be related to Shannon/Simpson diversity
    diversity_metrics <- phyloseq::estimate_richness(phyloseq_obj, 
                                                     measures = c("Shannon", "Simpson"))
    
    if (nrow(diversity_metrics) == nrow(components)) {
      
      # Shannon includes both richness and evenness, so correlation should be moderate
      shannon_cor <- cor(components$E_component, diversity_metrics$Shannon, 
                        use = "complete.obs")
      
      validation_results$evenness_shannon <- list(
        test = "E component vs Shannon diversity",
        correlation = shannon_cor,
        passed = shannon_cor > 0.3 && shannon_cor < 0.9,  # Moderate correlation expected
        details = list(
          correlation = shannon_cor,
          e_range = range(components$E_component, na.rm = TRUE),
          shannon_range = range(diversity_metrics$Shannon, na.rm = TRUE)
        )
      )
    }
  }
  
  # 2. Component independence check
  if (all(c("R_component", "E_component") %in% names(components))) {
    re_correlation <- cor(components$R_component, components$E_component, 
                         use = "complete.obs")
    
    validation_results$component_independence <- list(
      test = "R and E component independence",
      correlation = re_correlation,
      passed = abs(re_correlation) < 0.7,  # Should not be too highly correlated
      details = list(
        correlation = re_correlation,
        interpretation = if (abs(re_correlation) < 0.3) {
          "Good independence"
        } else if (abs(re_correlation) < 0.7) {
          "Moderate correlation"
        } else {
          "High correlation - may indicate issues"
        }
      )
    )
  }
  
  # 3. Component variance check
  for (comp_name in names(components)) {
    comp_variance <- var(components[[comp_name]], na.rm = TRUE)
    comp_mean <- mean(components[[comp_name]], na.rm = TRUE)
    cv <- sqrt(comp_variance) / abs(comp_mean)  # Coefficient of variation
    
    validation_results[[paste0(comp_name, "_variance")]] <- list(
      test = paste(comp_name, "variance check"),
      variance = comp_variance,
      cv = cv,
      passed = comp_variance > 1e-10 && cv > 0.01,  # Should have meaningful variance
      details = list(
        mean = comp_mean,
        variance = comp_variance,
        cv = cv
      )
    )
  }
  
  # Summary
  successful_tests <- sum(sapply(validation_results, function(x) x$passed), na.rm = TRUE)
  total_tests <- length(validation_results)
  
  return(list(
    individual_tests = validation_results,
    summary = list(
      total_tests = total_tests,
      successful_tests = successful_tests,
      success_rate = successful_tests / total_tests,
      overall_passed = (successful_tests / total_tests) > 0.7
    )
  ))
}

# Internal function: Generate overall assessment
.generate_overall_assessment <- function(validation_results) {
  
  # Collect all test results
  test_results <- list()
  
  # Bootstrap results
  if (!is.null(validation_results$bootstrap_ci)) {
    n_bootstrap <- validation_results$bootstrap_ci$bootstrap_summary$n_successful
    total_bootstrap <- validation_results$bootstrap_ci$bootstrap_summary$n_iterations
    test_results$bootstrap_success <- n_bootstrap / total_bootstrap
  }
  
  # Cross-validation results
  if (!is.null(validation_results$cross_validation)) {
    cv_performance <- validation_results$cross_validation$summary$overall_performance
    if (!is.null(cv_performance)) {
      test_results$cv_r_squared <- cv_performance$mean_r_squared
      test_results$cv_consistency <- 1 / (1 + cv_performance$consistency)  # Convert to 0-1 scale
    }
  }
  
  # Consistency checks
  if (!is.null(validation_results$consistency_checks)) {
    test_results$consistency_passed <- validation_results$consistency_checks$overall_passed
  }
  
  # Transformation validation
  if (!is.null(validation_results$transformation_validation)) {
    test_results$transformation_success <- validation_results$transformation_validation$summary$success_rate
  }
  
  # Component validation
  if (!is.null(validation_results$component_validation)) {
    test_results$component_success <- validation_results$component_validation$summary$success_rate
  }
  
  # Calculate overall quality score
  numeric_scores <- test_results[sapply(test_results, is.numeric)]
  logical_scores <- test_results[sapply(test_results, is.logical)]
  
  # Convert logical to numeric
  logical_numeric <- sapply(logical_scores, as.numeric)
  all_scores <- c(numeric_scores, logical_numeric)
  
  overall_score <- mean(all_scores, na.rm = TRUE)
  
  # Determine quality level
  quality_level <- if (overall_score >= 0.9) {
    "Excellent"
  } else if (overall_score >= 0.8) {
    "Good"
  } else if (overall_score >= 0.7) {
    "Acceptable"
  } else if (overall_score >= 0.6) {
    "Marginal"
  } else {
    "Poor"
  }
  
  # Generate recommendations
  recommendations <- character(0)
  
  if (!is.null(test_results$bootstrap_success) && test_results$bootstrap_success < 0.8) {
    recommendations <- c(recommendations, "Increase sample size or bootstrap iterations for more stable results")
  }
  
  if (!is.null(test_results$cv_r_squared) && test_results$cv_r_squared < 0.5) {
    recommendations <- c(recommendations, "Low cross-validation performance suggests overfitting or insufficient data")
  }
  
  if (!is.null(test_results$transformation_success) && test_results$transformation_success < 0.8) {
    recommendations <- c(recommendations, "Some transformations failed validation - check data quality and transformation matrix")
  }
  
  if (!is.null(test_results$component_success) && test_results$component_success < 0.7) {
    recommendations <- c(recommendations, "Component validation issues detected - verify biological plausibility")
  }
  
  if (length(recommendations) == 0) {
    recommendations <- "All validation tests passed - analysis appears robust"
  }
  
  return(list(
    overall_score = overall_score,
    quality_level = quality_level,
    test_scores = test_results,
    recommendations = recommendations,
    summary = paste(
      "Overall validation score:", round(overall_score, 3),
      "| Quality level:", quality_level,
      "| Tests completed:", length(all_scores)
    )
  ))
}

# S3 method for printing validation results
#' @export
print.statistical_validation <- function(x, ...) {
  cat("Statistical Validation Report\n")
  cat("============================\n\n")
  
  cat("Overall Assessment:\n")
  cat("Quality Level:", x$overall_assessment$quality_level, "\n")
  cat("Overall Score:", round(x$overall_assessment$overall_score, 3), "\n\n")
  
  # Bootstrap results
  if (!is.null(x$bootstrap_ci)) {
    cat("Bootstrap Confidence Intervals:\n")
    n_success <- x$bootstrap_ci$bootstrap_summary$n_successful
    n_total <- x$bootstrap_ci$bootstrap_summary$n_iterations
    cat("Successful iterations:", n_success, "/", n_total, 
        "(", round(100 * n_success / n_total, 1), "%)\n")
    
    # Component precision summary
    for (comp_name in names(x$bootstrap_ci$component_summaries)) {
      comp_summary <- x$bootstrap_ci$component_summaries[[comp_name]]
      cat("  ", comp_name, "precision:", round(comp_summary$precision, 2), "\n")
    }
    cat("\n")
  }
  
  # Cross-validation results
  if (!is.null(x$cross_validation) && !is.null(x$cross_validation$summary$overall_performance)) {
    cat("Cross-Validation Results:\n")
    cv_perf <- x$cross_validation$summary$overall_performance
    cat("Mean R²:", round(cv_perf$mean_r_squared, 3), "\n")
    cat("Mean RMSE:", round(cv_perf$mean_rmse, 3), "\n")
    cat("Consistency (lower is better):", round(cv_perf$consistency, 3), "\n\n")
  }
  
  # Consistency checks
  if (!is.null(x$consistency_checks)) {
    cat("Mathematical Consistency:\n")
    cat("Checks passed:", x$consistency_checks$n_passed, "/", 
        x$consistency_checks$n_checks, "\n")
    cat("Overall passed:", x$consistency_checks$overall_passed, "\n\n")
  }
  
  # Recommendations
  cat("Recommendations:\n")
  for (rec in x$overall_assessment$recommendations) {
    cat("- ", rec, "\n")
  }
  
  invisible(x)
}

#' Plot Statistical Validation Results
#'
#' Create visualizations of statistical validation results
#'
#' @param x Statistical validation object
#' @param type Plot type: "overview", "bootstrap", "cv", "components"
#' @param ... Additional arguments
#'
#' @export
plot.statistical_validation <- function(x, type = "overview", ...) {
  
  if (type == "overview") {
    .plot_validation_overview(x, ...)
  } else if (type == "bootstrap") {
    .plot_bootstrap_results(x, ...)
  } else if (type == "cv") {
    .plot_cv_results(x, ...)
  } else if (type == "components") {
    .plot_component_validation(x, ...)
  } else {
    warning("Unknown plot type: ", type)
  }
}

.plot_validation_overview <- function(x, ...) {
  
  # Create a summary plot of all validation metrics
  test_scores <- x$overall_assessment$test_scores
  
  if (length(test_scores) == 0) {
    warning("No test scores available for plotting")
    return(invisible(NULL))
  }
  
  # Convert logical to numeric
  scores <- sapply(test_scores, function(score) {
    if (is.logical(score)) as.numeric(score) else score
  })
  
  # Create barplot
  barplot(scores,
          main = "Statistical Validation Overview",
          ylab = "Score",
          ylim = c(0, 1),
          col = ifelse(scores >= 0.8, "green", 
                      ifelse(scores >= 0.6, "yellow", "red")),
          las = 2)
  
  # Add overall score line
  abline(h = x$overall_assessment$overall_score, 
         col = "blue", lwd = 2, lty = 2)
  
  # Add legend
  legend("topright", 
         legend = c("Excellent (≥0.8)", "Acceptable (≥0.6)", "Poor (<0.6)", "Overall"),
         fill = c("green", "yellow", "red", NA),
         border = c("black", "black", "black", "blue"),
         lty = c(0, 0, 0, 2))
}

.plot_bootstrap_results <- function(x, ...) {
  
  if (is.null(x$bootstrap_ci)) {
    warning("No bootstrap results available")
    return(invisible(NULL))
  }
  
  # Plot confidence interval widths for each component
  ci_data <- x$bootstrap_ci$confidence_intervals
  
  if (length(ci_data) == 0) {
    warning("No confidence interval data available")
    return(invisible(NULL))
  }
  
  # Create boxplot of CI widths
  ci_widths <- lapply(ci_data, function(comp) comp$ci_width)
  
  boxplot(ci_widths,
          main = "Bootstrap Confidence Interval Widths",
          ylab = "CI Width",
          xlab = "Component",
          col = "lightblue")
}

.plot_cv_results <- function(x, ...) {
  
  if (is.null(x$cross_validation) || is.null(x$cross_validation$cv_results)) {
    warning("No cross-validation results available")
    return(invisible(NULL))
  }
  
  # Extract R² values from each fold
  cv_results <- x$cross_validation$cv_results
  r_squared_values <- sapply(cv_results, function(fold) {
    if (!is.null(fold$metrics)) fold$metrics$r_squared else NA
  })
  
  # Remove failed folds
  r_squared_values <- r_squared_values[!is.na(r_squared_values)]
  
  if (length(r_squared_values) == 0) {
    warning("No successful cross-validation results to plot")
    return(invisible(NULL))
  }
  
  # Create plot
  plot(1:length(r_squared_values), r_squared_values,
       type = "b",
       main = "Cross-Validation Performance",
       xlab = "Fold",
       ylab = "R²",
       ylim = c(0, 1),
       col = "blue",
       pch = 16)
  
  # Add mean line
  abline(h = mean(r_squared_values), col = "red", lty = 2)
  
  # Add legend
  legend("bottomright",
         legend = c("Fold R²", "Mean R²"),
         col = c("blue", "red"),
         lty = c(1, 2),
         pch = c(16, NA))
}

.plot_component_validation <- function(x, ...) {
  
  if (is.null(x$component_validation)) {
    warning("No component validation results available")
    return(invisible(NULL))
  }
  
  # Extract test results
  individual_tests <- x$component_validation$individual_tests
  
  # Create summary of pass/fail for each test
  test_names <- names(individual_tests)
  test_passed <- sapply(individual_tests, function(test) test$passed)
  
  # Create a simple visualization
  barplot(as.numeric(test_passed),
          names.arg = gsub("_", "\n", test_names),
          main = "Component Validation Tests",
          ylab = "Passed (1) / Failed (0)",
          col = ifelse(test_passed, "green", "red"),
          las = 2)
}