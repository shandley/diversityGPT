#' Meta-Analysis Tools for diversityGPT
#'
#' Functions for comparing diversity analyses across multiple studies,
#' standardizing metrics between datasets, and performing meta-analyses
#' of universal diversity patterns.

#' Cross-Study Diversity Meta-Analysis
#'
#' Performs comprehensive meta-analysis of diversity patterns across multiple
#' studies using the universal information framework for standardization.
#'
#' @param study_list List of phyloseq objects or universal_information objects
#' @param study_names Optional names for studies (default: Study1, Study2, ...)
#' @param standardize_metrics Whether to standardize metrics across studies
#' @param normalization_method Method for cross-study normalization: "z_score", "min_max", "quantile"
#' @param meta_analysis_method Method for meta-analysis: "fixed_effects", "random_effects", "weighted"
#' @param study_weights Optional weights for each study in meta-analysis
#' @param include_forest_plots Whether to generate forest plots
#' @param include_heterogeneity Whether to assess between-study heterogeneity
#' @param confidence_level Confidence level for meta-analysis (default: 0.95)
#' @param bootstrap_n Number of bootstrap iterations for confidence intervals
#'
#' @return A list containing:
#'   \item{study_summaries}{Summary statistics for each study}
#'   \item{standardized_data}{Standardized diversity metrics across studies}
#'   \item{meta_analysis_results}{Meta-analysis results for each metric}
#'   \item{heterogeneity_assessment}{Between-study heterogeneity measures}
#'   \item{forest_plots}{Forest plot data for visualization}
#'   \item{cross_study_patterns}{Patterns identified across studies}
#'   \item{recommendations}{Recommendations for interpretation}
#'
#' @examples
#' \dontrun{
#' # Load multiple datasets
#' data(GlobalPatterns)
#' study1 <- create_demo_subset(GlobalPatterns, n_samples = 20, seed = 123)
#' study2 <- create_demo_subset(GlobalPatterns, n_samples = 25, seed = 456)
#' study3 <- create_demo_subset(GlobalPatterns, n_samples = 15, seed = 789)
#' 
#' # Perform meta-analysis
#' meta_results <- cross_study_meta_analysis(
#'   study_list = list(study1, study2, study3),
#'   study_names = c("Marine", "Soil", "Gut"),
#'   standardize_metrics = TRUE,
#'   meta_analysis_method = "random_effects",
#'   include_forest_plots = TRUE
#' )
#' 
#' print(meta_results)
#' plot(meta_results)
#' 
#' # Generate meta-analysis report
#' generate_meta_analysis_report(meta_results, "multi_study_analysis")
#' }
#'
#' @export
cross_study_meta_analysis <- function(study_list,
                                    study_names = NULL,
                                    standardize_metrics = TRUE,
                                    normalization_method = "z_score",
                                    meta_analysis_method = "random_effects",
                                    study_weights = NULL,
                                    include_forest_plots = TRUE,
                                    include_heterogeneity = TRUE,
                                    confidence_level = 0.95,
                                    bootstrap_n = 1000) {
  
  # Validate inputs
  if (!is.list(study_list) || length(study_list) < 2) {
    stop("study_list must be a list with at least 2 studies")
  }
  
  if (!normalization_method %in% c("z_score", "min_max", "quantile")) {
    stop("normalization_method must be 'z_score', 'min_max', or 'quantile'")
  }
  
  if (!meta_analysis_method %in% c("fixed_effects", "random_effects", "weighted")) {
    stop("meta_analysis_method must be 'fixed_effects', 'random_effects', or 'weighted'")
  }
  
  n_studies <- length(study_list)
  
  # Generate study names if not provided
  if (is.null(study_names)) {
    study_names <- paste0("Study", 1:n_studies)
  } else if (length(study_names) != n_studies) {
    stop("study_names must have same length as study_list")
  }
  
  cat("Cross-Study Meta-Analysis\n")
  cat("========================\n")
  cat("Number of studies:", n_studies, "\n")
  cat("Standardization:", ifelse(standardize_metrics, normalization_method, "none"), "\n")
  cat("Meta-analysis method:", meta_analysis_method, "\n\n")
  
  # Initialize results
  results <- list(
    study_summaries = list(),
    standardized_data = list(),
    meta_analysis_results = list(),
    heterogeneity_assessment = list(),
    forest_plots = list(),
    cross_study_patterns = list(),
    recommendations = character(),
    metadata = list(
      n_studies = n_studies,
      study_names = study_names,
      standardize_metrics = standardize_metrics,
      normalization_method = normalization_method,
      meta_analysis_method = meta_analysis_method,
      confidence_level = confidence_level,
      timestamp = Sys.time()
    )
  )
  
  # Step 1: Extract or validate universal information for each study
  cat("Step 1: Processing studies...\n")
  universal_info_list <- .extract_universal_info_from_studies(study_list, study_names)
  
  # Step 2: Generate study summaries
  cat("Step 2: Generating study summaries...\n")
  results$study_summaries <- .generate_study_summaries(universal_info_list, study_names)
  
  # Step 3: Standardize metrics across studies
  if (standardize_metrics) {
    cat("Step 3: Standardizing metrics across studies...\n")
    results$standardized_data <- .standardize_cross_study_metrics(
      universal_info_list, study_names, normalization_method
    )
  } else {
    cat("Step 3: Using raw metrics (no standardization)...\n")
    results$standardized_data <- .extract_raw_metrics(universal_info_list, study_names)
  }
  
  # Step 4: Perform meta-analysis
  cat("Step 4: Performing meta-analysis...\n")
  results$meta_analysis_results <- .perform_meta_analysis(
    results$standardized_data, meta_analysis_method, study_weights, confidence_level, bootstrap_n
  )
  
  # Step 5: Assess heterogeneity
  if (include_heterogeneity) {
    cat("Step 5: Assessing between-study heterogeneity...\n")
    results$heterogeneity_assessment <- .assess_heterogeneity(
      results$standardized_data, results$meta_analysis_results
    )
  }
  
  # Step 6: Generate forest plots data
  if (include_forest_plots) {
    cat("Step 6: Preparing forest plot data...\n")
    results$forest_plots <- .prepare_forest_plot_data(
      results$standardized_data, results$meta_analysis_results, study_names
    )
  }
  
  # Step 7: Identify cross-study patterns
  cat("Step 7: Identifying cross-study patterns...\n")
  results$cross_study_patterns <- .identify_cross_study_patterns(
    results$standardized_data, results$meta_analysis_results
  )
  
  # Step 8: Generate recommendations
  cat("Step 8: Generating recommendations...\n")
  results$recommendations <- .generate_meta_analysis_recommendations(results)
  
  # Add class for S3 methods
  class(results) <- c("meta_analysis", "list")
  
  cat("\nMeta-analysis complete!\n")
  cat("Studies analyzed:", n_studies, "\n")
  cat("Components analyzed:", length(results$meta_analysis_results), "\n")
  cat("Significant patterns found:", sum(sapply(results$meta_analysis_results, function(x) x$p_value < 0.05)), "\n")
  
  return(results)
}

# Internal function: Extract universal information from studies
.extract_universal_info_from_studies <- function(study_list, study_names) {
  
  universal_info_list <- list()
  
  pb <- create_progress_tracker(length(study_list), "Processing studies")
  
  for (i in seq_along(study_list)) {
    study <- study_list[[i]]
    study_name <- study_names[i]
    
    if (inherits(study, "universal_information")) {
      # Already processed
      universal_info_list[[study_name]] <- study
    } else if (inherits(study, "phyloseq")) {
      # Extract universal information
      universal_info_list[[study_name]] <- extract_universal_information(study)
    } else {
      stop("Study ", i, " (", study_name, ") must be either phyloseq or universal_information object")
    }
    
    update_progress(pb, i)
  }
  
  finish_progress(pb)
  
  return(universal_info_list)
}

# Internal function: Generate study summaries
.generate_study_summaries <- function(universal_info_list, study_names) {
  
  summaries <- list()
  
  for (study_name in study_names) {
    universal_info <- universal_info_list[[study_name]]
    components <- universal_info$information_components
    
    summary <- list(
      study_name = study_name,
      n_samples = nrow(components),
      n_components = ncol(components),
      component_names = names(components),
      component_means = sapply(components, mean, na.rm = TRUE),
      component_sds = sapply(components, sd, na.rm = TRUE),
      component_ranges = sapply(components, function(x) diff(range(x, na.rm = TRUE))),
      transformation_quality = universal_info$deconvolution_quality$mean_r_squared %||% NA,
      overall_quality = universal_info$deconvolution_quality$overall_quality %||% "Unknown"
    )
    
    summaries[[study_name]] <- summary
  }
  
  return(summaries)
}

# Internal function: Standardize metrics across studies
.standardize_cross_study_metrics <- function(universal_info_list, study_names, method) {
  
  # Extract all components from all studies
  all_components <- list()
  
  for (study_name in study_names) {
    components <- universal_info_list[[study_name]]$information_components
    all_components[[study_name]] <- components
  }
  
  # Find common components across all studies
  all_component_names <- unique(unlist(lapply(all_components, names)))
  
  # Check which components are present in all studies
  common_components <- character()
  for (comp_name in all_component_names) {
    present_in_all <- all(sapply(all_components, function(x) comp_name %in% names(x)))
    if (present_in_all) {
      common_components <- c(common_components, comp_name)
    }
  }
  
  if (length(common_components) == 0) {
    stop("No common components found across all studies")
  }
  
  cat("  Common components found:", paste(common_components, collapse = ", "), "\n")
  
  # Standardize each component across studies
  standardized_data <- list()
  
  for (comp_name in common_components) {
    # Collect values from all studies
    all_values <- unlist(lapply(all_components, function(x) x[[comp_name]]))
    
    # Calculate standardization parameters
    if (method == "z_score") {
      global_mean <- mean(all_values, na.rm = TRUE)
      global_sd <- sd(all_values, na.rm = TRUE)
      
      standardized_values <- lapply(all_components, function(study_comp) {
        (study_comp[[comp_name]] - global_mean) / global_sd
      })
      
    } else if (method == "min_max") {
      global_min <- min(all_values, na.rm = TRUE)
      global_max <- max(all_values, na.rm = TRUE)
      global_range <- global_max - global_min
      
      standardized_values <- lapply(all_components, function(study_comp) {
        (study_comp[[comp_name]] - global_min) / global_range
      })
      
    } else if (method == "quantile") {
      # Quantile normalization
      sorted_values <- sort(all_values, na.rm = TRUE)
      
      standardized_values <- lapply(all_components, function(study_comp) {
        ranks <- rank(study_comp[[comp_name]], na.rm.last = "keep")
        sorted_values[ranks]
      })
    }
    
    names(standardized_values) <- study_names
    standardized_data[[comp_name]] <- standardized_values
  }
  
  return(list(
    standardized_components = standardized_data,
    common_components = common_components,
    standardization_method = method,
    n_studies = length(study_names),
    study_names = study_names
  ))
}

# Internal function: Extract raw metrics (no standardization)
.extract_raw_metrics <- function(universal_info_list, study_names) {
  
  # Extract all components from all studies
  all_components <- list()
  
  for (study_name in study_names) {
    components <- universal_info_list[[study_name]]$information_components
    all_components[[study_name]] <- components
  }
  
  # Find common components
  all_component_names <- unique(unlist(lapply(all_components, names)))
  common_components <- character()
  
  for (comp_name in all_component_names) {
    present_in_all <- all(sapply(all_components, function(x) comp_name %in% names(x)))
    if (present_in_all) {
      common_components <- c(common_components, comp_name)
    }
  }
  
  # Extract raw values
  raw_data <- list()
  
  for (comp_name in common_components) {
    raw_values <- lapply(all_components, function(study_comp) {
      study_comp[[comp_name]]
    })
    names(raw_values) <- study_names
    raw_data[[comp_name]] <- raw_values
  }
  
  return(list(
    standardized_components = raw_data,
    common_components = common_components,
    standardization_method = "none",
    n_studies = length(study_names),
    study_names = study_names
  ))
}

# Internal function: Perform meta-analysis
.perform_meta_analysis <- function(standardized_data, method, study_weights, confidence_level, bootstrap_n) {
  
  meta_results <- list()
  alpha <- 1 - confidence_level
  
  for (comp_name in standardized_data$common_components) {
    comp_data <- standardized_data$standardized_components[[comp_name]]
    
    # Calculate study-level statistics
    study_means <- sapply(comp_data, mean, na.rm = TRUE)
    study_sds <- sapply(comp_data, sd, na.rm = TRUE)
    study_ns <- sapply(comp_data, function(x) sum(!is.na(x)))
    study_ses <- study_sds / sqrt(study_ns)  # Standard errors
    
    # Perform meta-analysis
    if (method == "fixed_effects") {
      meta_result <- .fixed_effects_meta_analysis(study_means, study_ses, confidence_level)
      
    } else if (method == "random_effects") {
      meta_result <- .random_effects_meta_analysis(study_means, study_ses, confidence_level)
      
    } else if (method == "weighted") {
      if (is.null(study_weights)) {
        # Use inverse variance weighting
        weights <- 1 / (study_ses^2)
      } else {
        weights <- study_weights
      }
      meta_result <- .weighted_meta_analysis(study_means, study_ses, weights, confidence_level)
    }
    
    # Add bootstrap confidence intervals
    if (bootstrap_n > 0) {
      bootstrap_ci <- .bootstrap_meta_analysis(comp_data, method, study_weights, confidence_level, bootstrap_n)
      meta_result$bootstrap_ci <- bootstrap_ci
    }
    
    # Add study-level data
    meta_result$study_data <- data.frame(
      study = standardized_data$study_names,
      mean = study_means,
      sd = study_sds,
      se = study_ses,
      n = study_ns,
      stringsAsFactors = FALSE
    )
    
    meta_results[[comp_name]] <- meta_result
  }
  
  return(meta_results)
}

# Internal function: Fixed effects meta-analysis
.fixed_effects_meta_analysis <- function(study_means, study_ses, confidence_level) {
  
  # Inverse variance weighting
  weights <- 1 / (study_ses^2)
  weights[is.infinite(weights) | is.na(weights)] <- 0
  
  # Calculate weighted mean
  if (sum(weights) > 0) {
    pooled_mean <- sum(weights * study_means) / sum(weights)
    pooled_se <- sqrt(1 / sum(weights))
  } else {
    pooled_mean <- mean(study_means, na.rm = TRUE)
    pooled_se <- sd(study_means, na.rm = TRUE) / sqrt(length(study_means))
  }
  
  # Confidence interval
  z_alpha <- qnorm(1 - (1 - confidence_level) / 2)
  ci_lower <- pooled_mean - z_alpha * pooled_se
  ci_upper <- pooled_mean + z_alpha * pooled_se
  
  # Test statistic
  z_stat <- pooled_mean / pooled_se
  p_value <- 2 * (1 - pnorm(abs(z_stat)))
  
  return(list(
    method = "fixed_effects",
    pooled_estimate = pooled_mean,
    standard_error = pooled_se,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    z_statistic = z_stat,
    p_value = p_value,
    weights = weights,
    heterogeneity = list()  # Will be filled by heterogeneity assessment
  ))
}

# Internal function: Random effects meta-analysis
.random_effects_meta_analysis <- function(study_means, study_ses, confidence_level) {
  
  # First perform fixed effects to get Q statistic
  fixed_result <- .fixed_effects_meta_analysis(study_means, study_ses, confidence_level)
  
  # Calculate Q statistic for heterogeneity
  k <- length(study_means)
  weights_fe <- fixed_result$weights
  
  Q <- sum(weights_fe * (study_means - fixed_result$pooled_estimate)^2)
  
  # Estimate between-study variance (tau^2)
  df <- k - 1
  if (Q > df) {
    tau_squared <- (Q - df) / (sum(weights_fe) - sum(weights_fe^2) / sum(weights_fe))
  } else {
    tau_squared <- 0
  }
  
  # Random effects weights
  weights_re <- 1 / (study_ses^2 + tau_squared)
  weights_re[is.infinite(weights_re) | is.na(weights_re)] <- 0
  
  # Calculate random effects estimate
  if (sum(weights_re) > 0) {
    pooled_mean <- sum(weights_re * study_means) / sum(weights_re)
    pooled_se <- sqrt(1 / sum(weights_re))
  } else {
    pooled_mean <- mean(study_means, na.rm = TRUE)
    pooled_se <- sd(study_means, na.rm = TRUE) / sqrt(length(study_means))
  }
  
  # Confidence interval
  z_alpha <- qnorm(1 - (1 - confidence_level) / 2)
  ci_lower <- pooled_mean - z_alpha * pooled_se
  ci_upper <- pooled_mean + z_alpha * pooled_se
  
  # Test statistic
  z_stat <- pooled_mean / pooled_se
  p_value <- 2 * (1 - pnorm(abs(z_stat)))
  
  return(list(
    method = "random_effects",
    pooled_estimate = pooled_mean,
    standard_error = pooled_se,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    z_statistic = z_stat,
    p_value = p_value,
    weights = weights_re,
    tau_squared = tau_squared,
    Q_statistic = Q,
    heterogeneity = list(
      Q = Q,
      df = df,
      p_heterogeneity = 1 - pchisq(Q, df),
      I_squared = max(0, (Q - df) / Q * 100),
      tau_squared = tau_squared
    )
  ))
}

# Internal function: Weighted meta-analysis
.weighted_meta_analysis <- function(study_means, study_ses, weights, confidence_level) {
  
  # Normalize weights
  weights <- weights / sum(weights)
  
  # Calculate weighted mean
  pooled_mean <- sum(weights * study_means)
  
  # Approximate standard error (weighted)
  pooled_se <- sqrt(sum((weights * study_ses)^2))
  
  # Confidence interval
  z_alpha <- qnorm(1 - (1 - confidence_level) / 2)
  ci_lower <- pooled_mean - z_alpha * pooled_se
  ci_upper <- pooled_mean + z_alpha * pooled_se
  
  # Test statistic
  z_stat <- pooled_mean / pooled_se
  p_value <- 2 * (1 - pnorm(abs(z_stat)))
  
  return(list(
    method = "weighted",
    pooled_estimate = pooled_mean,
    standard_error = pooled_se,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    z_statistic = z_stat,
    p_value = p_value,
    weights = weights,
    heterogeneity = list()
  ))
}

# Internal function: Bootstrap meta-analysis confidence intervals
.bootstrap_meta_analysis <- function(comp_data, method, study_weights, confidence_level, bootstrap_n) {
  
  alpha <- 1 - confidence_level
  bootstrap_estimates <- numeric(bootstrap_n)
  
  pb <- create_progress_tracker(bootstrap_n, "Bootstrap meta-analysis")
  
  for (i in 1:bootstrap_n) {
    # Bootstrap resample within each study
    bootstrap_data <- lapply(comp_data, function(study_values) {
      sample(study_values, size = length(study_values), replace = TRUE)
    })
    
    # Calculate study statistics
    study_means <- sapply(bootstrap_data, mean, na.rm = TRUE)
    study_sds <- sapply(bootstrap_data, sd, na.rm = TRUE)
    study_ns <- sapply(bootstrap_data, function(x) sum(!is.na(x)))
    study_ses <- study_sds / sqrt(study_ns)
    
    # Perform meta-analysis
    if (method == "fixed_effects") {
      meta_result <- .fixed_effects_meta_analysis(study_means, study_ses, confidence_level)
    } else if (method == "random_effects") {
      meta_result <- .random_effects_meta_analysis(study_means, study_ses, confidence_level)
    } else if (method == "weighted") {
      weights <- if (is.null(study_weights)) 1 / (study_ses^2) else study_weights
      meta_result <- .weighted_meta_analysis(study_means, study_ses, weights, confidence_level)
    }
    
    bootstrap_estimates[i] <- meta_result$pooled_estimate
    
    if (i %% 100 == 0) update_progress(pb, i)
  }
  
  finish_progress(pb)
  
  # Calculate bootstrap confidence intervals
  ci_lower <- quantile(bootstrap_estimates, alpha / 2, na.rm = TRUE)
  ci_upper <- quantile(bootstrap_estimates, 1 - alpha / 2, na.rm = TRUE)
  
  return(list(
    bootstrap_estimates = bootstrap_estimates,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    bootstrap_mean = mean(bootstrap_estimates, na.rm = TRUE),
    bootstrap_sd = sd(bootstrap_estimates, na.rm = TRUE)
  ))
}

# Internal function: Assess heterogeneity
.assess_heterogeneity <- function(standardized_data, meta_results) {
  
  heterogeneity_results <- list()
  
  for (comp_name in names(meta_results)) {
    meta_result <- meta_results[[comp_name]]
    
    if (meta_result$method == "random_effects") {
      # Heterogeneity already calculated
      heterogeneity_results[[comp_name]] <- meta_result$heterogeneity
    } else {
      # Calculate heterogeneity for other methods
      study_data <- meta_result$study_data
      
      # Q statistic
      weights <- 1 / (study_data$se^2)
      Q <- sum(weights * (study_data$mean - meta_result$pooled_estimate)^2)
      df <- nrow(study_data) - 1
      
      # I-squared
      I_squared <- max(0, (Q - df) / Q * 100)
      
      # Tau-squared (between-study variance)
      if (Q > df) {
        tau_squared <- (Q - df) / (sum(weights) - sum(weights^2) / sum(weights))
      } else {
        tau_squared <- 0
      }
      
      heterogeneity_results[[comp_name]] <- list(
        Q = Q,
        df = df,
        p_heterogeneity = 1 - pchisq(Q, df),
        I_squared = I_squared,
        tau_squared = tau_squared,
        interpretation = .interpret_heterogeneity(I_squared)
      )
    }
  }
  
  return(heterogeneity_results)
}

# Internal function: Interpret heterogeneity
.interpret_heterogeneity <- function(I_squared) {
  
  if (I_squared <= 25) {
    return("Low heterogeneity")
  } else if (I_squared <= 50) {
    return("Moderate heterogeneity")
  } else if (I_squared <= 75) {
    return("Substantial heterogeneity")
  } else {
    return("Considerable heterogeneity")
  }
}

# Internal function: Prepare forest plot data
.prepare_forest_plot_data <- function(standardized_data, meta_results, study_names) {
  
  forest_data <- list()
  
  for (comp_name in names(meta_results)) {
    meta_result <- meta_results[[comp_name]]
    study_data <- meta_result$study_data
    
    # Prepare data for forest plot
    plot_data <- data.frame(
      study = c(study_data$study, "Pooled"),
      estimate = c(study_data$mean, meta_result$pooled_estimate),
      lower_ci = c(study_data$mean - 1.96 * study_data$se, meta_result$ci_lower),
      upper_ci = c(study_data$mean + 1.96 * study_data$se, meta_result$ci_upper),
      weight = c(meta_result$weights / sum(meta_result$weights) * 100, NA),
      is_pooled = c(rep(FALSE, nrow(study_data)), TRUE),
      stringsAsFactors = FALSE
    )
    
    forest_data[[comp_name]] <- plot_data
  }
  
  return(forest_data)
}

# Internal function: Identify cross-study patterns
.identify_cross_study_patterns <- function(standardized_data, meta_results) {
  
  patterns <- list()
  
  # Pattern 1: Consistent effects across studies
  consistent_effects <- character()
  for (comp_name in names(meta_results)) {
    meta_result <- meta_results[[comp_name]]
    
    # Check if confidence interval excludes zero
    excludes_zero <- (meta_result$ci_lower > 0) || (meta_result$ci_upper < 0)
    
    # Check for low heterogeneity (if available)
    low_heterogeneity <- TRUE
    if (!is.null(meta_result$heterogeneity$I_squared)) {
      low_heterogeneity <- meta_result$heterogeneity$I_squared < 50
    }
    
    if (excludes_zero && low_heterogeneity) {
      consistent_effects <- c(consistent_effects, comp_name)
    }
  }
  patterns$consistent_effects <- consistent_effects
  
  # Pattern 2: Components with high between-study variability
  high_variability <- character()
  for (comp_name in names(meta_results)) {
    meta_result <- meta_results[[comp_name]]
    
    if (!is.null(meta_result$heterogeneity$I_squared)) {
      if (meta_result$heterogeneity$I_squared > 75) {
        high_variability <- c(high_variability, comp_name)
      }
    }
  }
  patterns$high_variability <- high_variability
  
  # Pattern 3: Effect sizes
  effect_sizes <- sapply(meta_results, function(x) abs(x$pooled_estimate))
  large_effects <- names(effect_sizes)[effect_sizes > 0.5]  # Arbitrary threshold
  patterns$large_effects <- large_effects
  
  # Pattern 4: Significant meta-analytic effects
  significant_effects <- character()
  for (comp_name in names(meta_results)) {
    if (meta_results[[comp_name]]$p_value < 0.05) {
      significant_effects <- c(significant_effects, comp_name)
    }
  }
  patterns$significant_effects <- significant_effects
  
  return(patterns)
}

# Internal function: Generate meta-analysis recommendations
.generate_meta_analysis_recommendations <- function(results) {
  
  recommendations <- character()
  
  # Number of studies
  n_studies <- results$metadata$n_studies
  if (n_studies < 5) {
    recommendations <- c(recommendations, 
                        "Consider including more studies for more robust meta-analytic conclusions")
  }
  
  # Significant patterns
  n_significant <- length(results$cross_study_patterns$significant_effects)
  if (n_significant > 0) {
    recommendations <- c(recommendations,
                        paste("Focus on", n_significant, "components showing significant cross-study patterns"))
  }
  
  # High heterogeneity
  n_high_var <- length(results$cross_study_patterns$high_variability)
  if (n_high_var > 0) {
    recommendations <- c(recommendations,
                        paste("Investigate sources of heterogeneity for", n_high_var, 
                              "highly variable components"))
  }
  
  # Consistent effects
  n_consistent <- length(results$cross_study_patterns$consistent_effects)
  if (n_consistent > 0) {
    recommendations <- c(recommendations,
                        paste("Exploit", n_consistent, "components showing consistent effects across studies"))
  }
  
  # Study quality
  quality_assessment <- sapply(results$study_summaries, function(x) x$transformation_quality)
  mean_quality <- mean(quality_assessment, na.rm = TRUE)
  if (!is.na(mean_quality) && mean_quality < 0.7) {
    recommendations <- c(recommendations,
                        "Consider data quality issues - mean transformation quality is low")
  }
  
  # Default recommendation
  if (length(recommendations) == 0) {
    recommendations <- "Results suggest robust cross-study patterns suitable for meta-analytic synthesis"
  }
  
  return(recommendations)
}

# S3 method for printing meta-analysis results
#' @export
print.meta_analysis <- function(x, ...) {
  cat("Cross-Study Meta-Analysis Results\n")
  cat("================================\n\n")
  
  cat("Studies analyzed:", x$metadata$n_studies, "\n")
  cat("Study names:", paste(x$metadata$study_names, collapse = ", "), "\n")
  cat("Standardization:", x$metadata$normalization_method, "\n")
  cat("Meta-analysis method:", x$metadata$meta_analysis_method, "\n\n")
  
  # Components analyzed
  cat("Components analyzed:", length(x$meta_analysis_results), "\n")
  for (comp_name in names(x$meta_analysis_results)) {
    result <- x$meta_analysis_results[[comp_name]]
    cat(sprintf("- %s: Estimate = %.3f [%.3f, %.3f], p = %.3f\n",
                comp_name, result$pooled_estimate, result$ci_lower, 
                result$ci_upper, result$p_value))
  }
  cat("\n")
  
  # Cross-study patterns
  if (length(x$cross_study_patterns$significant_effects) > 0) {
    cat("Significant effects:", paste(x$cross_study_patterns$significant_effects, collapse = ", "), "\n")
  }
  
  if (length(x$cross_study_patterns$consistent_effects) > 0) {
    cat("Consistent effects:", paste(x$cross_study_patterns$consistent_effects, collapse = ", "), "\n")
  }
  
  if (length(x$cross_study_patterns$high_variability) > 0) {
    cat("High variability:", paste(x$cross_study_patterns$high_variability, collapse = ", "), "\n")
  }
  
  cat("\nRecommendations:\n")
  for (rec in x$recommendations) {
    cat("- ", rec, "\n")
  }
  
  invisible(x)
}

#' Plot Meta-Analysis Results
#'
#' Create visualizations of meta-analysis results including forest plots
#'
#' @param x Meta-analysis results object
#' @param type Plot type: "forest", "heterogeneity", "effects", "studies"
#' @param component Component to plot (for forest plots)
#' @param ... Additional arguments
#'
#' @export
plot.meta_analysis <- function(x, type = "forest", component = NULL, ...) {
  
  if (type == "forest") {
    .plot_forest_plot(x, component, ...)
  } else if (type == "heterogeneity") {
    .plot_heterogeneity(x, ...)
  } else if (type == "effects") {
    .plot_effect_sizes(x, ...)
  } else if (type == "studies") {
    .plot_study_comparison(x, ...)
  } else {
    warning("Unknown plot type: ", type)
  }
}

# Internal function: Plot forest plot
.plot_forest_plot <- function(x, component, ...) {
  
  # Select component to plot
  if (is.null(component)) {
    component <- names(x$forest_plots)[1]
    message("No component specified, plotting: ", component)
  }
  
  if (!component %in% names(x$forest_plots)) {
    stop("Component '", component, "' not found in results")
  }
  
  plot_data <- x$forest_plots[[component]]
  
  # Simple forest plot using base R
  n_studies <- nrow(plot_data)
  y_pos <- n_studies:1
  
  # Set up plot
  xlim <- range(c(plot_data$lower_ci, plot_data$upper_ci), na.rm = TRUE)
  xlim <- xlim + c(-0.1, 0.1) * diff(xlim)
  
  plot(NULL, xlim = xlim, ylim = c(0.5, n_studies + 0.5),
       xlab = paste("Effect Size (", component, ")"),
       ylab = "", yaxt = "n",
       main = paste("Forest Plot:", component))
  
  # Add vertical line at zero
  abline(v = 0, lty = 2, col = "gray")
  
  # Plot confidence intervals and points
  for (i in 1:n_studies) {
    y <- y_pos[i]
    
    # Confidence interval
    lines(c(plot_data$lower_ci[i], plot_data$upper_ci[i]), c(y, y), lwd = 2)
    
    # Point estimate
    point_col <- if (plot_data$is_pooled[i]) "red" else "blue"
    point_pch <- if (plot_data$is_pooled[i]) 18 else 16
    point_cex <- if (plot_data$is_pooled[i]) 2 else 1.5
    
    points(plot_data$estimate[i], y, pch = point_pch, col = point_col, cex = point_cex)
  }
  
  # Add study labels
  axis(2, at = y_pos, labels = plot_data$study, las = 1, tick = FALSE)
  
  # Add weight information if available
  if (!all(is.na(plot_data$weight))) {
    weight_text <- paste0("(", round(plot_data$weight, 1), "%)")
    weight_text[is.na(plot_data$weight)] <- ""
    text(xlim[2], y_pos, weight_text, adj = 1, cex = 0.8)
  }
}

# Internal function: Plot heterogeneity
.plot_heterogeneity <- function(x, ...) {
  
  if (is.null(x$heterogeneity_assessment)) {
    warning("No heterogeneity assessment available")
    return(invisible(NULL))
  }
  
  # Extract I-squared values
  i_squared_values <- sapply(x$heterogeneity_assessment, function(h) h$I_squared)
  component_names <- names(i_squared_values)
  
  if (length(i_squared_values) == 0) {
    warning("No heterogeneity data to plot")
    return(invisible(NULL))
  }
  
  # Create barplot
  barplot(i_squared_values,
          names.arg = component_names,
          main = "Between-Study Heterogeneity (I²)",
          ylab = "I² (%)",
          ylim = c(0, 100),
          col = ifelse(i_squared_values > 75, "red",
                      ifelse(i_squared_values > 50, "orange", "green")))
  
  # Add interpretation lines
  abline(h = 25, lty = 2, col = "blue")
  abline(h = 50, lty = 2, col = "orange") 
  abline(h = 75, lty = 2, col = "red")
  
  # Add legend
  legend("topright", 
         legend = c("Low (<25%)", "Moderate (25-50%)", "Substantial (50-75%)", "Considerable (>75%)"),
         fill = c("green", "orange", "orange", "red"))
}

# Internal function: Plot effect sizes
.plot_effect_sizes <- function(x, ...) {
  
  # Extract effect sizes
  effect_sizes <- sapply(x$meta_analysis_results, function(r) r$pooled_estimate)
  component_names <- names(effect_sizes)
  
  # Extract confidence intervals
  ci_lower <- sapply(x$meta_analysis_results, function(r) r$ci_lower)
  ci_upper <- sapply(x$meta_analysis_results, function(r) r$ci_upper)
  
  # Create plot
  y_pos <- length(effect_sizes):1
  xlim <- range(c(ci_lower, ci_upper), na.rm = TRUE)
  xlim <- xlim + c(-0.1, 0.1) * diff(xlim)
  
  plot(NULL, xlim = xlim, ylim = c(0.5, length(effect_sizes) + 0.5),
       xlab = "Pooled Effect Size",
       ylab = "", yaxt = "n",
       main = "Meta-Analytic Effect Sizes")
  
  # Add vertical line at zero
  abline(v = 0, lty = 2, col = "gray")
  
  # Plot confidence intervals and points
  for (i in 1:length(effect_sizes)) {
    y <- y_pos[i]
    
    # Confidence interval
    lines(c(ci_lower[i], ci_upper[i]), c(y, y), lwd = 2)
    
    # Point estimate
    points(effect_sizes[i], y, pch = 16, cex = 1.5, col = "blue")
  }
  
  # Add component labels
  axis(2, at = y_pos, labels = component_names, las = 1, tick = FALSE)
}

# Internal function: Plot study comparison
.plot_study_comparison <- function(x, ...) {
  
  # Extract study summaries
  study_names <- names(x$study_summaries)
  n_samples <- sapply(x$study_summaries, function(s) s$n_samples)
  quality <- sapply(x$study_summaries, function(s) s$transformation_quality)
  
  # Create comparison plot
  plot(n_samples, quality,
       pch = 16, cex = 1.5, col = "blue",
       xlab = "Number of Samples",
       ylab = "Transformation Quality",
       main = "Study Comparison")
  
  # Add study labels
  text(n_samples, quality, study_names, pos = 3, cex = 0.8)
  
  # Add trend line if applicable
  if (length(n_samples) > 2) {
    lm_fit <- lm(quality ~ n_samples)
    abline(lm_fit, lty = 2, col = "red")
  }
}

#' Generate Meta-Analysis Report
#'
#' Create a comprehensive report of meta-analysis results
#'
#' @param meta_results Meta-analysis results object
#' @param output_file Output file path
#' @param format Output format: "html" or "pdf"
#'
#' @export
generate_meta_analysis_report <- function(meta_results, output_file = "meta_analysis_report", format = "html") {
  
  cat("Generating meta-analysis report...\n")
  
  # Use the existing report generation framework
  # This would integrate with the report_generation.R functions
  
  # For now, create a simple summary
  report_content <- sprintf('
<!DOCTYPE html>
<html>
<head>
    <title>Meta-Analysis Report</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 40px; }
        .header { background-color: #f0f8ff; padding: 20px; border-radius: 10px; }
        .section { margin: 20px 0; padding: 15px; background-color: #f9f9f9; }
        table { border-collapse: collapse; width: 100%%; }
        th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
        th { background-color: #f2f2f2; }
    </style>
</head>
<body>
    <div class="header">
        <h1>Cross-Study Meta-Analysis Report</h1>
        <p>Generated: %s</p>
        <p>Studies: %d | Method: %s</p>
    </div>
    
    <div class="section">
        <h2>Summary</h2>
        <p>This report presents meta-analytic results from %d studies using the %s method.</p>
        <p>Significant effects found for: %s</p>
    </div>
    
    <div class="section">
        <h2>Recommendations</h2>
        %s
    </div>
    
</body>
</html>',
    Sys.time(),
    meta_results$metadata$n_studies,
    meta_results$metadata$meta_analysis_method,
    meta_results$metadata$n_studies,
    meta_results$metadata$meta_analysis_method,
    paste(meta_results$cross_study_patterns$significant_effects, collapse = ", "),
    paste("<p>", meta_results$recommendations, "</p>", collapse = "")
  )
  
  # Write report
  report_path <- paste0(output_file, ".html")
  writeLines(report_content, report_path)
  
  cat("Meta-analysis report generated:", report_path, "\n")
  return(report_path)
}