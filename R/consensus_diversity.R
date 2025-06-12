#' Consensus analysis across multiple diversity metrics
#'
#' @description
#' This function resolves conflicts between different diversity metrics by
#' applying consensus algorithms. When Shannon diversity shows significance
#' but Simpson doesn't, this function helps determine the overall pattern.
#'
#' @param diversity_results A diversity_results object from calculate_diversity()
#' @param method Character string specifying consensus method:
#'   - "adaptive": Adaptively combines reliability and significance (default)
#'   - "weighted_mean": Weight by reliability (inverse of CV)
#'   - "majority_vote": Based on statistical significance patterns
#'   - "correlation_weighted": Weight by inter-metric correlation
#' @param groups Optional grouping variable for statistical comparisons
#' @param alpha Significance level for statistical tests (default 0.05)
#'
#' @return A list containing:
#'   - consensus_scores: Overall diversity scores per sample
#'   - method_weights: Weights assigned to each metric
#'   - conflict_analysis: Details about metric disagreements
#'   - interpretation: Summary of consensus patterns
#'
#' @export
#' @examples
#' \dontrun{
#' data(example_physeq)
#' div_results <- calculate_diversity(example_physeq, groups = "Group")
#' consensus <- consensus_diversity(div_results, groups = "Group")
#' print(consensus)
#' }
consensus_diversity <- function(diversity_results, 
                              method = c("adaptive", "weighted_mean", "majority_vote", "correlation_weighted"),
                              groups = NULL,
                              alpha = 0.05) {
  
  # Input validation
  if (!inherits(diversity_results, "diversity_results")) {
    cli::cli_abort("Input must be a diversity_results object from calculate_diversity()")
  }
  
  method <- match.arg(method)
  
  # Extract metrics (exclude sample and group columns)
  metric_cols <- attr(diversity_results, "metrics")
  if (is.null(metric_cols)) {
    # Fallback: detect numeric columns that aren't sample or group
    numeric_cols <- sapply(diversity_results, is.numeric)
    metric_cols <- names(diversity_results)[numeric_cols]
  }
  
  if (length(metric_cols) < 2) {
    cli::cli_abort("Need at least 2 diversity metrics for consensus analysis")
  }
  
  cli::cli_alert_info("Running consensus analysis with {length(metric_cols)} metrics")
  
  # Extract metric data
  metric_data <- diversity_results[, metric_cols, drop = FALSE]
  
  # Calculate consensus based on method
  consensus_result <- switch(method,
    adaptive = calculate_adaptive_consensus(metric_data, diversity_results, groups, alpha),
    weighted_mean = calculate_weighted_consensus(metric_data, diversity_results, groups, alpha),
    majority_vote = calculate_majority_consensus(metric_data, diversity_results, groups, alpha),
    correlation_weighted = calculate_correlation_consensus(metric_data, diversity_results, groups, alpha)
  )
  
  # Add metadata
  consensus_result$method <- method
  consensus_result$metrics_used <- metric_cols
  consensus_result$n_samples <- nrow(diversity_results)
  consensus_result$groups <- groups
  
  # Set class
  class(consensus_result) <- c("consensus_results", "list")
  
  return(consensus_result)
}

#' Calculate weighted mean consensus
#' @keywords internal
calculate_weighted_consensus <- function(metric_data, diversity_results, groups, alpha) {
  
  # Calculate reliability weights (inverse of coefficient of variation)
  weights <- apply(metric_data, 2, function(x) {
    cv <- sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)
    1 / (cv + 0.01)  # Add small constant to avoid division by zero
  })
  
  # Normalize weights
  weights <- weights / sum(weights)
  
  # Calculate weighted consensus scores
  consensus_scores <- as.vector(as.matrix(metric_data) %*% weights)
  names(consensus_scores) <- diversity_results$sample
  
  # Analyze conflicts if groups are provided
  conflict_analysis <- NULL
  if (!is.null(groups) && groups %in% names(diversity_results)) {
    conflict_analysis <- analyze_metric_conflicts(metric_data, diversity_results[[groups]], alpha)
  }
  
  # Create interpretation
  interpretation <- generate_consensus_interpretation(weights, conflict_analysis, "weighted_mean")
  
  return(list(
    consensus_scores = consensus_scores,
    method_weights = weights,
    conflict_analysis = conflict_analysis,
    interpretation = interpretation
  ))
}

#' Calculate majority vote consensus
#' @keywords internal
calculate_majority_consensus <- function(metric_data, diversity_results, groups, alpha) {
  
  if (is.null(groups) || !groups %in% names(diversity_results)) {
    cli::cli_warn("Majority vote requires groups for statistical testing. Using weighted mean instead.")
    return(calculate_weighted_consensus(metric_data, diversity_results, groups, alpha))
  }
  
  group_var <- diversity_results[[groups]]
  
  # Test each metric for group differences
  metric_pvalues <- sapply(metric_data, function(x) {
    if (length(unique(group_var)) == 2) {
      # t-test for 2 groups
      tryCatch({
        t.test(x ~ group_var)$p.value
      }, error = function(e) 1.0)
    } else {
      # ANOVA for >2 groups
      tryCatch({
        summary(aov(x ~ group_var))[[1]][["Pr(>F)"]][1]
      }, error = function(e) 1.0)
    }
  })
  
  # Weight by significance (more significant = higher weight)
  significance_weights <- -log10(metric_pvalues + 1e-10)
  significance_weights <- significance_weights / sum(significance_weights)
  
  # Calculate consensus scores
  consensus_scores <- as.vector(as.matrix(metric_data) %*% significance_weights)
  names(consensus_scores) <- diversity_results$sample
  
  # Analyze conflicts
  conflict_analysis <- analyze_metric_conflicts(metric_data, group_var, alpha)
  conflict_analysis$metric_pvalues <- metric_pvalues
  
  # Create interpretation
  interpretation <- generate_consensus_interpretation(significance_weights, conflict_analysis, "majority_vote")
  
  return(list(
    consensus_scores = consensus_scores,
    method_weights = significance_weights,
    conflict_analysis = conflict_analysis,
    interpretation = interpretation
  ))
}

#' Calculate correlation-weighted consensus
#' @keywords internal
calculate_correlation_consensus <- function(metric_data, diversity_results, groups, alpha) {
  
  # Calculate correlation matrix
  cor_matrix <- cor(metric_data, use = "complete.obs")
  
  # Weight by average correlation with other metrics
  avg_correlations <- rowMeans(abs(cor_matrix), na.rm = TRUE)
  correlation_weights <- avg_correlations / sum(avg_correlations)
  
  # Calculate consensus scores
  consensus_scores <- as.vector(as.matrix(metric_data) %*% correlation_weights)
  names(consensus_scores) <- diversity_results$sample
  
  # Analyze conflicts if groups provided
  conflict_analysis <- NULL
  if (!is.null(groups) && groups %in% names(diversity_results)) {
    conflict_analysis <- analyze_metric_conflicts(metric_data, diversity_results[[groups]], alpha)
    conflict_analysis$correlation_matrix <- cor_matrix
  }
  
  # Create interpretation
  interpretation <- generate_consensus_interpretation(correlation_weights, conflict_analysis, "correlation_weighted")
  
  return(list(
    consensus_scores = consensus_scores,
    method_weights = correlation_weights,
    conflict_analysis = conflict_analysis,
    interpretation = interpretation
  ))
}

#' Analyze conflicts between metrics
#' @keywords internal
analyze_metric_conflicts <- function(metric_data, groups, alpha) {
  
  # Test each metric
  results <- data.frame(
    metric = names(metric_data),
    p_value = NA,
    p_value_formatted = NA,
    significant = NA,
    effect_size = NA,
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(metric_data)) {
    metric_name <- names(metric_data)[i]
    metric_values <- metric_data[[i]]
    
    if (length(unique(groups)) == 2) {
      # t-test for 2 groups
      test_result <- tryCatch({
        t.test(metric_values ~ groups)
      }, error = function(e) NULL)
      
      if (!is.null(test_result)) {
        results$p_value[i] <- test_result$p.value
        results$significant[i] <- test_result$p.value < alpha
        # Calculate Cohen's d
        group_means <- tapply(metric_values, groups, mean, na.rm = TRUE)
        pooled_sd <- sqrt(((length(metric_values[groups == unique(groups)[1]]) - 1) * var(metric_values[groups == unique(groups)[1]], na.rm = TRUE) +
                          (length(metric_values[groups == unique(groups)[2]]) - 1) * var(metric_values[groups == unique(groups)[2]], na.rm = TRUE)) /
                         (length(metric_values) - 2))
        results$effect_size[i] <- abs(diff(group_means)) / pooled_sd
      }
    } else {
      # ANOVA for >2 groups
      test_result <- tryCatch({
        aov(metric_values ~ groups)
      }, error = function(e) NULL)
      
      if (!is.null(test_result)) {
        p_val <- summary(test_result)[[1]][["Pr(>F)"]][1]
        results$p_value[i] <- p_val
        results$significant[i] <- p_val < alpha
        # Eta squared as effect size
        ss_total <- sum((metric_values - mean(metric_values, na.rm = TRUE))^2, na.rm = TRUE)
        ss_group <- sum(test_result$residuals^2, na.rm = TRUE)
        results$effect_size[i] <- (ss_total - ss_group) / ss_total
      }
    }
  }
  
  # Format p-values for display
  results$p_value_formatted <- format_p_values(results$p_value)
  
  # Identify conflicts
  n_significant <- sum(results$significant, na.rm = TRUE)
  n_total <- sum(!is.na(results$significant))
  
  conflict_summary <- list(
    n_metrics_significant = n_significant,
    n_metrics_total = n_total,
    proportion_significant = n_significant / n_total,
    has_conflict = n_significant > 0 && n_significant < n_total,
    results = results
  )
  
  return(conflict_summary)
}

#' Calculate adaptive consensus
#' @keywords internal
calculate_adaptive_consensus <- function(metric_data, diversity_results, groups, alpha) {
  
  # Adaptive method: combines reliability and significance weighting
  # First, calculate reliability weights (like weighted_mean)
  reliability_weights <- apply(metric_data, 2, function(x) {
    cv <- sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)
    1 / (cv + 0.01)
  })
  
  # If groups provided, also calculate significance weights
  if (!is.null(groups) && groups %in% names(diversity_results)) {
    group_var <- diversity_results[[groups]]
    
    # Test each metric for group differences
    metric_pvalues <- sapply(metric_data, function(x) {
      if (length(unique(group_var)) == 2) {
        tryCatch({
          t.test(x ~ group_var)$p.value
        }, error = function(e) 1.0)
      } else {
        tryCatch({
          summary(aov(x ~ group_var))[[1]][["Pr(>F)"]][1]
        }, error = function(e) 1.0)
      }
    })
    
    # Weight by significance
    significance_weights <- -log10(metric_pvalues + 1e-10)
    
    # Combine reliability and significance weights adaptively
    # If all p-values are non-significant, use only reliability
    # If some are significant, blend both
    prop_significant <- sum(metric_pvalues < alpha) / length(metric_pvalues)
    
    if (prop_significant > 0) {
      # Blend weights based on proportion significant
      combined_weights <- (1 - prop_significant) * reliability_weights + 
                         prop_significant * significance_weights
    } else {
      combined_weights <- reliability_weights
    }
  } else {
    # No groups, use reliability weights only
    combined_weights <- reliability_weights
  }
  
  # Normalize weights
  combined_weights <- combined_weights / sum(combined_weights)
  
  # Calculate consensus scores
  consensus_scores <- as.vector(as.matrix(metric_data) %*% combined_weights)
  names(consensus_scores) <- diversity_results$sample
  
  # Analyze conflicts if groups are provided
  conflict_analysis <- NULL
  if (!is.null(groups) && groups %in% names(diversity_results)) {
    conflict_analysis <- analyze_metric_conflicts(metric_data, diversity_results[[groups]], alpha)
  }
  
  # Create interpretation
  interpretation <- generate_consensus_interpretation(combined_weights, conflict_analysis, "adaptive")
  
  return(list(
    consensus_scores = consensus_scores,
    method_weights = combined_weights,
    conflict_analysis = conflict_analysis,
    interpretation = interpretation,
    consensus_value = mean(consensus_scores)  # Add mean consensus value
  ))
}

#' Generate interpretation of consensus results
#' @keywords internal
generate_consensus_interpretation <- function(weights, conflict_analysis, method) {
  
  interpretation <- list()
  
  # Weight interpretation
  top_metric <- names(weights)[which.max(weights)]
  interpretation$dominant_metric <- top_metric
  interpretation$weight_distribution <- if (max(weights) > 0.6) "dominated" else "balanced"
  
  # Conflict interpretation
  if (!is.null(conflict_analysis)) {
    if (conflict_analysis$has_conflict) {
      interpretation$conflict_status <- "conflicting"
      interpretation$conflict_details <- paste0(
        conflict_analysis$n_metrics_significant, " of ", conflict_analysis$n_metrics_total,
        " metrics show significant group differences"
      )
    } else {
      interpretation$conflict_status <- if (conflict_analysis$n_metrics_significant == 0) "no_differences" else "consensus"
      interpretation$conflict_details <- "All metrics agree on significance pattern"
    }
  } else {
    interpretation$conflict_status <- "unknown"
    interpretation$conflict_details <- "No group comparisons performed"
  }
  
  # Method-specific interpretation
  interpretation$method_note <- switch(method,
    adaptive = "Adaptively combines reliability and significance for optimal consensus",
    weighted_mean = "Weights based on metric reliability (low variability = high weight)",
    majority_vote = "Weights based on statistical significance strength",
    correlation_weighted = "Weights based on correlation with other metrics",
    "Unknown method"  # Default fallback
  )
  
  return(interpretation)
}

#' Format p-values for display
#' @keywords internal
format_p_values <- function(p_values) {
  formatted <- character(length(p_values))
  
  for (i in seq_along(p_values)) {
    if (is.na(p_values[i])) {
      formatted[i] <- "NA"
    } else if (p_values[i] < 0.001) {
      formatted[i] <- "< 0.001"
    } else if (p_values[i] < 0.01) {
      formatted[i] <- sprintf("%.3f", p_values[i])
    } else if (p_values[i] < 0.05) {
      formatted[i] <- sprintf("%.3f", p_values[i])
    } else {
      formatted[i] <- sprintf("%.3f", p_values[i])
    }
  }
  
  return(formatted)
}

#' Print method for consensus results
#'
#' @param x A consensus_results object
#' @param ... Additional arguments (unused)
#' @export
print.consensus_results <- function(x, ...) {
  cli::cli_h1("Diversity Consensus Analysis")
  cli::cli_text("Method: {x$method}")
  cli::cli_text("Samples: {x$n_samples}")
  cli::cli_text("Metrics: {paste(x$metrics_used, collapse = ', ')}")
  cli::cli_text("")
  
  cli::cli_h2("Consensus Interpretation")
  cli::cli_text("Dominant metric: {x$interpretation$dominant_metric}")
  cli::cli_text("Weight distribution: {x$interpretation$weight_distribution}")
  cli::cli_text("Conflict status: {x$interpretation$conflict_status}")
  cli::cli_text("Details: {x$interpretation$conflict_details}")
  cli::cli_text("")
  
  cli::cli_h2("Metric Weights")
  weights_df <- data.frame(
    Metric = names(x$method_weights),
    Weight = round(x$method_weights, 3)
  )
  print(weights_df)
  
  # Add conflict analysis details if available
  if (!is.null(x$conflict_analysis) && !is.null(x$conflict_analysis$results)) {
    cli::cli_text("")
    cli::cli_h2("Statistical Tests by Metric")
    
    results_df <- x$conflict_analysis$results
    # Use formatted p-values if available, otherwise format them
    if ("p_value_formatted" %in% names(results_df)) {
      p_display <- results_df$p_value_formatted
    } else {
      p_display <- format_p_values(results_df$p_value)
    }
    
    display_df <- data.frame(
      Metric = results_df$metric,
      `P-value` = p_display,
      Significant = ifelse(results_df$significant, "Yes", "No"),
      `Effect Size` = round(results_df$effect_size, 3)
    )
    print(display_df, row.names = FALSE)
  }
  
  cli::cli_text("")
  if (!is.null(x$interpretation$method_note)) {
    cli::cli_text("Note: {x$interpretation$method_note}")
  }
}

#' Summary method for consensus results
#'
#' @param object A consensus_results object
#' @param ... Additional arguments (unused)
#' @export
summary.consensus_results <- function(object, ...) {
  cat("\n=== Diversity Consensus Summary ===\n\n")
  
  # Basic info
  cat("Method:", object$method, "\n")
  cat("Number of samples:", object$n_samples, "\n")
  cat("Metrics analyzed:", paste(object$metrics_used, collapse = ", "), "\n\n")
  
  # Consensus value if available
  if (!is.null(object$consensus_value)) {
    cat("Overall consensus value:", round(object$consensus_value, 3), "\n\n")
  }
  
  # Weights summary
  cat("Metric weights:\n")
  for (i in seq_along(object$method_weights)) {
    cat(sprintf("  %s: %.1f%%\n", 
                names(object$method_weights)[i], 
                object$method_weights[i] * 100))
  }
  
  # Statistical results if groups were provided
  if (!is.null(object$conflict_analysis) && !is.null(object$conflict_analysis$results)) {
    cat("\nStatistical tests:\n")
    results <- object$conflict_analysis$results
    
    # Use formatted p-values
    p_display <- if ("p_value_formatted" %in% names(results)) {
      results$p_value_formatted
    } else {
      format_p_values(results$p_value)
    }
    
    for (i in seq_len(nrow(results))) {
      sig_marker <- if (results$significant[i]) "***" else "   "
      cat(sprintf("  %s: p = %s %s (effect size = %.2f)\n",
                  results$metric[i],
                  p_display[i],
                  sig_marker,
                  results$effect_size[i]))
    }
    
    cat(sprintf("\nConflict analysis: %d of %d metrics significant (%.0f%%)\n",
                object$conflict_analysis$n_metrics_significant,
                object$conflict_analysis$n_metrics_total,
                object$conflict_analysis$proportion_significant * 100))
  }
  
  invisible(object)
}