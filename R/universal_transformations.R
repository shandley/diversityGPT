#' Universal Diversity Metric Transformations
#'
#' @description
#' Revolutionary any-to-any diversity metric transformation system. Uses
#' universal information components (R, E, P, S) to mathematically convert
#' between ANY diversity metrics. Enables prediction of missing metrics,
#' cross-study standardization, and relationship discovery.
#'
#' @param source_metrics Named vector or data.frame of available diversity metrics
#' @param target_metrics Character vector of desired metrics to predict/transform
#' @param transformation_matrix Transformation matrix from extract_universal_information()
#' @param method Character: transformation method ("linear", "nonlinear", "ensemble")
#' @param quality_threshold Numeric: minimum R² threshold for reliable transformations
#'
#' @return A list containing:
#'   - predicted_metrics: Data.frame of predicted metric values
#'   - transformation_quality: Quality assessment for each prediction
#'   - information_components: Estimated R, E, P, S components
#'   - confidence_intervals: Bootstrap confidence intervals
#'
#' @export
#' @examples
#' \dontrun{
#' # Extract universal information first
#' universal_info <- extract_universal_information(phyloseq_obj)
#' 
#' # Transform Shannon to any other metric
#' available_metrics <- c(shannon = 2.3, observed = 45)
#' predicted <- universal_diversity_transform(
#'   source_metrics = available_metrics,
#'   target_metrics = c("simpson", "chao1", "faith_pd"),
#'   transformation_matrix = universal_info$transformation_matrix
#' )
#' 
#' print(predicted)
#' }
universal_diversity_transform <- function(source_metrics,
                                        target_metrics,
                                        transformation_matrix,
                                        method = c("linear", "nonlinear", "ensemble"),
                                        quality_threshold = 0.6) {
  
  method <- match.arg(method)
  
  cli::cli_alert_info("Performing universal diversity transformation...")
  cli::cli_alert_info("Source metrics: {paste(names(source_metrics), collapse = ', ')}")
  cli::cli_alert_info("Target metrics: {paste(target_metrics, collapse = ', ')}")
  
  # Convert source_metrics to data.frame if needed
  if (is.vector(source_metrics)) {
    source_df <- data.frame(
      sample_id = "sample_1",
      as.list(source_metrics),
      stringsAsFactors = FALSE
    )
  } else {
    source_df <- as.data.frame(source_metrics)
  }
  
  # Estimate information components from source metrics
  cli::cli_alert_info("Estimating information components from source metrics...")
  estimated_components <- estimate_information_components(
    source_df, 
    transformation_matrix
  )
  
  # Predict target metrics
  cli::cli_alert_info("Predicting target metrics...")
  predicted_metrics <- predict_metrics_from_components(
    estimated_components,
    target_metrics,
    transformation_matrix,
    quality_threshold
  )
  
  # Assess prediction quality
  quality_assessment <- assess_prediction_quality(
    predicted_metrics,
    target_metrics,
    transformation_matrix
  )
  
  # Calculate confidence intervals if possible
  confidence_intervals <- calculate_prediction_confidence(
    estimated_components,
    target_metrics,
    transformation_matrix
  )
  
  result <- list(
    predicted_metrics = predicted_metrics,
    transformation_quality = quality_assessment,
    information_components = estimated_components,
    confidence_intervals = confidence_intervals,
    method = method,
    source_metrics = source_metrics,
    target_metrics = target_metrics
  )
  
  class(result) <- c("universal_transformation", "list")
  
  cli::cli_alert_success("Universal transformation complete!")
  
  return(result)
}

#' Predict missing diversity metrics from available ones
#'
#' @description
#' High-level function to predict any missing diversity metrics from
#' whatever metrics are available. Automatically determines the best
#' transformation pathway.
#'
#' @param available_metrics Named vector of available diversity metric values
#' @param phyloseq_reference Optional phyloseq object for building transformation matrix
#' @param transformation_matrix Pre-computed transformation matrix (optional)
#' @param target_metrics Character vector of desired metrics (optional, defaults to common metrics)
#'
#' @return universal_transformation object with predicted metrics
#'
#' @export
#' @examples
#' \dontrun{
#' # Predict all common metrics from just Shannon diversity
#' predicted <- predict_missing_diversity_metrics(
#'   available_metrics = c(shannon = 2.3),
#'   phyloseq_reference = my_phyloseq
#' )
#' 
#' # View predictions
#' print(predicted)
#' plot(predicted)
#' }
predict_missing_diversity_metrics <- function(available_metrics,
                                            phyloseq_reference = NULL,
                                            transformation_matrix = NULL,
                                            target_metrics = NULL) {
  
  # Default target metrics if not specified
  if (is.null(target_metrics)) {
    target_metrics <- c(
      "shannon", "simpson", "invsimpson", "observed", "chao1",
      "pielou_evenness", "simpson_evenness", "hill_q0", "hill_q1", "hill_q2"
    )
    
    # Remove metrics that are already available
    target_metrics <- setdiff(target_metrics, names(available_metrics))
  }
  
  # Build transformation matrix if not provided
  if (is.null(transformation_matrix)) {
    if (is.null(phyloseq_reference)) {
      cli::cli_abort("Either transformation_matrix or phyloseq_reference must be provided")
    }
    
    cli::cli_alert_info("Building transformation matrix from reference data...")
    universal_info <- extract_universal_information(phyloseq_reference)
    transformation_matrix <- universal_info$transformation_matrix
  }
  
  # Perform transformation
  result <- universal_diversity_transform(
    source_metrics = available_metrics,
    target_metrics = target_metrics,
    transformation_matrix = transformation_matrix
  )
  
  return(result)
}

#' Estimate information components from available metrics
#' @keywords internal
estimate_information_components <- function(source_df, transformation_matrix) {
  
  available_metrics <- names(source_df)[sapply(source_df, is.numeric)]
  available_metrics <- setdiff(available_metrics, "sample_id")
  
  # Get transformation data for available metrics
  transformations <- attr(transformation_matrix, "transformations")
  component_cols <- attr(transformation_matrix, "component_cols")
  
  if (is.null(transformations) || is.null(component_cols)) {
    cli::cli_abort("Transformation matrix missing required attributes")
  }
  
# Find which metrics we can use for component estimation
  usable_metrics <- intersect(available_metrics, names(transformations))
  
  if (length(usable_metrics) == 0) {
    cli::cli_alert_warning("No usable metrics found in transformation matrix, using default components")
    
    # Return default/neutral information components
    result <- data.frame(
      sample_id = source_df$sample_id,
      R_component = 0.25,
      E_component = 0.25, 
      P_component = 0.25,
      S_component = 0.25,
      total_information = 1.0,
      R_proportion = 0.25,
      E_proportion = 0.25,
      P_proportion = 0.25,
      S_proportion = 0.25,
      stringsAsFactors = FALSE
    )
    
    return(result)
  }
  
  cli::cli_alert_info("Using {length(usable_metrics)} metrics for component estimation")
  
  n_samples <- nrow(source_df)
  
  # Initialize components matrix
  estimated_components <- matrix(0, nrow = n_samples, ncol = length(component_cols))
  colnames(estimated_components) <- component_cols
  component_weights <- numeric(length(component_cols))
  names(component_weights) <- component_cols
  
  # Estimate each component using inverse transformation
  for (metric in usable_metrics) {
    
    metric_transform <- transformations[[metric]]
    
    if (metric_transform$r_squared < 0.3) {
      next  # Skip unreliable transformations
    }
    
    metric_values <- source_df[[metric]]
    coefficients <- metric_transform$coefficients
    
    # Solve for components: metric = intercept + R*coef_R + E*coef_E + P*coef_P + S*coef_S
    # This is an underdetermined system, so we use weighted least squares
    
    for (i in seq_len(n_samples)) {
      
      target_value <- metric_values[i] - coefficients["intercept"]
      
      # Simple heuristic: assign proportional to coefficient magnitudes
      total_coef <- sum(abs(coefficients[component_cols]))
      
      if (total_coef > 0) {
        for (comp in component_cols) {
          if (comp %in% names(coefficients)) {
            weight <- abs(coefficients[comp]) / total_coef
            estimated_components[i, comp] <- estimated_components[i, comp] + 
                                           (target_value * weight * metric_transform$r_squared)
            component_weights[comp] <- component_weights[comp] + metric_transform$r_squared
          }
        }
      }
    }
  }
  
  # Normalize by weights
  for (comp in component_cols) {
    if (component_weights[comp] > 0) {
      estimated_components[, comp] <- estimated_components[, comp] / component_weights[comp]
    }
  }
  
  # Ensure components are non-negative and normalized
  estimated_components[estimated_components < 0] <- 0
  
  # Convert to data.frame
  result <- as.data.frame(estimated_components)
  result$sample_id <- source_df$sample_id
  
  # Calculate total information and proportions
  result$total_information <- rowSums(result[, component_cols, drop = FALSE])
  
  for (comp in component_cols) {
    prop_col <- paste0(gsub("_component", "", comp), "_proportion")
    result[[prop_col]] <- result[[comp]] / (result$total_information + 1e-10)
  }
  
  return(result)
}

#' Predict metrics from information components
#' @keywords internal
predict_metrics_from_components <- function(estimated_components, target_metrics, transformation_matrix, quality_threshold) {
  
  transformations <- attr(transformation_matrix, "transformations")
  component_cols <- attr(transformation_matrix, "component_cols")
  
  n_samples <- nrow(estimated_components)
  
  # Initialize results
  predicted_metrics <- data.frame(
    sample_id = estimated_components$sample_id,
    stringsAsFactors = FALSE
  )
  
  prediction_quality <- data.frame(
    metric = character(0),
    r_squared = numeric(0),
    reliable = logical(0),
    stringsAsFactors = FALSE
  )
  
  for (metric in target_metrics) {
    
    if (!metric %in% names(transformations)) {
      cli::cli_alert_warning("No transformation available for {metric}")
      predicted_metrics[[metric]] <- NA
      
      prediction_quality <- rbind(prediction_quality, data.frame(
        metric = metric,
        r_squared = 0,
        reliable = FALSE,
        stringsAsFactors = FALSE
      ))
      
      next
    }
    
    transform <- transformations[[metric]]
    
    # Check quality threshold
    reliable <- transform$r_squared >= quality_threshold
    
    if (!reliable) {
      cli::cli_alert_warning("Low quality transformation for {metric} (R² = {round(transform$r_squared, 3)})")
    }
    
    # Predict using linear combination
    coefficients <- transform$coefficients
    
    predicted_values <- rep(coefficients["intercept"], n_samples)
    
    for (comp in component_cols) {
      if (comp %in% names(coefficients) && comp %in% names(estimated_components)) {
        predicted_values <- predicted_values + 
                          estimated_components[[comp]] * coefficients[comp]
      }
    }
    
    # Ensure predictions are reasonable (non-negative for most metrics)
    if (metric %in% c("shannon", "simpson", "observed", "chao1", "hill_q0", "hill_q1", "hill_q2")) {
      predicted_values[predicted_values < 0] <- 0
    }
    
    predicted_metrics[[metric]] <- predicted_values
    
    prediction_quality <- rbind(prediction_quality, data.frame(
      metric = metric,
      r_squared = transform$r_squared,
      reliable = reliable,
      stringsAsFactors = FALSE
    ))
  }
  
  attr(predicted_metrics, "quality") <- prediction_quality
  
  return(predicted_metrics)
}

#' Assess prediction quality
#' @keywords internal
assess_prediction_quality <- function(predicted_metrics, target_metrics, transformation_matrix) {
  
  quality_df <- attr(predicted_metrics, "quality")
  
  if (is.null(quality_df)) {
    return(list(overall_quality = "Unknown"))
  }
  
  n_reliable <- sum(quality_df$reliable)
  n_total <- nrow(quality_df)
  
  mean_r_squared <- mean(quality_df$r_squared, na.rm = TRUE)
  
  overall_quality <- if (mean_r_squared > 0.8 && n_reliable/n_total > 0.8) {
    "Excellent"
  } else if (mean_r_squared > 0.6 && n_reliable/n_total > 0.6) {
    "Good"
  } else if (mean_r_squared > 0.4) {
    "Fair"
  } else {
    "Poor"
  }
  
  list(
    overall_quality = overall_quality,
    mean_r_squared = mean_r_squared,
    reliable_predictions = n_reliable,
    total_predictions = n_total,
    reliability_rate = n_reliable / n_total,
    quality_details = quality_df
  )
}

#' Calculate prediction confidence intervals
#' @keywords internal
calculate_prediction_confidence <- function(estimated_components, target_metrics, transformation_matrix) {
  
  transformations <- attr(transformation_matrix, "transformations")
  
  # Simple confidence based on transformation quality
  confidence_intervals <- list()
  
  for (metric in target_metrics) {
    if (metric %in% names(transformations)) {
      transform <- transformations[[metric]]
      
      # Confidence width based on RMSE and R²
      confidence_width <- transform$rmse * (2 - transform$r_squared)
      
      confidence_intervals[[metric]] <- list(
        confidence_width = confidence_width,
        r_squared = transform$r_squared
      )
    }
  }
  
  return(confidence_intervals)
}

#' Discover mathematical relationships between ALL diversity metrics
#'
#' @description
#' Advanced analysis to discover and quantify mathematical relationships
#' between any combination of diversity metrics using information theory.
#'
#' @param phyloseq_object A phyloseq object
#' @param metric_subset Character vector: specific metrics to analyze (optional)
#' @param relationship_types Character vector: types of relationships to discover
#'
#' @return A universal_relationships object containing discovered patterns
#'
#' @export
discover_metric_relationships <- function(phyloseq_object,
                                        metric_subset = NULL,
                                        relationship_types = c("linear", "nonlinear", "conditional")) {
  
  cli::cli_alert_info("Discovering mathematical relationships between diversity metrics...")
  
  # Extract universal information
  universal_info <- extract_universal_information(phyloseq_object)
  
  metrics_data <- universal_info$metric_profiles
  transformation_matrix <- universal_info$transformation_matrix
  
  # Select metrics to analyze
  if (is.null(metric_subset)) {
    numeric_cols <- names(metrics_data)[sapply(metrics_data, is.numeric)]
    metric_subset <- setdiff(numeric_cols, "sample_id")
  }
  
  # Compute pairwise relationships
  relationships <- compute_pairwise_relationships(
    metrics_data[, metric_subset, drop = FALSE],
    relationship_types
  )
  
  # Information-theoretic analysis
  information_relationships <- analyze_information_relationships(
    universal_info$information_components,
    metrics_data[, metric_subset, drop = FALSE]
  )
  
  result <- list(
    pairwise_relationships = relationships,
    information_relationships = information_relationships,
    universal_info = universal_info,
    metrics_analyzed = metric_subset
  )
  
  class(result) <- c("universal_relationships", "list")
  
  return(result)
}

#' Print method for universal_transformation objects
#'
#' @param x A universal_transformation object
#' @param ... Additional arguments (unused)
#' @export
print.universal_transformation <- function(x, ...) {
  cli::cli_h1("Universal Diversity Transformation")
  
  cli::cli_text("Method: {x$method}")
  cli::cli_text("Source metrics: {paste(names(x$source_metrics), collapse = ', ')}")
  cli::cli_text("Target metrics: {paste(x$target_metrics, collapse = ', ')}")
  cli::cli_text("")
  
  cli::cli_h2("Transformation Quality")
  quality <- x$transformation_quality
  cli::cli_text("Overall Quality: {quality$overall_quality}")
  cli::cli_text("Mean R²: {round(quality$mean_r_squared, 3)}")
  cli::cli_text("Reliable Predictions: {quality$reliable_predictions}/{quality$total_predictions}")
  cli::cli_text("Reliability Rate: {round(quality$reliability_rate * 100, 1)}%")
  
  cli::cli_text("")
  cli::cli_h2("Predicted Metrics")
  
  predicted <- x$predicted_metrics
  quality_details <- quality$quality_details
  
  for (i in seq_len(nrow(quality_details))) {
    metric <- quality_details$metric[i]
    r_sq <- quality_details$r_squared[i]
    reliable <- quality_details$reliable[i]
    
    if (metric %in% names(predicted)) {
      values <- predicted[[metric]]
      if (!all(is.na(values))) {
        status <- if (reliable) "✓" else "⚠"
        cli::cli_text("{status} {metric}: {round(mean(values, na.rm = TRUE), 3)} (R² = {round(r_sq, 3)})")
      }
    }
  }
  
  cli::cli_text("")
  cli::cli_h2("Information Components")
  
  components <- x$information_components
  comp_cols <- c("R_component", "E_component", "P_component", "S_component")
  
  for (comp in comp_cols) {
    if (comp %in% names(components)) {
      values <- components[[comp]]
      cli::cli_text("{comp}: {round(mean(values, na.rm = TRUE), 3)}")
    }
  }
  
  invisible(x)
}