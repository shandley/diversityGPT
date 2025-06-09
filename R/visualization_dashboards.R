#' Information Component Dashboard Visualization
#'
#' @description
#' Creates multi-panel dashboards showing the decomposition of diversity metrics
#' into their universal information components (R, E, P, S). Includes component
#' proportions, metric profiles, and quality assessments.
#'
#' @param universal_info A universal_information object
#' @param metrics Character vector: specific metrics to display (NULL for all)
#' @param plot_type Character: "grid" for static grid, "interactive" for plotly
#' @param n_cols Number of columns for grid layout (default 2)
#' @param color_scheme Character: color palette ("default", "viridis", "RColorBrewer")
#' 
#' @return A multi-panel visualization (ggplot2 or plotly object)
#'
#' @export
#' @examples
#' \dontrun{
#' universal_info <- extract_universal_information(phyloseq_obj)
#' 
#' # Create information dashboard
#' plot_information_components(universal_info)
#' 
#' # Interactive version
#' plot_information_components(universal_info, plot_type = "interactive")
#' }
plot_information_components <- function(universal_info,
                                      metrics = NULL,
                                      plot_type = c("grid", "interactive"),
                                      n_cols = 2,
                                      color_scheme = c("default", "viridis", "RColorBrewer")) {
  
  plot_type <- match.arg(plot_type)
  color_scheme <- match.arg(color_scheme)
  
  # Check for required packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort("Package 'ggplot2' is required. Install with: install.packages('ggplot2')")
  }
  
  if (plot_type == "interactive" && !requireNamespace("plotly", quietly = TRUE)) {
    cli::cli_alert_warning("Package 'plotly' needed for interactive plots. Falling back to static.")
    plot_type <- "grid"
  }
  
  # Extract data
  components <- universal_info$information_components
  trans_matrix <- universal_info$transformation_matrix
  quality <- universal_info$deconvolution_quality
  
  # Select metrics to display
  if (is.null(metrics)) {
    metrics <- trans_matrix$metric
  }
  
  # Create component plots
  plots <- list()
  
  # 1. Component Proportions Stacked Bar Chart
  plots$proportions <- create_component_proportions_plot(components, color_scheme)
  
  # 2. Component Values Heatmap
  plots$heatmap <- create_component_heatmap(components, color_scheme)
  
  # 3. Metric Quality Assessment
  plots$quality <- create_quality_assessment_plot(trans_matrix, quality)
  
  # 4. Component Relationships
  if (!is.null(quality$component_importance)) {
    plots$importance <- create_component_importance_plot(quality$component_importance)
  }
  
  # 5. Sample-wise Component Profiles
  plots$profiles <- create_sample_profiles_plot(components, n_samples = min(10, nrow(components)))
  
  # 6. Component Correlations
  plots$correlations <- create_component_correlation_plot(components)
  
  # Combine plots
  if (plot_type == "grid") {
    dashboard <- create_static_dashboard(plots, n_cols)
  } else {
    dashboard <- create_interactive_dashboard(plots)
  }
  
  return(dashboard)
}

#' Create component proportions stacked bar chart
#' @keywords internal
create_component_proportions_plot <- function(components, color_scheme) {
  
  # Prepare data for plotting
  prop_cols <- c("R_proportion", "E_proportion", "P_proportion", "S_proportion")
  
  # Check if proportion columns exist, if not calculate them
  if (!all(prop_cols %in% names(components))) {
    comp_cols <- c("R_component", "E_component", "P_component", "S_component")
    if (all(comp_cols %in% names(components))) {
      total_info <- rowSums(components[, comp_cols])
      components$R_proportion <- components$R_component / (total_info + 1e-10)
      components$E_proportion <- components$E_component / (total_info + 1e-10)
      components$P_proportion <- components$P_component / (total_info + 1e-10) 
      components$S_proportion <- components$S_component / (total_info + 1e-10)
    }
  }
  
  plot_data <- components[, c("sample_id", prop_cols)]
  
  # Convert to long format
  plot_data_long <- reshape(
    plot_data,
    direction = "long",
    varying = prop_cols,
    v.names = "proportion",
    timevar = "component",
    times = c("R", "E", "P", "S"),
    idvar = "sample_id"
  )
  
  # Set colors
  if (color_scheme == "default") {
    colors <- c(R = "#E74C3C", E = "#3498DB", P = "#2ECC71", S = "#F39C12")
  } else if (color_scheme == "viridis") {
    if (requireNamespace("viridis", quietly = TRUE)) {
      colors <- viridis::viridis(4)
      names(colors) <- c("R", "E", "P", "S")
    } else {
      # Fallback to default if viridis not available
      colors <- c(R = "#E74C3C", E = "#3498DB", P = "#2ECC71", S = "#F39C12")
    }
  } else {
    if (requireNamespace("RColorBrewer", quietly = TRUE)) {
      colors <- RColorBrewer::brewer.pal(4, "Set1")
      names(colors) <- c("R", "E", "P", "S")
    } else {
      # Fallback to default if RColorBrewer not available
      colors <- c(R = "#E74C3C", E = "#3498DB", P = "#2ECC71", S = "#F39C12")
    }
  }
  
  # Create plot
  p <- ggplot2::ggplot(plot_data_long, ggplot2::aes(x = sample_id, y = proportion, fill = component)) +
    ggplot2::geom_bar(stat = "identity", position = "stack") +
    ggplot2::scale_fill_manual(values = colors,
                              labels = c(R = "Richness", E = "Evenness", 
                                       P = "Phylogenetic", S = "Spatial")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(title = "Information Component Proportions by Sample",
                  x = "Sample",
                  y = "Proportion",
                  fill = "Component") +
    ggplot2::coord_flip()
  
  return(p)
}

#' Create component values heatmap
#' @keywords internal
create_component_heatmap <- function(components, color_scheme) {
  
  # Select component columns
  comp_cols <- c("R_component", "E_component", "P_component", "S_component")
  heatmap_data <- as.matrix(components[, comp_cols])
  rownames(heatmap_data) <- components$sample_id
  colnames(heatmap_data) <- c("Richness", "Evenness", "Phylogenetic", "Spatial")
  
  # Scale for better visualization
  heatmap_data_scaled <- scale(heatmap_data)
  
  # Convert to long format for ggplot2
  plot_data <- expand.grid(
    Sample = rownames(heatmap_data_scaled),
    Component = colnames(heatmap_data_scaled)
  )
  plot_data$Value <- as.vector(heatmap_data_scaled)
  
  # Create heatmap
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Component, y = Sample, fill = Value)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                                  midpoint = 0, name = "Scaled\nValue") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(title = "Information Component Values (Scaled)",
                  x = "Component",
                  y = "Sample")
  
  return(p)
}

#' Create quality assessment plot
#' @keywords internal
create_quality_assessment_plot <- function(trans_matrix, quality) {
  
  # Prepare data
  plot_data <- data.frame(
    metric = trans_matrix$metric,
    r_squared = trans_matrix$r_squared,
    quality_category = cut(trans_matrix$r_squared,
                          breaks = c(0, 0.4, 0.6, 0.8, 1.0),
                          labels = c("Poor", "Fair", "Good", "Excellent"),
                          include.lowest = TRUE)
  )
  
  # Sort by R²
  plot_data <- plot_data[order(plot_data$r_squared, decreasing = TRUE), ]
  plot_data$metric <- factor(plot_data$metric, levels = plot_data$metric)
  
  # Create plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = metric, y = r_squared, fill = quality_category)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_fill_manual(values = c(Poor = "#E74C3C", Fair = "#F39C12", 
                                        Good = "#3498DB", Excellent = "#2ECC71")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::geom_hline(yintercept = 0.8, linetype = "dashed", color = "gray50") +
    ggplot2::labs(title = "Transformation Quality by Metric",
                  subtitle = paste("Overall Quality:", quality$overall_quality),
                  x = "Metric",
                  y = "R²",
                  fill = "Quality")
  
  return(p)
}

#' Create component importance plot
#' @keywords internal
create_component_importance_plot <- function(component_importance) {
  
  # Prepare data
  plot_data <- data.frame(
    component = c("Richness", "Evenness", "Phylogenetic", "Spatial"),
    mean_coefficient = component_importance$mean_coefficient,
    contribution_frequency = component_importance$contribution_frequency
  )
  
  # Create plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = component)) +
    ggplot2::geom_bar(ggplot2::aes(y = mean_coefficient), 
                     stat = "identity", fill = "skyblue", alpha = 0.7) +
    ggplot2::geom_line(ggplot2::aes(y = contribution_frequency * max(mean_coefficient), 
                                   group = 1), 
                      color = "red", size = 1.5) +
    ggplot2::geom_point(ggplot2::aes(y = contribution_frequency * max(mean_coefficient)), 
                       color = "red", size = 3) +
    ggplot2::scale_y_continuous(
      name = "Mean Coefficient",
      sec.axis = ggplot2::sec_axis(~./max(plot_data$mean_coefficient), 
                                  name = "Contribution Frequency")
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Component Importance in Transformations",
                  x = "Information Component")
  
  return(p)
}

#' Create sample profiles plot
#' @keywords internal
create_sample_profiles_plot <- function(components, n_samples = 10) {
  
  # Select subset of samples
  sample_indices <- seq_len(min(n_samples, nrow(components)))
  comp_cols <- c("R_component", "E_component", "P_component", "S_component")
  
  plot_data <- components[sample_indices, c("sample_id", comp_cols)]
  
  # Convert to long format
  plot_data_long <- reshape(
    plot_data,
    direction = "long",
    varying = comp_cols,
    v.names = "value",
    timevar = "component",
    times = c("R", "E", "P", "S"),
    idvar = "sample_id"
  )
  
  # Create radar/spider plot data
  p <- ggplot2::ggplot(plot_data_long, 
                      ggplot2::aes(x = component, y = value, 
                                  group = sample_id, color = sample_id)) +
    ggplot2::geom_line(alpha = 0.7) +
    ggplot2::geom_point(size = 2) +
    ggplot2::facet_wrap(~sample_id, ncol = 2) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(title = "Sample Information Profiles",
                  x = "Component",
                  y = "Value") +
    ggplot2::coord_polar()
  
  return(p)
}

#' Create component correlation plot
#' @keywords internal
create_component_correlation_plot <- function(components) {
  
  comp_cols <- c("R_component", "E_component", "P_component", "S_component")
  cor_matrix <- cor(components[, comp_cols], use = "complete.obs")
  
  # Convert to long format
  plot_data <- expand.grid(
    Component1 = colnames(cor_matrix),
    Component2 = colnames(cor_matrix)
  )
  plot_data$Correlation <- as.vector(cor_matrix)
  
  # Create plot
  p <- ggplot2::ggplot(plot_data, 
                      ggplot2::aes(x = Component1, y = Component2, fill = Correlation)) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(ggplot2::aes(label = round(Correlation, 2)), 
                      color = "black", size = 3) +
    ggplot2::scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                                  midpoint = 0, limits = c(-1, 1)) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Component Correlations",
                  x = "",
                  y = "") +
    ggplot2::coord_fixed()
  
  return(p)
}

#' Create static dashboard layout
#' @keywords internal
create_static_dashboard <- function(plots, n_cols) {
  
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    cli::cli_alert_warning("Package 'patchwork' recommended for better layouts. Using base R.")
    
    # Simple grid layout
    n_plots <- length(plots)
    n_rows <- ceiling(n_plots / n_cols)
    
    par(mfrow = c(n_rows, n_cols))
    
    for (plot in plots) {
      print(plot)
    }
    
    par(mfrow = c(1, 1))
    
    return(invisible(plots))
  }
  
  # Use patchwork for nice layouts
  dashboard <- patchwork::wrap_plots(plots, ncol = n_cols) +
    patchwork::plot_annotation(
      title = "Universal Information Component Dashboard",
      subtitle = "Decomposition of Diversity Metrics into R, E, P, S Components"
    )
  
  return(dashboard)
}

#' Create interactive dashboard
#' @keywords internal
create_interactive_dashboard <- function(plots) {
  
  # Convert ggplot objects to plotly
  interactive_plots <- lapply(plots, function(p) {
    if (inherits(p, "ggplot")) {
      plotly::ggplotly(p)
    } else {
      p
    }
  })
  
  # Create subplot layout
  dashboard <- plotly::subplot(
    interactive_plots,
    nrows = ceiling(length(interactive_plots) / 2),
    shareX = FALSE,
    shareY = FALSE,
    titleX = TRUE,
    titleY = TRUE
  )
  
  dashboard <- dashboard %>%
    plotly::layout(
      title = list(
        text = "Universal Information Component Dashboard",
        font = list(size = 20)
      ),
      showlegend = TRUE
    )
  
  return(dashboard)
}

#' Plot transformation quality assessment
#'
#' @description
#' Visualizes the quality of diversity metric transformations, showing
#' R² values, reliability scores, and transformation pathways.
#'
#' @param universal_info A universal_information object
#' @param plot_type Character: "matrix", "network", or "distribution"
#' @param highlight_threshold Numeric: R² threshold to highlight (default 0.8)
#' 
#' @return A visualization of transformation quality
#' @export
plot_transformation_quality <- function(universal_info,
                                      plot_type = c("matrix", "network", "distribution"),
                                      highlight_threshold = 0.8) {
  
  plot_type <- match.arg(plot_type)
  
  trans_matrix <- universal_info$transformation_matrix
  quality <- universal_info$deconvolution_quality
  
  if (plot_type == "matrix") {
    plot <- create_quality_matrix_plot(trans_matrix, highlight_threshold)
  } else if (plot_type == "network") {
    plot <- create_quality_network_plot(trans_matrix, highlight_threshold)
  } else {  # distribution
    plot <- create_quality_distribution_plot(trans_matrix, quality)
  }
  
  return(plot)
}

#' Create quality matrix visualization
#' @keywords internal
create_quality_matrix_plot <- function(trans_matrix, highlight_threshold) {
  
  # Create matrix of R² values
  metrics <- unique(trans_matrix$metric)
  n_metrics <- length(metrics)
  
  quality_matrix <- matrix(NA, n_metrics, n_metrics,
                          dimnames = list(metrics, metrics))
  
  for (i in seq_len(nrow(trans_matrix))) {
    metric <- trans_matrix$metric[i]
    quality_matrix[metric, ] <- trans_matrix$r_squared[i]
  }
  
  # Convert to long format
  plot_data <- expand.grid(
    Source = metrics,
    Target = metrics
  )
  plot_data$R_squared <- as.vector(quality_matrix)
  
  # Create plot
  p <- ggplot2::ggplot(plot_data, 
                      ggplot2::aes(x = Source, y = Target, fill = R_squared)) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(ggplot2::aes(label = ifelse(is.na(R_squared), "", 
                                                   round(R_squared, 2))),
                      color = "black", size = 3) +
    ggplot2::scale_fill_gradient2(low = "red", mid = "yellow", high = "green",
                                  midpoint = 0.6, na.value = "gray90",
                                  limits = c(0, 1)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(title = "Transformation Quality Matrix",
                  subtitle = paste("Highlighted: R² >", highlight_threshold),
                  x = "Source Metric",
                  y = "Target Metric",
                  fill = "R²") +
    ggplot2::coord_fixed()
  
  # Add highlight boxes for high-quality transformations
  high_quality <- plot_data[!is.na(plot_data$R_squared) & 
                           plot_data$R_squared >= highlight_threshold, ]
  
  if (nrow(high_quality) > 0) {
    p <- p + ggplot2::geom_tile(data = high_quality, 
                               color = "black", size = 1, fill = NA)
  }
  
  return(p)
}

#' Plot transformation results
#'
#' @description
#' Visualizes the results of universal diversity transformations, showing
#' predicted vs actual values (if available) and confidence intervals.
#'
#' @param transformation A universal_transformation object
#' @param actual_values Optional named vector of actual values for validation
#' 
#' @return A visualization of transformation results
#' @export
plot_transformation_results <- function(transformation, actual_values = NULL) {
  
  predicted <- transformation$predicted_metrics
  quality <- transformation$transformation_quality
  
  # Prepare data
  plot_data <- data.frame(
    metric = names(predicted)[-1],  # Exclude sample_id
    predicted = as.numeric(predicted[1, -1]),
    stringsAsFactors = FALSE
  )
  
  # Add quality information
  quality_df <- quality$quality_details
  plot_data <- merge(plot_data, quality_df, by = "metric", all.x = TRUE)
  
  # Add actual values if provided
  if (!is.null(actual_values)) {
    plot_data$actual <- actual_values[plot_data$metric]
  }
  
  # Create plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = metric)) +
    ggplot2::geom_bar(ggplot2::aes(y = predicted, fill = reliable), 
                     stat = "identity", alpha = 0.7) +
    ggplot2::scale_fill_manual(values = c("TRUE" = "#2ECC71", "FALSE" = "#E74C3C"),
                              labels = c("TRUE" = "Reliable", "FALSE" = "Unreliable")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  
  if (!is.null(actual_values)) {
    p <- p + ggplot2::geom_point(ggplot2::aes(y = actual), 
                                color = "black", size = 3, shape = 18)
  }
  
  p <- p + ggplot2::labs(
    title = "Universal Diversity Transformation Results",
    subtitle = paste("Overall Quality:", quality$overall_quality,
                    "| Reliable:", quality$reliable_predictions, "/", quality$total_predictions),
    x = "Metric",
    y = "Value",
    fill = "Reliability"
  )
  
  return(p)
}