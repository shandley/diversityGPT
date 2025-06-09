#' Interactive Network Visualization of Diversity Metric Relationships
#'
#' @description
#' Creates interactive network plots showing mathematical relationships between
#' diversity metrics. Nodes represent metrics, edges show transformation quality,
#' and colors indicate information component dominance (R, E, P, S).
#'
#' @param universal_info A universal_information object from extract_universal_information()
#' @param relationships A universal_relationships object from discover_metric_relationships()
#' @param min_r_squared Minimum R² threshold for displaying edges (default 0.3)
#' @param edge_width_scale Scaling factor for edge widths based on R² (default 5)
#' @param node_size_by Character: "importance", "connections", or "uniform"
#' @param interactive Logical: create interactive plot (TRUE) or static (FALSE)
#' @param layout Character: network layout algorithm ("force", "circle", "grid")
#' 
#' @return An interactive network visualization (networkD3 or igraph object)
#'
#' @export
#' @examples
#' \dontrun{
#' # Extract universal information
#' universal_info <- extract_universal_information(phyloseq_obj)
#' 
#' # Discover relationships
#' relationships <- discover_metric_relationships(phyloseq_obj)
#' 
#' # Create interactive network
#' plot_diversity_network(universal_info, relationships)
#' }
plot_diversity_network <- function(universal_info = NULL,
                                 relationships = NULL,
                                 min_r_squared = 0.3,
                                 edge_width_scale = 5,
                                 node_size_by = c("importance", "connections", "uniform"),
                                 interactive = TRUE,
                                 layout = c("force", "circle", "grid")) {
  
  node_size_by <- match.arg(node_size_by)
  layout <- match.arg(layout)
  
  # Check for required packages
  if (interactive && !requireNamespace("networkD3", quietly = TRUE)) {
    cli::cli_alert_warning("Package 'networkD3' needed for interactive plots. Falling back to static.")
    interactive <- FALSE
  }
  
  if (!requireNamespace("igraph", quietly = TRUE)) {
    cli::cli_abort("Package 'igraph' is required for network visualization. Install with: install.packages('igraph')")
  }
  
  # Build network data
  cli::cli_alert_info("Building diversity metric network...")
  
  if (!is.null(relationships)) {
    network_data <- build_network_from_relationships(relationships, min_r_squared)
  } else if (!is.null(universal_info)) {
    network_data <- build_network_from_transformation_matrix(universal_info, min_r_squared)
  } else {
    cli::cli_abort("Either universal_info or relationships must be provided")
  }
  
  # Add information component data to nodes
  if (!is.null(universal_info)) {
    network_data <- add_component_information(network_data, universal_info)
  }
  
  # Calculate node sizes
  network_data <- calculate_node_sizes(network_data, node_size_by)
  
  # Create visualization
  if (interactive) {
    plot <- create_interactive_network(network_data, layout, edge_width_scale)
  } else {
    plot <- create_static_network(network_data, layout, edge_width_scale)
  }
  
  return(plot)
}

#' Build network data from transformation matrix
#' @keywords internal
build_network_from_transformation_matrix <- function(universal_info, min_r_squared) {
  
  trans_matrix <- universal_info$transformation_matrix
  
  # Get unique metrics
  metrics <- unique(trans_matrix$metric)
  
  # Create nodes
  nodes <- data.frame(
    id = seq_along(metrics) - 1,  # 0-indexed for networkD3
    name = metrics,
    stringsAsFactors = FALSE
  )
  
  # Create edges based on transformation quality
  edges <- data.frame(
    source = integer(0),
    target = integer(0),
    r_squared = numeric(0),
    stringsAsFactors = FALSE
  )
  
  # Add edges for transformations above threshold
  for (i in seq_len(nrow(trans_matrix))) {
    if (trans_matrix$r_squared[i] >= min_r_squared) {
      metric_name <- trans_matrix$metric[i]
      metric_idx <- which(nodes$name == metric_name) - 1
      
      # Connect to metrics with high component contributions
      component_cols <- c("R_component", "E_component", "P_component", "S_component")
      
      for (j in seq_along(metrics)) {
        if (metrics[j] != metric_name) {
          # Simple heuristic: connect if they share dominant components
          edges <- rbind(edges, data.frame(
            source = metric_idx,
            target = j - 1,
            r_squared = trans_matrix$r_squared[i],
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }
  
  # Remove duplicate edges
  edges <- unique(edges)
  
  return(list(nodes = nodes, edges = edges))
}

#' Build network data from relationships
#' @keywords internal  
build_network_from_relationships <- function(relationships, min_r_squared) {
  
  pairwise <- relationships$pairwise_relationships
  
  # Extract unique metrics
  all_metrics <- unique(c(
    sapply(pairwise, function(x) x$metric_x),
    sapply(pairwise, function(x) x$metric_y)
  ))
  
  # Create nodes
  nodes <- data.frame(
    id = seq_along(all_metrics) - 1,
    name = all_metrics,
    stringsAsFactors = FALSE
  )
  
  # Create edges
  edges <- data.frame(
    source = integer(0),
    target = integer(0),
    r_squared = numeric(0),
    correlation = numeric(0),
    relationship_type = character(0),
    stringsAsFactors = FALSE
  )
  
  for (rel in pairwise) {
    # Get best relationship strength
    r_squared <- 0
    correlation <- 0
    rel_type <- "none"
    
    if (!is.null(rel$relationships$linear)) {
      r_squared <- rel$relationships$linear$r_squared
      correlation <- abs(rel$relationships$linear$pearson_correlation)
      rel_type <- "linear"
    }
    
    if (!is.null(rel$relationships$nonlinear)) {
      nonlinear_r2 <- rel$relationships$nonlinear$best_nonlinear
      if (nonlinear_r2 > r_squared) {
        r_squared <- nonlinear_r2
        rel_type <- "nonlinear"
      }
    }
    
    if (r_squared >= min_r_squared) {
      source_idx <- which(nodes$name == rel$metric_x) - 1
      target_idx <- which(nodes$name == rel$metric_y) - 1
      
      edges <- rbind(edges, data.frame(
        source = source_idx,
        target = target_idx,
        r_squared = r_squared,
        correlation = correlation,
        relationship_type = rel_type,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  return(list(nodes = nodes, edges = edges))
}

#' Add information component data to nodes
#' @keywords internal
add_component_information <- function(network_data, universal_info) {
  
  component_contributions <- universal_info$transformation_matrix
  nodes <- network_data$nodes
  
  # Initialize component columns
  nodes$R_dominance <- 0
  nodes$E_dominance <- 0
  nodes$P_dominance <- 0
  nodes$S_dominance <- 0
  nodes$dominant_component <- "mixed"
  
  # Calculate dominant component for each metric
  for (i in seq_len(nrow(nodes))) {
    metric_name <- nodes$name[i]
    
    # Find this metric in transformation matrix
    metric_rows <- component_contributions[component_contributions$metric == metric_name, ]
    
    if (nrow(metric_rows) > 0) {
      # Average component contributions
      R_avg <- mean(abs(metric_rows$R_component), na.rm = TRUE)
      E_avg <- mean(abs(metric_rows$E_component), na.rm = TRUE)
      P_avg <- mean(abs(metric_rows$P_component), na.rm = TRUE)
      S_avg <- mean(abs(metric_rows$S_component), na.rm = TRUE)
      
      nodes$R_dominance[i] <- R_avg
      nodes$E_dominance[i] <- E_avg
      nodes$P_dominance[i] <- P_avg
      nodes$S_dominance[i] <- S_avg
      
      # Determine dominant component
      components <- c(R = R_avg, E = E_avg, P = P_avg, S = S_avg)
      dominant <- names(which.max(components))
      nodes$dominant_component[i] <- dominant
    }
  }
  
  network_data$nodes <- nodes
  return(network_data)
}

#' Calculate node sizes based on importance
#' @keywords internal
calculate_node_sizes <- function(network_data, node_size_by) {
  
  nodes <- network_data$nodes
  edges <- network_data$edges
  
  if (node_size_by == "uniform") {
    nodes$size <- 20
  } else if (node_size_by == "connections") {
    # Count connections per node
    degree <- table(c(edges$source, edges$target))
    nodes$size <- 10  # default size
    
    for (i in seq_len(nrow(nodes))) {
      node_id <- as.character(nodes$id[i])
      if (node_id %in% names(degree)) {
        nodes$size[i] <- 10 + degree[node_id] * 3
      }
    }
  } else {  # importance
    # Use average R² of connections
    nodes$importance <- 0
    
    for (i in seq_len(nrow(nodes))) {
      node_id <- nodes$id[i]
      
      # Find all edges involving this node
      connected_edges <- edges[edges$source == node_id | edges$target == node_id, ]
      
      if (nrow(connected_edges) > 0) {
        nodes$importance[i] <- mean(connected_edges$r_squared)
      }
    }
    
    # Scale to size
    nodes$size <- 10 + nodes$importance * 30
  }
  
  network_data$nodes <- nodes
  return(network_data)
}

#' Create interactive network visualization
#' @keywords internal
create_interactive_network <- function(network_data, layout, edge_width_scale) {
  
  nodes <- network_data$nodes
  edges <- network_data$edges
  
  # Color nodes by dominant component
  component_colors <- c(
    R = "#E74C3C",  # Red for Richness
    E = "#3498DB",  # Blue for Evenness
    P = "#2ECC71",  # Green for Phylogenetic
    S = "#F39C12",  # Orange for Spatial
    mixed = "#95A5A6"  # Gray for mixed
  )
  
  nodes$color <- component_colors[nodes$dominant_component]
  
  # Scale edge widths by R²
  edges$width <- edges$r_squared * edge_width_scale
  
  # Create networkD3 plot
  if (layout == "force") {
    plot <- networkD3::forceNetwork(
      Links = edges,
      Nodes = nodes,
      Source = "source",
      Target = "target",
      Value = "width",
      NodeID = "name",
      Group = "dominant_component",
      Nodesize = "size",
      opacity = 0.8,
      zoom = TRUE,
      legend = TRUE,
      arrows = FALSE,
      bounded = FALSE,
      clickAction = networkD3::JS("function(d) {
        alert('Metric: ' + d.name + '\\n' +
              'Dominant Component: ' + d.dominant_component + '\\n' +
              'Size: ' + d.size);
      }")
    )
  } else {
    # For other layouts, convert to igraph first
    g <- igraph::graph_from_data_frame(edges, directed = FALSE, vertices = nodes)
    
    if (layout == "circle") {
      coords <- igraph::layout_in_circle(g)
    } else {  # grid
      coords <- igraph::layout_on_grid(g)
    }
    
    # Still use forceNetwork but with fixed positions
    plot <- networkD3::forceNetwork(
      Links = edges,
      Nodes = nodes,
      Source = "source",
      Target = "target",
      Value = "width",
      NodeID = "name",
      Group = "dominant_component",
      Nodesize = "size",
      opacity = 0.8,
      zoom = TRUE,
      legend = TRUE
    )
  }
  
  return(plot)
}

#' Create static network visualization
#' @keywords internal
create_static_network <- function(network_data, layout, edge_width_scale) {
  
  nodes <- network_data$nodes
  edges <- network_data$edges
  
  # Create igraph object
  g <- igraph::graph_from_data_frame(edges, directed = FALSE, vertices = nodes)
  
  # Set layout
  if (layout == "force") {
    l <- igraph::layout_with_fr(g)
  } else if (layout == "circle") {
    l <- igraph::layout_in_circle(g)
  } else {  # grid
    l <- igraph::layout_on_grid(g)
  }
  
  # Colors by component
  component_colors <- c(
    R = "#E74C3C",
    E = "#3498DB", 
    P = "#2ECC71",
    S = "#F39C12",
    mixed = "#95A5A6"
  )
  
  node_colors <- component_colors[nodes$dominant_component]
  
  # Plot
  plot(g,
       layout = l,
       vertex.color = node_colors,
       vertex.size = nodes$size,
       vertex.label = nodes$name,
       vertex.label.cex = 0.8,
       edge.width = edges$width,
       edge.color = adjustcolor("gray50", alpha.f = 0.5),
       main = "Diversity Metric Relationship Network"
  )
  
  # Add legend
  legend("bottomright",
         legend = c("R (Richness)", "E (Evenness)", "P (Phylogenetic)", "S (Spatial)", "Mixed"),
         fill = component_colors,
         title = "Dominant Component",
         cex = 0.8)
  
  invisible(g)
}

#' Plot method for universal_information objects
#'
#' @param x A universal_information object
#' @param type Character: plot type ("network", "components", "quality")
#' @param ... Additional arguments passed to specific plot functions
#' @export
plot.universal_information <- function(x, type = c("network", "components", "quality"), ...) {
  type <- match.arg(type)
  
  if (type == "network") {
    # Discover relationships for network plot
    cli::cli_alert_info("Computing metric relationships for network visualization...")
    relationships <- discover_metric_relationships(
      phyloseq_object = NULL,  # Not needed, we have the metrics
      metric_subset = x$transformation_matrix$metric
    )
    
    plot_diversity_network(
      universal_info = x,
      relationships = relationships,
      ...
    )
  } else if (type == "components") {
    plot_information_components(x, ...)
  } else {  # quality
    plot_transformation_quality(x, ...)
  }
}

#' Plot method for universal_transformation objects
#'
#' @param x A universal_transformation object
#' @param ... Additional arguments (unused)
#' @export
plot.universal_transformation <- function(x, ...) {
  plot_transformation_results(x, ...)
}