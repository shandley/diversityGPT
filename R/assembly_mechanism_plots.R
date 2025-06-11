#' Assembly Mechanism Visualization Functions
#'
#' Enhanced visualization functions for community assembly mechanism detection results
#'
#' @name assembly_mechanism_plots
#' @import ggplot2
#' @import igraph
#' @importFrom plotly ggplotly plot_ly
NULL

#' Plot Assembly Mechanisms
#'
#' S3 plot method for assembly_mechanisms objects with multiple visualization types
#'
#' @param x An assembly_mechanisms object from detect_assembly_mechanisms()
#' @param type Plot type: "summary", "evidence", "network", "confidence", or "all"
#' @param interactive Create interactive plot using plotly (default: FALSE)
#' @param colors Custom color palette for mechanisms
#' @param ... Additional arguments passed to plotting functions
#'
#' @return A ggplot2 or plotly object (or list of plots if type = "all")
#' @export
#' @examples
#' # Create example data
#' physeq <- create_demo_phyloseq()
#' universal_info <- extract_universal_information(physeq)
#' mechanisms <- detect_assembly_mechanisms(universal_info)
#' 
#' # Summary plot
#' plot(mechanisms, type = "summary")
#' 
#' # Interactive confidence plot
#' plot(mechanisms, type = "confidence", interactive = TRUE)
plot.assembly_mechanisms <- function(x, 
                                   type = c("summary", "evidence", "network", 
                                           "confidence", "all"),
                                   interactive = FALSE,
                                   colors = NULL,
                                   ...) {
  
  type <- match.arg(type)
  
  # Default color palette
  if (is.null(colors)) {
    colors <- c(
      "Environmental Filtering" = "#E74C3C",
      "Competitive Exclusion" = "#3498DB", 
      "Neutral Drift" = "#2ECC71",
      "Dispersal Limitation" = "#F39C12",
      "Phylogenetic Clustering" = "#9B59B6"
    )
  }
  
  if (type == "all") {
    # Return all plot types
    plots <- list(
      summary = plot_mechanism_summary(x, colors, interactive),
      evidence = plot_mechanism_evidence(x, colors, interactive),
      network = plot_mechanism_network(x, colors, interactive),
      confidence = plot_mechanism_confidence(x, colors, interactive)
    )
    return(plots)
  }
  
  # Single plot type
  switch(type,
    summary = plot_mechanism_summary(x, colors, interactive),
    evidence = plot_mechanism_evidence(x, colors, interactive),
    network = plot_mechanism_network(x, colors, interactive),
    confidence = plot_mechanism_confidence(x, colors, interactive)
  )
}

#' Plot Assembly Mechanism Summary
#'
#' Creates a summary visualization of detected mechanisms
#'
#' @param mechanisms Assembly mechanisms object
#' @param colors Color palette
#' @param interactive Create interactive plot
#'
#' @return ggplot2 or plotly object
#' @keywords internal
plot_mechanism_summary <- function(mechanisms, colors, interactive = FALSE) {
  
  # Extract mechanism data - handle both old and new formats
  if (is.data.frame(mechanisms$mechanisms)) {
    # New format: mechanisms is a data frame
    mech_data <- mechanisms$mechanisms
    mech_data$detected <- mech_data$confidence > 0.5  # Detection threshold
  } else {
    # Old format: mechanisms is a list
    mech_data <- data.frame(
      mechanism = character(),
      confidence = numeric(),
      detected = logical(),
      stringsAsFactors = FALSE
    )
    
    for (mech_name in names(mechanisms$mechanisms)) {
      mech <- mechanisms$mechanisms[[mech_name]]
      mech_data <- rbind(mech_data, data.frame(
        mechanism = mech_name,
        confidence = mech$confidence,
        detected = mech$detected,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Clean mechanism names for display
  mech_data$mechanism_clean <- gsub("_", " ", mech_data$mechanism)
  mech_data$mechanism_clean <- tools::toTitleCase(mech_data$mechanism_clean)
  
  # Create bar plot
  p <- ggplot(mech_data, aes(x = reorder(mechanism_clean, confidence), 
                             y = confidence, 
                             fill = mechanism_clean)) +
    geom_col(alpha = 0.8) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50") +
    coord_flip() +
    scale_fill_manual(values = colors, guide = "none") +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    labs(
      title = "Community Assembly Mechanisms",
      subtitle = "Confidence scores for each mechanism",
      x = "",
      y = "Confidence Score"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12, color = "gray50"),
      axis.text = element_text(size = 11),
      panel.grid.major.y = element_blank()
    )
  
  # Add detection indicators
  p <- p + geom_point(
    data = mech_data[mech_data$detected, ],
    aes(x = mechanism_clean, y = confidence),
    shape = 8, size = 3, color = "darkgreen"
  )
  
  if (interactive) {
    plotly::ggplotly(p, tooltip = c("x", "y"))
  } else {
    p
  }
}

#' Plot Mechanism Evidence
#'
#' Visualizes the evidence supporting each mechanism
#'
#' @param mechanisms Assembly mechanisms object
#' @param colors Color palette
#' @param interactive Create interactive plot
#'
#' @return ggplot2 or plotly object
#' @keywords internal
plot_mechanism_evidence <- function(mechanisms, colors, interactive = FALSE) {
  
  # Extract evidence for detected mechanisms
  evidence_data <- data.frame(
    mechanism = character(),
    evidence_type = character(),
    value = numeric(),
    stringsAsFactors = FALSE
  )
  
  # For new format, check if evidence is available
  if (!is.null(mechanisms$evidence)) {
    # Extract evidence from the evidence field
    for (i in seq_along(mechanisms$evidence)) {
      ev <- mechanisms$evidence[[i]]
      if (!is.null(ev$significant_correlations) && length(ev$significant_correlations) > 0) {
        for (j in seq_along(ev$significant_correlations)) {
          evidence_data <- rbind(evidence_data, data.frame(
            mechanism = mechanisms$mechanisms$mechanism[i],
            evidence_type = ev$significant_correlations[j],
            value = ev$correlation_strengths[j],
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }
  
  if (nrow(evidence_data) == 0) {
    # Create placeholder plot
    p <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, 
               label = "No detailed evidence available", 
               size = 6, color = "gray50") +
      theme_void()
    return(p)
  }
  
  # Clean names
  evidence_data$mechanism_clean <- gsub("_", " ", evidence_data$mechanism)
  evidence_data$mechanism_clean <- tools::toTitleCase(evidence_data$mechanism_clean)
  evidence_data$evidence_clean <- gsub("_", " ", evidence_data$evidence_type)
  
  # Create heatmap
  p <- ggplot(evidence_data, aes(x = evidence_clean, y = mechanism_clean, 
                                 fill = value)) +
    geom_tile(color = "white", size = 0.5) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                        midpoint = 0, name = "Evidence\nStrength") +
    labs(
      title = "Assembly Mechanism Evidence",
      subtitle = "Detailed evidence supporting each mechanism",
      x = "Evidence Type",
      y = "Mechanism"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12, color = "gray50"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    )
  
  if (interactive) {
    plotly::ggplotly(p)
  } else {
    p
  }
}

#' Plot Mechanism Network
#'
#' Creates a network visualization showing relationships between mechanisms
#'
#' @param mechanisms Assembly mechanisms object
#' @param colors Color palette
#' @param interactive Create interactive plot
#'
#' @return ggplot2 or plotly object
#' @keywords internal
plot_mechanism_network <- function(mechanisms, colors, interactive = FALSE) {
  
  # Create mechanism interaction network
  if (is.data.frame(mechanisms$mechanisms)) {
    detected_mechs <- mechanisms$mechanisms$mechanism[mechanisms$mechanisms$confidence > 0.5]
  } else {
    detected_mechs <- names(which(sapply(mechanisms$mechanisms, function(x) x$detected)))
  }
  
  if (length(detected_mechs) < 2) {
    # Not enough mechanisms for network
    p <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, 
               label = "Need at least 2 detected mechanisms for network", 
               size = 6, color = "gray50") +
      theme_void()
    return(p)
  }
  
  # Create edge list based on mechanism interactions
  edges <- data.frame(
    from = character(),
    to = character(),
    weight = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Define known mechanism interactions
  interactions <- list(
    "environmental_filtering" = c("competitive_exclusion", "phylogenetic_clustering"),
    "competitive_exclusion" = c("environmental_filtering"),
    "neutral_drift" = c("dispersal_limitation"),
    "dispersal_limitation" = c("neutral_drift", "phylogenetic_clustering")
  )
  
  for (mech1 in detected_mechs) {
    if (mech1 %in% names(interactions)) {
      for (mech2 in interactions[[mech1]]) {
        if (mech2 %in% detected_mechs) {
          # Weight based on combined confidence
          if (is.data.frame(mechanisms$mechanisms)) {
            conf1 <- mechanisms$mechanisms$confidence[mechanisms$mechanisms$mechanism == mech1][1]
            conf2 <- mechanisms$mechanisms$confidence[mechanisms$mechanisms$mechanism == mech2][1]
            weight <- (conf1 + conf2) / 2
          } else {
            weight <- (mechanisms$mechanisms[[mech1]]$confidence + 
                      mechanisms$mechanisms[[mech2]]$confidence) / 2
          }
          edges <- rbind(edges, data.frame(
            from = mech1, to = mech2, weight = weight,
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }
  
  # Create igraph object
  if (nrow(edges) > 0) {
    g <- igraph::graph_from_data_frame(edges, directed = FALSE)
  } else {
    # Create graph with just nodes
    g <- igraph::make_empty_graph(n = length(detected_mechs))
    igraph::V(g)$name <- detected_mechs
  }
  
  # Add node attributes
  if (is.data.frame(mechanisms$mechanisms)) {
    igraph::V(g)$confidence <- sapply(igraph::V(g)$name, function(x) {
      mechanisms$mechanisms$confidence[mechanisms$mechanisms$mechanism == x][1]
    })
  } else {
    igraph::V(g)$confidence <- sapply(igraph::V(g)$name, function(x) {
      mechanisms$mechanisms[[x]]$confidence
    })
  }
  
  # Layout
  layout <- igraph::layout_with_fr(g)
  
  # Convert to ggplot
  if (nrow(edges) > 0) {
    edge_df <- igraph::as_data_frame(g, "edges")
    edge_df$from_x <- layout[match(edge_df$from, igraph::V(g)$name), 1]
    edge_df$from_y <- layout[match(edge_df$from, igraph::V(g)$name), 2]
    edge_df$to_x <- layout[match(edge_df$to, igraph::V(g)$name), 1]
    edge_df$to_y <- layout[match(edge_df$to, igraph::V(g)$name), 2]
  }
  
  node_df <- data.frame(
    name = igraph::V(g)$name,
    x = layout[, 1],
    y = layout[, 2],
    confidence = igraph::V(g)$confidence,
    stringsAsFactors = FALSE
  )
  
  # Clean names
  node_df$name_clean <- gsub("_", " ", node_df$name)
  node_df$name_clean <- tools::toTitleCase(node_df$name_clean)
  
  # Create plot
  p <- ggplot()
  
  # Add edges
  if (nrow(edges) > 0) {
    p <- p + geom_segment(
      data = edge_df,
      aes(x = from_x, y = from_y, xend = to_x, yend = to_y),
      alpha = 0.5, size = 1, color = "gray50"
    )
  }
  
  # Add nodes
  p <- p + geom_point(
    data = node_df,
    aes(x = x, y = y, size = confidence, color = name),
    alpha = 0.8
  ) +
    geom_text(
      data = node_df,
      aes(x = x, y = y, label = name_clean),
      vjust = -1.5, size = 3
    ) +
    scale_size_continuous(range = c(5, 15), name = "Confidence") +
    scale_color_manual(values = colors, guide = "none") +
    labs(
      title = "Assembly Mechanism Network",
      subtitle = "Relationships between detected mechanisms"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, color = "gray50", hjust = 0.5),
      legend.position = "bottom"
    )
  
  if (interactive) {
    # Convert to plotly
    plotly::ggplotly(p)
  } else {
    p
  }
}

#' Plot Mechanism Confidence
#'
#' Creates a radar/spider plot of mechanism confidences
#'
#' @param mechanisms Assembly mechanisms object
#' @param colors Color palette
#' @param interactive Create interactive plot
#'
#' @return ggplot2 or plotly object
#' @keywords internal
plot_mechanism_confidence <- function(mechanisms, colors, interactive = FALSE) {
  
  # Extract confidence scores
  if (is.data.frame(mechanisms$mechanisms)) {
    mech_names <- mechanisms$mechanisms$mechanism
    confidences <- mechanisms$mechanisms$confidence
  } else {
    mech_names <- names(mechanisms$mechanisms)
    confidences <- sapply(mechanisms$mechanisms, function(x) x$confidence)
  }
  
  # Create data for radar plot
  radar_data <- data.frame(
    mechanism = rep(mech_names, 2),
    confidence = c(confidences, rep(0, length(mech_names))),
    group = rep(c("Observed", "Baseline"), each = length(mech_names)),
    stringsAsFactors = FALSE
  )
  
  # Clean names
  radar_data$mechanism_clean <- gsub("_", " ", radar_data$mechanism)
  radar_data$mechanism_clean <- tools::toTitleCase(radar_data$mechanism_clean)
  
  if (interactive) {
    # Create interactive radar plot with plotly
    p <- plotly::plot_ly(
      type = 'scatterpolar',
      mode = 'lines+markers',
      fill = 'toself'
    )
    
    # Add observed values
    obs_data <- radar_data[radar_data$group == "Observed", ]
    p <- p %>% plotly::add_trace(
      theta = obs_data$mechanism_clean,
      r = obs_data$confidence,
      name = 'Observed',
      line = list(color = 'blue'),
      fillcolor = 'rgba(52, 152, 219, 0.3)'
    )
    
    # Add threshold line
    p <- p %>% plotly::add_trace(
      theta = obs_data$mechanism_clean,
      r = rep(0.5, nrow(obs_data)),
      name = 'Threshold',
      line = list(color = 'red', dash = 'dash'),
      fill = 'none'
    )
    
    p <- p %>% plotly::layout(
      title = list(text = "Assembly Mechanism Confidence Scores"),
      polar = list(
        radialaxis = list(
          visible = TRUE,
          range = c(0, 1)
        )
      )
    )
    
    return(p)
  } else {
    # Create ggplot radar chart using coord_polar
    # First need to ensure data closes the polygon
    obs_data <- radar_data[radar_data$group == "Observed", ]
    obs_data <- rbind(obs_data, obs_data[1, ])  # Close the polygon
    
    p <- ggplot(obs_data, aes(x = mechanism_clean, y = confidence)) +
      geom_polygon(aes(group = 1), fill = "steelblue", alpha = 0.3) +
      geom_line(aes(group = 1), color = "steelblue", size = 1) +
      geom_point(color = "steelblue", size = 3) +
      geom_hline(yintercept = 0.5, linetype = "dashed", color = "red", alpha = 0.5) +
      coord_polar() +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
      labs(
        title = "Assembly Mechanism Confidence Scores",
        subtitle = "Radar plot showing confidence for each mechanism",
        x = "",
        y = "Confidence"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, color = "gray50", hjust = 0.5),
        axis.text.x = element_text(size = 10),
        panel.grid.major = element_line(color = "gray90")
      )
    
    return(p)
  }
}

#' Create Assembly Mechanism Dashboard
#'
#' Creates a comprehensive dashboard with multiple visualizations
#'
#' @param mechanisms Assembly mechanisms object
#' @param save_path Optional path to save the dashboard as HTML
#'
#' @return A list of plots or saves an HTML file
#' @export
#' @examples
#' \dontrun{
#' # Create dashboard
#' physeq <- create_demo_phyloseq()
#' universal_info <- extract_universal_information(physeq)
#' mechanisms <- detect_assembly_mechanisms(universal_info)
#' 
#' # Display dashboard
#' assembly_mechanism_dashboard(mechanisms)
#' 
#' # Save as HTML
#' assembly_mechanism_dashboard(mechanisms, save_path = "mechanisms_dashboard.html")
#' }
assembly_mechanism_dashboard <- function(mechanisms, save_path = NULL) {
  
  # Create all plots
  plots <- plot(mechanisms, type = "all", interactive = TRUE)
  
  if (!is.null(save_path)) {
    # Create HTML dashboard
    html_content <- create_mechanism_html_dashboard(plots, mechanisms)
    writeLines(html_content, save_path)
    message("Dashboard saved to: ", save_path)
    invisible(plots)
  } else {
    # Return plots for display
    plots
  }
}

#' Create HTML Dashboard Content
#'
#' @param plots List of plotly plots
#' @param mechanisms Assembly mechanisms object
#' @return HTML content as character string
#' @keywords internal
create_mechanism_html_dashboard <- function(plots, mechanisms) {
  
  # Convert plots to HTML
  plot_html <- lapply(plots, function(p) {
    if (inherits(p, "plotly")) {
      as.character(plotly::as_widget(p))
    } else {
      # Convert ggplot to plotly first
      p_plotly <- plotly::ggplotly(p)
      as.character(plotly::as_widget(p_plotly))
    }
  })
  
  # Create HTML structure
  html_content <- sprintf('
<!DOCTYPE html>
<html>
<head>
    <title>Assembly Mechanism Analysis Dashboard</title>
    <script src="https://cdn.plot.ly/plotly-latest.min.js"></script>
    <style>
        body {
            font-family: Arial, sans-serif;
            margin: 20px;
            background-color: #f5f5f5;
        }
        .header {
            text-align: center;
            padding: 20px;
            background-color: white;
            border-radius: 10px;
            margin-bottom: 20px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .plot-container {
            background-color: white;
            padding: 20px;
            margin-bottom: 20px;
            border-radius: 10px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .summary-text {
            background-color: white;
            padding: 20px;
            border-radius: 10px;
            margin-bottom: 20px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        h1 { color: #2c3e50; }
        h2 { color: #34495e; }
        .grid-container {
            display: grid;
            grid-template-columns: 1fr 1fr;
            gap: 20px;
        }
        @media (max-width: 768px) {
            .grid-container { grid-template-columns: 1fr; }
        }
    </style>
</head>
<body>
    <div class="header">
        <h1>Assembly Mechanism Analysis Dashboard</h1>
        <p>Generated on %s</p>
    </div>
    
    <div class="summary-text">
        <h2>Summary</h2>
        <p>%s</p>
    </div>
    
    <div class="grid-container">
        <div class="plot-container">
            <h2>Mechanism Summary</h2>
            %s
        </div>
        
        <div class="plot-container">
            <h2>Confidence Scores</h2>
            %s
        </div>
    </div>
    
    <div class="plot-container">
        <h2>Mechanism Network</h2>
        %s
    </div>
    
    <div class="plot-container">
        <h2>Evidence Details</h2>
        %s
    </div>
</body>
</html>',
    Sys.Date(),
    mechanisms$interpretation,
    plot_html$summary,
    plot_html$confidence,
    plot_html$network,
    plot_html$evidence
  )
  
  return(html_content)
}