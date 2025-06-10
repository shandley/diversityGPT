#' Taxa Indicator Visualization Functions
#'
#' @description Advanced visualization functions for taxa indicator analysis
#' @name taxa_indicator_plots
#' @keywords visualization
NULL

#' Plot Indicator Network
#'
#' Creates a network visualization showing relationships between taxa and components
#'
#' @param indicators Taxa indicators object
#' @param top_n Number of top taxa per component
#' @param interactive Use interactive networkD3
#'
#' @return Network plot object
#' @keywords internal
plot_indicator_network <- function(indicators, top_n, interactive) {
  # Prepare edge data
  edges <- list()
  
  components <- c("richness", "evenness", "phylogenetic", "spatial")
  for (comp in components) {
    comp_name <- paste0(comp, "_indicators")
    if (!is.null(indicators[[comp_name]])) {
      indicator_df <- head(indicators[[comp_name]], top_n)
      
      for (i in 1:nrow(indicator_df)) {
        edges[[length(edges) + 1]] <- data.frame(
          from = indicator_df$taxon[i],
          to = paste0(toupper(substr(comp, 1, 1)), "-component"),
          weight = indicator_df$contribution[i],
          component = comp,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  edge_df <- do.call(rbind, edges)
  
  if (interactive) {
    # Create interactive network using networkD3
    # Get unique nodes
    nodes <- data.frame(
      name = unique(c(edge_df$from, edge_df$to)),
      stringsAsFactors = FALSE
    )
    nodes$id <- 0:(nrow(nodes) - 1)
    
    # Add node type
    nodes$type <- ifelse(grepl("-component$", nodes$name), "component", "taxon")
    
    # Convert edges to use node IDs
    edges_d3 <- edge_df
    edges_d3$source <- match(edges_d3$from, nodes$name) - 1
    edges_d3$target <- match(edges_d3$to, nodes$name) - 1
    edges_d3$value <- edges_d3$weight * 100  # Scale for visibility
    
    # Create color mapping
    component_colors <- c(
      "richness" = "#E41A1C",
      "evenness" = "#377EB8", 
      "phylogenetic" = "#4DAF4A",
      "spatial" = "#984EA3"
    )
    
    edges_d3$color <- component_colors[edges_d3$component]
    
    # Create network
    p <- networkD3::forceNetwork(
      Links = edges_d3,
      Nodes = nodes,
      Source = "source",
      Target = "target", 
      Value = "value",
      NodeID = "name",
      Group = "type",
      linkColour = edges_d3$color,
      opacity = 0.8,
      zoom = TRUE,
      legend = TRUE,
      fontSize = 12,
      charge = -100
    )
    
  } else {
    # Create static network using igraph/ggraph
    g <- igraph::graph_from_data_frame(edge_df, directed = FALSE)
    
    # Set edge attributes
    igraph::E(g)$weight <- edge_df$weight
    igraph::E(g)$component <- edge_df$component
    
    # Set node attributes
    igraph::V(g)$type <- ifelse(grepl("-component$", igraph::V(g)$name), 
                                "component", "taxon")
    
    # Create layout
    layout <- igraph::layout_with_fr(g)
    
    # Create ggraph plot
    p <- ggraph::ggraph(g, layout = layout) +
      ggraph::geom_edge_link(ggplot2::aes(width = weight, 
                                         color = component),
                           alpha = 0.6) +
      ggraph::geom_node_point(ggplot2::aes(size = ifelse(type == "component", 8, 4),
                                          color = type)) +
      ggraph::geom_node_text(ggplot2::aes(label = name),
                           repel = TRUE, size = 3) +
      ggraph::scale_edge_width(range = c(0.5, 3)) +
      ggplot2::labs(title = "Taxa-Component Indicator Network",
                   subtitle = "Edge width represents contribution strength") +
      ggraph::theme_graph()
  }
  
  return(p)
}

#' Plot Indicator Heatmap
#'
#' Creates a heatmap showing taxa contributions across all components
#'
#' @param indicators Taxa indicators object
#' @param top_n Number of top taxa to include
#' @param interactive Use plotly for interactivity
#'
#' @return Heatmap plot object
#' @keywords internal
plot_indicator_heatmap <- function(indicators, top_n, interactive) {
  # Collect all unique taxa from top indicators
  all_taxa <- unique(unlist(lapply(
    c("richness_indicators", "evenness_indicators", 
      "phylogenetic_indicators", "spatial_indicators"),
    function(comp) {
      if (!is.null(indicators[[comp]])) {
        head(indicators[[comp]]$taxon, top_n)
      }
    }
  )))
  
  # Create matrix of contributions
  contribution_mat <- matrix(0, 
                           nrow = length(all_taxa),
                           ncol = 4,
                           dimnames = list(all_taxa,
                                         c("Richness", "Evenness", 
                                           "Phylogenetic", "Spatial")))
  
  # Fill matrix
  components <- c("richness", "evenness", "phylogenetic", "spatial")
  for (i in seq_along(components)) {
    comp_name <- paste0(components[i], "_indicators")
    if (!is.null(indicators[[comp_name]])) {
      indicator_df <- indicators[[comp_name]]
      for (j in 1:nrow(indicator_df)) {
        if (indicator_df$taxon[j] %in% all_taxa) {
          contribution_mat[indicator_df$taxon[j], i] <- indicator_df$contribution[j]
        }
      }
    }
  }
  
  # Order taxa by total contribution
  total_contrib <- rowSums(contribution_mat)
  contribution_mat <- contribution_mat[order(total_contrib, decreasing = TRUE), ]
  
  if (interactive) {
    # Create interactive heatmap using plotly
    p <- plotly::plot_ly(
      z = contribution_mat,
      x = colnames(contribution_mat),
      y = rownames(contribution_mat),
      type = "heatmap",
      colorscale = "Viridis",
      hovertemplate = "Taxon: %{y}<br>Component: %{x}<br>Contribution: %{z:.3f}<extra></extra>"
    ) %>%
      plotly::layout(
        title = "Taxa Contributions Across Information Components",
        xaxis = list(title = "Information Component"),
        yaxis = list(title = "Taxon", tickfont = list(size = 10))
      )
    
  } else {
    # Create static heatmap using ggplot2
    # Convert to long format
    heatmap_data <- as.data.frame(as.table(contribution_mat))
    names(heatmap_data) <- c("Taxon", "Component", "Contribution")
    
    p <- ggplot2::ggplot(heatmap_data,
                        ggplot2::aes(x = Component, 
                                    y = Taxon,
                                    fill = Contribution)) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_viridis_c() +
      ggplot2::labs(title = "Taxa Contributions Across Information Components",
                   x = "Information Component",
                   y = "Taxon",
                   fill = "Contribution") +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.y = ggplot2::element_text(size = 8),
                    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  }
  
  return(p)
}

#' Plot Indicator Contributions
#'
#' Creates a comprehensive visualization of taxa contributions
#'
#' @param indicators Taxa indicators object
#' @param top_n Number of top taxa
#' @param interactive Use plotly
#'
#' @return Contribution plot object
#' @keywords internal
plot_indicator_contributions <- function(indicators, top_n, interactive) {
  # Combine data from all components
  all_indicators <- list()
  common_cols <- c("taxon", "contribution", "rank", "component")
  
  components <- c("richness", "evenness", "phylogenetic", "spatial")
  for (comp in components) {
    comp_name <- paste0(comp, "_indicators")
    if (!is.null(indicators[[comp_name]]) && nrow(indicators[[comp_name]]) > 0) {
      indicator_df <- head(indicators[[comp_name]], top_n)
      # Extract only common columns and add component
      indicator_df_common <- indicator_df[, c("taxon", "contribution", "rank")]
      indicator_df_common$component <- comp
      all_indicators[[comp]] <- indicator_df_common
    }
  }
  
  combined_indicators <- do.call(rbind, all_indicators)
  
  # Calculate summary statistics per taxon
  taxon_summary <- aggregate(contribution ~ taxon,
                           data = combined_indicators,
                           FUN = function(x) c(mean = mean(x),
                                             sum = sum(x),
                                             n = length(x)))
  
  taxon_summary <- do.call(data.frame, taxon_summary)
  names(taxon_summary) <- c("taxon", "mean_contribution", "total_contribution", "n_components")
  
  # Order by total contribution
  taxon_summary <- taxon_summary[order(taxon_summary$total_contribution, 
                                     decreasing = TRUE), ]
  taxon_summary <- head(taxon_summary, top_n)
  
  # Create plot
  if (interactive) {
    # Create scatter plot with size representing n_components
    p <- plotly::plot_ly(
      data = taxon_summary,
      x = ~mean_contribution,
      y = ~total_contribution,
      size = ~n_components,
      text = ~taxon,
      hovertemplate = paste(
        "Taxon: %{text}<br>",
        "Mean contribution: %{x:.3f}<br>",
        "Total contribution: %{y:.3f}<br>",
        "Components: %{marker.size}<br>",
        "<extra></extra>"
      ),
      type = "scatter",
      mode = "markers+text",
      textposition = "top center",
      marker = list(
        sizemode = "diameter",
        sizeref = 0.1,
        color = ~n_components,
        colorscale = "Viridis"
      )
    ) %>%
      plotly::layout(
        title = "Taxa Indicator Contribution Summary",
        xaxis = list(title = "Mean Contribution per Component"),
        yaxis = list(title = "Total Contribution Across Components")
      )
    
  } else {
    # Create static scatter plot
    p <- ggplot2::ggplot(taxon_summary,
                        ggplot2::aes(x = mean_contribution,
                                    y = total_contribution,
                                    size = n_components,
                                    color = n_components,
                                    label = taxon)) +
      ggplot2::geom_point(alpha = 0.7) +
      ggplot2::geom_text(vjust = -0.5, size = 3) +
      ggplot2::scale_size_continuous(range = c(3, 10)) +
      ggplot2::scale_color_viridis_c() +
      ggplot2::labs(title = "Taxa Indicator Contribution Summary",
                   x = "Mean Contribution per Component",
                   y = "Total Contribution Across Components",
                   size = "# Components",
                   color = "# Components") +
      ggplot2::theme_minimal()
  }
  
  return(p)
}

#' Create Taxa Indicator Report
#'
#' Generates a comprehensive HTML report of taxa indicator analysis
#'
#' @param indicators Taxa indicators object
#' @param physeq Original phyloseq object
#' @param output_file Output HTML file path
#' @param title Report title
#'
#' @return Path to generated report
#' @export
#' @examples
#' \dontrun{
#' # Generate taxa indicator report
#' report_taxa_indicators(indicators, physeq, "taxa_indicators_report.html")
#' }
report_taxa_indicators <- function(indicators, 
                               physeq,
                               output_file = "taxa_indicators_report.html",
                               title = "Taxa Indicator Analysis Report") {
  
  # Create temporary Rmd file
  temp_rmd <- tempfile(fileext = ".Rmd")
  
  # Write Rmd content
  rmd_content <- sprintf('---
title: "%s"
date: "`r Sys.Date()`"
output:
  html_document:
    toc: true
    toc_float: true
    code_folding: hide
    theme: cerulean
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE, fig.width = 10, fig.height = 6)
library(diversityGPT)
library(ggplot2)
library(plotly)
library(DT)
```

# Overview

This report presents the results of taxa indicator analysis, identifying which taxa serve as indicators for each information component (Richness, Evenness, Phylogenetic, and Spatial) in the diversity patterns.

**Analysis method:** `r indicators$method`

# Summary Statistics

```{r summary}
# Create summary table
summary_df <- data.frame(
  Component = c("Richness", "Evenness", "Phylogenetic", "Spatial"),
  Top_Taxon = sapply(c("richness", "evenness", "phylogenetic", "spatial"), function(comp) {
    comp_name <- paste0(comp, "_indicators")
    if (!is.null(indicators[[comp_name]]) && nrow(indicators[[comp_name]]) > 0) {
      indicators[[comp_name]]$taxon[1]
    } else {
      "N/A"
    }
  }),
  Top_Contribution = sapply(c("richness", "evenness", "phylogenetic", "spatial"), function(comp) {
    comp_name <- paste0(comp, "_indicators")
    if (!is.null(indicators[[comp_name]]) && nrow(indicators[[comp_name]]) > 0) {
      round(indicators[[comp_name]]$contribution[1], 3)
    } else {
      NA
    }
  }),
  stringsAsFactors = FALSE
)

DT::datatable(summary_df, options = list(pageLength = 4, dom = "t"))
```

**Total unique indicator taxa:** `r indicators$summary$overall$total_unique_indicators`

**Taxa indicating multiple components:** `r indicators$summary$overall$n_multi_component`

# Component-Specific Indicators

## Richness Indicators

Taxa that serve as indicators for species richness patterns:

```{r richness-table}
if (!is.null(indicators$richness_indicators)) {
  DT::datatable(indicators$richness_indicators[, c("taxon", "contribution", "presence_frequency", "mean_abundance")],
                options = list(pageLength = 10)) %%>%%
    DT::formatRound(columns = c("contribution", "presence_frequency", "mean_abundance"), digits = 3)
}
```

## Evenness Indicators

Taxa that serve as indicators for community evenness patterns:

```{r evenness-table}
if (!is.null(indicators$evenness_indicators)) {
  DT::datatable(indicators$evenness_indicators[, c("taxon", "contribution", "mean_relative_abundance", "cv_relative_abundance")],
                options = list(pageLength = 10)) %%>%%
    DT::formatRound(columns = c("contribution", "mean_relative_abundance", "cv_relative_abundance"), digits = 3)
}
```

## Phylogenetic Indicators

Taxa that serve as indicators for phylogenetic diversity patterns:

```{r phylogenetic-table}
if (!is.null(indicators$phylogenetic_indicators)) {
  cols <- c("taxon", "contribution")
  if ("phylogenetic_uniqueness" %%in%% names(indicators$phylogenetic_indicators)) {
    cols <- c(cols, "phylogenetic_uniqueness")
  }
  DT::datatable(indicators$phylogenetic_indicators[, cols],
                options = list(pageLength = 10)) %%>%%
    DT::formatRound(columns = setdiff(cols, "taxon"), digits = 3)
}
```

## Spatial Indicators

Taxa that serve as indicators for spatial heterogeneity:

```{r spatial-table}
if (!is.null(indicators$spatial_indicators)) {
  DT::datatable(indicators$spatial_indicators[, c("taxon", "contribution", "dispersion_index", "occupancy")],
                options = list(pageLength = 10)) %%>%%
    DT::formatRound(columns = c("contribution", "dispersion_index", "occupancy"), digits = 3)
}
```

# Visualizations

## Indicator Contributions by Component

```{r bar-plot, fig.height=8}
plot(indicators, type = "bar", top_n = 10, interactive = TRUE)
```

## Taxa-Component Network

```{r network-plot}
plot(indicators, type = "network", top_n = 10, interactive = TRUE)
```

## Contribution Heatmap

```{r heatmap}
plot(indicators, type = "heatmap", top_n = 15, interactive = TRUE)
```

## Multi-Component Indicators

```{r multi-indicators}
plot(indicators, type = "contribution", top_n = 20, interactive = TRUE)
```

# Session Information

```{r session}
sessionInfo()
```
', title)
  
  writeLines(rmd_content, temp_rmd)
  
  # Render report
  rmarkdown::render(temp_rmd,
                   output_file = output_file,
                   output_dir = dirname(output_file),
                   quiet = TRUE)
  
  # Clean up
  unlink(temp_rmd)
  
  message("Taxa indicator report generated: ", output_file)
  return(output_file)
}