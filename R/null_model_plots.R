#' Null Model Validation Plots
#'
#' @description Visualization functions for null model validation results
#' @name null_model_plots
#' @keywords visualization
NULL

#' Plot P-values from Null Model Validation
#'
#' Creates a plot showing p-values for each taxon indicator
#'
#' @param validated A validated_indicators object
#' @param component Which component to plot ("richness", "evenness", "phylogenetic", "spatial")
#' @param alpha Significance threshold line (default: 0.05)
#'
#' @return A ggplot2 object
#' @keywords internal
plot_validation_pvalues <- function(validated, component, alpha = 0.05) {
  
  comp_name <- paste0(component, "_indicators")
  
  if (is.null(validated$observed[[comp_name]]) || 
      !"p_value" %in% names(validated$observed[[comp_name]])) {
    stop("No p-values found for ", component, " component")
  }
  
  plot_data <- validated$observed[[comp_name]]
  plot_data$significant <- plot_data$p_value <= alpha
  
  # Create plot
  p <- ggplot2::ggplot(plot_data, 
                      ggplot2::aes(x = stats::reorder(taxon, -p_value),
                                  y = p_value,
                                  fill = significant)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::geom_hline(yintercept = alpha, 
                       linetype = "dashed", 
                       color = "red") +
    ggplot2::scale_fill_manual(values = c("TRUE" = "darkgreen", 
                                        "FALSE" = "gray50")) +
    ggplot2::labs(
      title = paste("P-values for", tools::toTitleCase(component), "Indicators"),
      subtitle = paste("Null models:", paste(validated$null_models_used, collapse = ", ")),
      x = "Taxon",
      y = "P-value",
      fill = paste("p <", alpha)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  
  return(p)
}

#' Plot Effect Sizes from Null Model Validation
#'
#' Creates a plot showing standardized effect sizes with significance
#'
#' @param validated A validated_indicators object
#' @param component Which component to plot
#'
#' @return A ggplot2 object
#' @keywords internal
plot_validation_effects <- function(validated, component) {
  
  comp_name <- paste0(component, "_indicators")
  
  if (is.null(validated$observed[[comp_name]]) || 
      !"effect_size" %in% names(validated$observed[[comp_name]])) {
    stop("No effect sizes found for ", component, " component")
  }
  
  plot_data <- validated$observed[[comp_name]]
  plot_data$significant <- plot_data$p_value <= 0.05
  
  # Add confidence intervals based on null distribution
  # Approximate 95% CI as +/- 2 (standardized effect)
  plot_data$ci_lower <- -2
  plot_data$ci_upper <- 2
  
  # Create plot
  p <- ggplot2::ggplot(plot_data,
                      ggplot2::aes(x = stats::reorder(taxon, -abs(effect_size)),
                                  y = effect_size,
                                  color = significant)) +
    ggplot2::geom_hline(yintercept = 0, color = "black") +
    ggplot2::geom_hline(yintercept = c(-2, 2), 
                       linetype = "dashed", 
                       color = "gray50") +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_segment(ggplot2::aes(xend = taxon, y = 0, yend = effect_size),
                         alpha = 0.5) +
    ggplot2::scale_color_manual(values = c("TRUE" = "darkgreen",
                                         "FALSE" = "gray50")) +
    ggplot2::labs(
      title = paste("Standardized Effect Sizes for", 
                   tools::toTitleCase(component), "Indicators"),
      subtitle = "Effect size = (observed - mean(null)) / sd(null)",
      x = "Taxon",
      y = "Standardized Effect Size",
      color = "Significant\n(p < 0.05)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  
  return(p)
}

#' Plot Null Distributions
#'
#' Shows observed values against null distributions for top indicators
#'
#' @param validated A validated_indicators object
#' @param component Which component to plot
#' @param top_n Number of top indicators to show (default: 6)
#'
#' @return A ggplot2 object
#' @keywords internal
plot_null_distributions <- function(validated, component, top_n = 6) {
  
  comp_name <- paste0(component, "_indicators")
  
  if (is.null(validated$observed[[comp_name]])) {
    stop("No data found for ", component, " component")
  }
  
  # Get top indicators
  obs_data <- validated$observed[[comp_name]]
  top_taxa <- head(obs_data$taxon, top_n)
  
  # Collect null distributions for these taxa
  plot_data <- list()
  
  for (i in 1:length(top_taxa)) {
    taxon_name <- top_taxa[i]
    obs_value <- obs_data$contribution[i]
    
    # Extract null values for this position
    null_vals <- numeric()
    
    for (null_model in names(validated$null_distributions)) {
      if (!is.null(validated$null_distributions[[null_model]])) {
        model_nulls <- sapply(validated$null_distributions[[null_model]],
                            function(x) {
                              if (length(x[[component]]) >= i) {
                                x[[component]][i]
                              } else {
                                NA
                              }
                            })
        null_vals <- c(null_vals, model_nulls[!is.na(model_nulls)])
      }
    }
    
    if (length(null_vals) > 0) {
      # Create data frame for this taxon
      taxon_df <- data.frame(
        taxon = taxon_name,
        value = c(null_vals, obs_value),
        type = c(rep("null", length(null_vals)), "observed"),
        stringsAsFactors = FALSE
      )
      plot_data[[taxon_name]] <- taxon_df
    }
  }
  
  # Combine all data
  plot_df <- do.call(rbind, plot_data)
  
  # Create faceted density plots
  p <- ggplot2::ggplot(plot_df[plot_df$type == "null", ],
                      ggplot2::aes(x = value)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ..density..),
                          bins = 30,
                          fill = "gray70",
                          alpha = 0.7) +
    ggplot2::geom_density(color = "black") +
    ggplot2::geom_vline(data = plot_df[plot_df$type == "observed", ],
                       ggplot2::aes(xintercept = value),
                       color = "red",
                       linetype = "dashed",
                       size = 1) +
    ggplot2::facet_wrap(~ taxon, scales = "free", ncol = 2) +
    ggplot2::labs(
      title = paste("Null Distributions for Top", 
                   tools::toTitleCase(component), "Indicators"),
      subtitle = "Red line = observed value, Gray = null distribution",
      x = "Contribution Value",
      y = "Density"
    ) +
    ggplot2::theme_minimal()
  
  return(p)
}

#' Create Null Model Validation Report
#'
#' Generates comprehensive HTML report of null model validation
#'
#' @param validated A validated_indicators object
#' @param physeq Original phyloseq object
#' @param output_file Output HTML file path
#' @param title Report title
#'
#' @return Path to generated report
#' @export
#' @examples
#' \dontrun{
#' # Validate indicators
#' validated <- validate_taxa_indicators(physeq, indicators)
#' 
#' # Generate report
#' report_null_validation(validated, physeq, "null_validation_report.html")
#' }
report_null_validation <- function(validated,
                                 physeq,
                                 output_file = "null_validation_report.html",
                                 title = "Null Model Validation Report") {
  
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
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE, 
                     fig.width = 10, fig.height = 6)
library(diversityGPT)
library(ggplot2)
library(DT)
```

# Overview

This report presents null model validation results for taxa-component indicators.

**Null models used:** `r paste(validated$null_models_used, collapse = ", ")`  
**Number of permutations:** `r validated$n_permutations`

# Summary Statistics

```{r summary-table}
# Create summary table
summary_data <- data.frame(
  Component = c("Richness", "Evenness", "Phylogenetic", "Spatial"),
  Total_Indicators = sapply(c("richness", "evenness", "phylogenetic", "spatial"), 
                          function(x) {
                            if (!is.null(validated$summary_statistics[[x]])) {
                              validated$summary_statistics[[x]]$n_total
                            } else NA
                          }),
  Significant = sapply(c("richness", "evenness", "phylogenetic", "spatial"),
                      function(x) {
                        if (!is.null(validated$summary_statistics[[x]])) {
                          validated$summary_statistics[[x]]$n_significant
                        } else NA
                      }),
  Percent_Significant = sapply(c("richness", "evenness", "phylogenetic", "spatial"),
                             function(x) {
                               if (!is.null(validated$summary_statistics[[x]])) {
                                 round(validated$summary_statistics[[x]]$prop_significant * 100, 1)
                               } else NA
                             }),
  Mean_Effect_Size = sapply(c("richness", "evenness", "phylogenetic", "spatial"),
                          function(x) {
                            if (!is.null(validated$summary_statistics[[x]])) {
                              round(validated$summary_statistics[[x]]$mean_effect_size, 2)
                            } else NA
                          })
)

DT::datatable(summary_data, options = list(pageLength = 4, dom = "t"))
```

# Component-Specific Results

## Richness Indicators

```{r richness-pvalues, fig.height=5}
plot(validated, type = "pvalues", component = "richness")
```

```{r richness-effects, fig.height=5}
plot(validated, type = "effects", component = "richness")
```

```{r richness-nulldist, fig.height=8}
plot(validated, type = "null_dist", component = "richness")
```

### Significant Richness Indicators

```{r richness-table}
if (!is.null(validated$significant_indicators$richness_drivers) && 
    nrow(validated$significant_indicators$richness_drivers) > 0) {
  DT::datatable(validated$significant_indicators$richness_drivers[,
                  c("taxon", "contribution", "p_value", "effect_size")],
                options = list(pageLength = 10)) %%>%%
    DT::formatRound(columns = c("contribution", "p_value", "effect_size"), 
                   digits = 3)
} else {
  cat("No significant richness indicators found.")
}
```

## Evenness Indicators

```{r evenness-pvalues, fig.height=5}
plot(validated, type = "pvalues", component = "evenness")
```

```{r evenness-effects, fig.height=5}
plot(validated, type = "effects", component = "evenness")
```

### Significant Evenness Indicators

```{r evenness-table}
if (!is.null(validated$significant_indicators$evenness_drivers) &&
    nrow(validated$significant_indicators$evenness_drivers) > 0) {
  DT::datatable(validated$significant_indicators$evenness_drivers[,
                  c("taxon", "contribution", "p_value", "effect_size")],
                options = list(pageLength = 10)) %%>%%
    DT::formatRound(columns = c("contribution", "p_value", "effect_size"),
                   digits = 3)
} else {
  cat("No significant evenness indicators found.")
}
```

## Phylogenetic Indicators

```{r phylo-pvalues, fig.height=5}
plot(validated, type = "pvalues", component = "phylogenetic")
```

```{r phylo-effects, fig.height=5}
plot(validated, type = "effects", component = "phylogenetic")
```

### Significant Phylogenetic Indicators

```{r phylo-table}
if (!is.null(validated$significant_indicators$phylogenetic_drivers) &&
    nrow(validated$significant_indicators$phylogenetic_drivers) > 0) {
  DT::datatable(validated$significant_indicators$phylogenetic_drivers[,
                  c("taxon", "contribution", "p_value", "effect_size")],
                options = list(pageLength = 10)) %%>%%
    DT::formatRound(columns = c("contribution", "p_value", "effect_size"),
                   digits = 3)
} else {
  cat("No significant phylogenetic indicators found.")
}
```

## Spatial Indicators

```{r spatial-pvalues, fig.height=5}
plot(validated, type = "pvalues", component = "spatial")
```

```{r spatial-effects, fig.height=5}
plot(validated, type = "effects", component = "spatial")
```

### Significant Spatial Indicators

```{r spatial-table}
if (!is.null(validated$significant_indicators$spatial_drivers) &&
    nrow(validated$significant_indicators$spatial_drivers) > 0) {
  DT::datatable(validated$significant_indicators$spatial_drivers[,
                  c("taxon", "contribution", "p_value", "effect_size")],
                options = list(pageLength = 10)) %%>%%
    DT::formatRound(columns = c("contribution", "p_value", "effect_size"),
                   digits = 3)
} else {
  cat("No significant spatial indicators found.")
}
```

# Interpretation

## What do these results mean?

- **P-values** indicate whether observed indicator values exceed random expectation
- **Effect sizes** show the magnitude of deviation from null models (in standard deviations)
- **Significant indicators** (p < 0.05) are unlikely to arise by chance alone

## Caveats

- Different null models test different aspects of community structure
- Multiple testing corrections may be appropriate for strict inference
- Effect sizes are more informative than p-values alone

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
  
  message("Null validation report generated: ", output_file)
  return(output_file)
}