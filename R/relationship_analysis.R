#' Helper functions for metric relationship analysis
#'
#' @description
#' Supporting functions for discovering mathematical relationships
#' between diversity metrics through pairwise analysis and 
#' information-theoretic approaches.

#' Compute pairwise relationships between metrics
#' @keywords internal
compute_pairwise_relationships <- function(metrics_data, relationship_types) {
  
  metric_names <- names(metrics_data)
  n_metrics <- length(metric_names)
  
  # Initialize results storage
  relationships <- list()
  
  cli::cli_alert_info("Computing {n_metrics * (n_metrics - 1) / 2} pairwise relationships...")
  
  for (i in seq_len(n_metrics - 1)) {
    for (j in seq(i + 1, n_metrics)) {
      
      metric_x <- metric_names[i]
      metric_y <- metric_names[j]
      
      pair_key <- paste(metric_x, metric_y, sep = "_vs_")
      
      x_values <- metrics_data[[metric_x]]
      y_values <- metrics_data[[metric_y]]
      
      # Remove missing values
      complete_cases <- complete.cases(x_values, y_values)
      
      if (sum(complete_cases) < 5) {
        next  # Not enough data
      }
      
      x_clean <- x_values[complete_cases]
      y_clean <- y_values[complete_cases]
      
      # Compute different relationship types
      pair_relationships <- list()
      
      # Linear relationship
      if ("linear" %in% relationship_types) {
        linear_result <- tryCatch({
          cor_pearson <- cor(x_clean, y_clean, method = "pearson")
          cor_spearman <- cor(x_clean, y_clean, method = "spearman")
          
          lm_model <- lm(y_clean ~ x_clean)
          r_squared <- summary(lm_model)$r.squared
          
          list(
            pearson_correlation = cor_pearson,
            spearman_correlation = cor_spearman,
            r_squared = r_squared,
            slope = coef(lm_model)[2],
            intercept = coef(lm_model)[1]
          )
        }, error = function(e) NULL)
        
        if (!is.null(linear_result)) {
          pair_relationships$linear <- linear_result
        }
      }
      
      # Nonlinear relationship
      if ("nonlinear" %in% relationship_types) {
        nonlinear_result <- tryCatch({
          
          # Try polynomial fit (degree 2)
          poly_model <- lm(y_clean ~ poly(x_clean, 2))
          poly_r_squared <- summary(poly_model)$r.squared
          
          # Try log transformation
          if (all(x_clean > 0) && all(y_clean > 0)) {
            log_model <- lm(log(y_clean) ~ log(x_clean))
            log_r_squared <- summary(log_model)$r.squared
          } else {
            log_r_squared <- 0
          }
          
          # Try exponential relationship
          if (all(y_clean > 0)) {
            exp_model <- tryCatch({
              lm(log(y_clean) ~ x_clean)
            }, error = function(e) NULL)
            
            exp_r_squared <- if (!is.null(exp_model)) {
              summary(exp_model)$r.squared
            } else {
              0
            }
          } else {
            exp_r_squared <- 0
          }
          
          list(
            polynomial_r_squared = poly_r_squared,
            log_transform_r_squared = log_r_squared,
            exponential_r_squared = exp_r_squared,
            best_nonlinear = max(poly_r_squared, log_r_squared, exp_r_squared)
          )
        }, error = function(e) NULL)
        
        if (!is.null(nonlinear_result)) {
          pair_relationships$nonlinear <- nonlinear_result
        }
      }
      
      # Conditional relationship (based on terciles)
      if ("conditional" %in% relationship_types) {
        conditional_result <- tryCatch({
          
          # Split into terciles based on x
          x_terciles <- quantile(x_clean, probs = c(0, 1/3, 2/3, 1))
          
          tercile_cors <- numeric(3)
          
          for (t in 1:3) {
            lower <- x_terciles[t]
            upper <- x_terciles[t + 1]
            
            in_tercile <- x_clean >= lower & x_clean <= upper
            
            if (sum(in_tercile) >= 3) {
              tercile_cors[t] <- cor(x_clean[in_tercile], y_clean[in_tercile])
            } else {
              tercile_cors[t] <- NA
            }
          }
          
          # Measure of conditional dependence
          conditional_variance <- var(tercile_cors, na.rm = TRUE)
          
          list(
            tercile_correlations = tercile_cors,
            conditional_variance = conditional_variance,
            relationship_stability = 1 - conditional_variance  # Higher = more stable
          )
        }, error = function(e) NULL)
        
        if (!is.null(conditional_result)) {
          pair_relationships$conditional <- conditional_result
        }
      }
      
      # Store results
      relationships[[pair_key]] <- list(
        metric_x = metric_x,
        metric_y = metric_y,
        n_observations = sum(complete_cases),
        relationships = pair_relationships
      )
    }
  }
  
  return(relationships)
}

#' Analyze information-theoretic relationships
#' @keywords internal
analyze_information_relationships <- function(information_components, metrics_data) {
  
  component_cols <- c("R_component", "E_component", "P_component", "S_component")
  metric_names <- names(metrics_data)
  
  # Component contributions to each metric
  component_contributions <- list()
  
  for (metric in metric_names) {
    
    metric_values <- metrics_data[[metric]]
    
    # Skip if all NA or constant
    if (all(is.na(metric_values)) || sd(metric_values, na.rm = TRUE) == 0) {
      next
    }
    
    # Compute correlation with each component
    contributions <- list()
    
    for (comp in component_cols) {
      if (comp %in% names(information_components)) {
        comp_values <- information_components[[comp]]
        
        complete_cases <- complete.cases(metric_values, comp_values)
        
        if (sum(complete_cases) >= 5) {
          # Check for zero standard deviation
          metric_sd <- sd(metric_values[complete_cases])
          comp_sd <- sd(comp_values[complete_cases])
          
          if (metric_sd > 0 && comp_sd > 0) {
            correlation <- cor(
              metric_values[complete_cases], 
              comp_values[complete_cases],
              use = "complete.obs"
            )
            contributions[[comp]] <- correlation
          } else {
            contributions[[comp]] <- 0  # No correlation if no variation
          }
        }
      }
    }
    
    component_contributions[[metric]] <- contributions
  }
  
  # Information content analysis
  information_analysis <- analyze_information_content(
    information_components,
    component_cols
  )
  
  # Metric clustering based on information profiles
  metric_clustering <- cluster_metrics_by_information(
    component_contributions
  )
  
  return(list(
    component_contributions = component_contributions,
    information_analysis = information_analysis,
    metric_clustering = metric_clustering
  ))
}

#' Analyze information content patterns
#' @keywords internal
analyze_information_content <- function(information_components, component_cols) {
  
  # Component statistics
  component_stats <- list()
  
  for (comp in component_cols) {
    if (comp %in% names(information_components)) {
      values <- information_components[[comp]]
      
      component_stats[[comp]] <- list(
        mean = mean(values, na.rm = TRUE),
        sd = sd(values, na.rm = TRUE),
        min = min(values, na.rm = TRUE),
        max = max(values, na.rm = TRUE),
        cv = sd(values, na.rm = TRUE) / mean(values, na.rm = TRUE)
      )
    }
  }
  
  # Component correlations (handle zero variance)
  component_matrix <- information_components[, component_cols, drop = FALSE]
  
  # Check for constant columns
  constant_cols <- sapply(component_matrix, function(x) sd(x, na.rm = TRUE) == 0)
  
  if (any(constant_cols)) {
    cli::cli_alert_warning("Some information components have zero variance")
    # Set correlations to 0 for constant components
    component_correlations <- matrix(0, nrow = ncol(component_matrix), ncol = ncol(component_matrix))
    rownames(component_correlations) <- colnames(component_correlations) <- colnames(component_matrix)
    diag(component_correlations) <- 1
  } else {
    component_correlations <- cor(component_matrix, use = "complete.obs")
  }
  
  # Principal component analysis of information components
  pca_result <- tryCatch({
    prcomp(component_matrix, scale. = TRUE, center = TRUE)
  }, error = function(e) NULL)
  
  return(list(
    component_statistics = component_stats,
    component_correlations = component_correlations,
    pca_result = pca_result
  ))
}

#' Cluster metrics by information profiles
#' @keywords internal
cluster_metrics_by_information <- function(component_contributions) {
  
  # Convert to matrix for clustering
  metric_names <- names(component_contributions)
  component_names <- unique(unlist(lapply(component_contributions, names)))
  
  if (length(metric_names) < 3 || length(component_names) < 2) {
    return(list(clustering = "insufficient_data"))
  }
  
  # Build contribution matrix
  contrib_matrix <- matrix(0, nrow = length(metric_names), ncol = length(component_names))
  rownames(contrib_matrix) <- metric_names
  colnames(contrib_matrix) <- component_names
  
  for (i in seq_along(metric_names)) {
    metric <- metric_names[i]
    for (comp in component_names) {
      if (comp %in% names(component_contributions[[metric]])) {
        contrib_matrix[i, comp] <- component_contributions[[metric]][[comp]]
      }
    }
  }
  
  # Hierarchical clustering
  clustering_result <- tryCatch({
    
    # Use correlation distance
    dist_matrix <- as.dist(1 - cor(t(contrib_matrix), use = "complete.obs"))
    hclust_result <- hclust(dist_matrix, method = "ward.D2")
    
    # Cut tree into clusters
    n_clusters <- min(4, floor(length(metric_names) / 2))
    if (n_clusters >= 2) {
      cluster_assignments <- cutree(hclust_result, k = n_clusters)
    } else {
      cluster_assignments <- rep(1, length(metric_names))
      names(cluster_assignments) <- metric_names
    }
    
    list(
      hclust = hclust_result,
      clusters = cluster_assignments,
      n_clusters = n_clusters,
      contribution_matrix = contrib_matrix
    )
    
  }, error = function(e) {
    list(clustering = "failed", error = e$message)
  })
  
  return(clustering_result)
}

#' Print method for universal_relationships objects
#'
#' @param x A universal_relationships object
#' @param ... Additional arguments (unused)
#' @export
print.universal_relationships <- function(x, ...) {
  cli::cli_h1("Universal Metric Relationships")
  
  cli::cli_text("Metrics analyzed: {length(x$metrics_analyzed)}")
  cli::cli_text("Pairwise relationships: {length(x$pairwise_relationships)}")
  cli::cli_text("")
  
  # Summary of strongest relationships
  cli::cli_h2("Strongest Linear Relationships")
  
  linear_cors <- sapply(x$pairwise_relationships, function(rel) {
    if (!is.null(rel$relationships$linear)) {
      abs(rel$relationships$linear$pearson_correlation)
    } else {
      0
    }
  })
  
  # Top 5 correlations
  top_indices <- order(linear_cors, decreasing = TRUE)[1:min(5, length(linear_cors))]
  
  for (i in top_indices) {
    if (linear_cors[i] > 0) {
      rel <- x$pairwise_relationships[[i]]
      cor_val <- rel$relationships$linear$pearson_correlation
      cli::cli_text("{rel$metric_x} ↔ {rel$metric_y}: r = {round(cor_val, 3)}")
    }
  }
  
  # Information component analysis
  cli::cli_text("")
  cli::cli_h2("Information Component Analysis")
  
  info_analysis <- x$information_relationships$information_analysis
  
  if (!is.null(info_analysis$component_statistics)) {
    cli::cli_text("Component Importance (by coefficient of variation):")
    
    cvs <- sapply(info_analysis$component_statistics, function(stat) stat$cv)
    cvs_sorted <- sort(cvs, decreasing = TRUE)
    
    for (comp in names(cvs_sorted)) {
      cli::cli_text("  {comp}: CV = {round(cvs_sorted[comp], 3)}")
    }
  }
  
  # Clustering results
  clustering <- x$information_relationships$metric_clustering
  
  if (!is.null(clustering$clusters)) {
    cli::cli_text("")
    cli::cli_h2("Metric Clustering")
    cli::cli_text("Number of clusters: {clustering$n_clusters}")
    
    for (cluster_id in unique(clustering$clusters)) {
      metrics_in_cluster <- names(clustering$clusters)[clustering$clusters == cluster_id]
      cli::cli_text("Cluster {cluster_id}: {paste(metrics_in_cluster, collapse = ', ')}")
    }
  }
  
  invisible(x)
}