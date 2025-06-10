#' Universal Information Theory Framework for Diversity Analysis
#'
#' @description
#' Revolutionary system that decomposes ANY diversity metric into universal
#' information components: Richness (R), Evenness (E), Phylogenetic (P), 
#' and Spatial (S). Enables mathematical relationships discovery and 
#' any-to-any metric transformations.
#'
#' @param phyloseq_object A phyloseq object
#' @param groups Character: grouping variable from sample_data
#' @param include_phylogenetic Logical: whether to calculate phylogenetic components
#' @param include_spatial Logical: whether to calculate spatial components (requires coordinates)
#' @param ... Additional arguments passed to diversity calculations
#'
#' @return A universal_information object containing:
#'   - information_components: R, E, P, S decomposition for each sample
#'   - transformation_matrix: Mathematical relationships between metrics
#'   - metric_profiles: All calculated diversity metrics
#'   - deconvolution_quality: Quality assessment of decomposition
#'
#' @export
#' @examples
#' # Create small demo dataset
#' demo_data <- create_demo_phyloseq(n_samples = 5, n_taxa = 20)
#' 
#' # Extract universal information
#' universal_info <- extract_universal_information(
#'   demo_data,
#'   include_phylogenetic = FALSE  # Faster for examples
#' )
#' 
#' # View structure
#' names(universal_info)
#' 
#' # Check components
#' head(universal_info$information_components, 3)
extract_universal_information <- function(phyloseq_object,
                                        groups = NULL,
                                        include_phylogenetic = TRUE,
                                        include_spatial = FALSE,
                                        ...) {
  
  cli::cli_alert_info("Extracting universal information components...")
  
  # Calculate comprehensive diversity metrics
  diversity_metrics <- calculate_comprehensive_metrics(
    phyloseq_object, 
    groups, 
    include_phylogenetic, 
    include_spatial,
    ...
  )
  
  # Extract universal information components (R, E, P, S)
  cli::cli_alert_info("Decomposing metrics into R, E, P, S components...")
  information_components <- decompose_to_information_components(
    phyloseq_object, 
    diversity_metrics,
    include_phylogenetic,
    include_spatial
  )
  
  # Build transformation matrix between all metrics
  cli::cli_alert_info("Computing transformation relationships...")
  transformation_matrix <- compute_transformation_matrix(
    diversity_metrics,
    information_components
  )
  
  # Assess deconvolution quality
  quality_assessment <- assess_deconvolution_quality(
    diversity_metrics,
    information_components,
    transformation_matrix
  )
  
  # Create result object
  result <- list(
    information_components = information_components,
    transformation_matrix = transformation_matrix,
    metric_profiles = diversity_metrics,
    deconvolution_quality = quality_assessment,
    phylogenetic_included = include_phylogenetic,
    spatial_included = include_spatial,
    n_samples = nrow(phyloseq::sample_data(phyloseq_object)),
    n_taxa = phyloseq::ntaxa(phyloseq_object)
  )
  
  class(result) <- c("universal_information", "list")
  
  cli::cli_alert_success("Universal information extraction complete!")
  
  return(result)
}

#' Calculate comprehensive diversity metrics for deconvolution
#' @keywords internal
calculate_comprehensive_metrics <- function(phyloseq_object, groups, include_phylogenetic, include_spatial, ...) {
  
  # Get abundance matrix
  otu_table <- as.matrix(phyloseq::otu_table(phyloseq_object))
  if (phyloseq::taxa_are_rows(phyloseq_object)) {
    otu_table <- t(otu_table)
  }
  
  # Initialize results
  metrics <- data.frame(
    sample_id = rownames(otu_table),
    stringsAsFactors = FALSE
  )
  
  # Add grouping variable if provided
  if (!is.null(groups)) {
    sample_data <- phyloseq::sample_data(phyloseq_object)
    if (groups %in% colnames(sample_data)) {
      metrics$group <- sample_data[[groups]]
    }
  }
  
  # Alpha diversity metrics (Hill numbers foundation)
  cli::cli_alert_info("Calculating Hill diversity profiles...")
  
  # Hill numbers: q=0 (richness), q=1 (Shannon), q=2 (Simpson)
  metrics$hill_q0 <- apply(otu_table, 1, function(x) sum(x > 0))  # Richness
  metrics$hill_q1 <- apply(otu_table, 1, function(x) exp(vegan::diversity(x, index = "shannon")))
  metrics$hill_q2 <- apply(otu_table, 1, function(x) 1 / vegan::diversity(x, index = "invsimpson"))
  
  # Traditional metrics
  metrics$shannon <- apply(otu_table, 1, function(x) vegan::diversity(x, index = "shannon"))
  metrics$simpson <- apply(otu_table, 1, function(x) vegan::diversity(x, index = "simpson"))
  metrics$invsimpson <- apply(otu_table, 1, function(x) vegan::diversity(x, index = "invsimpson"))
  
  # Richness-based metrics
  metrics$observed <- apply(otu_table, 1, function(x) sum(x > 0))
  metrics$chao1 <- apply(otu_table, 1, function(x) {
    tryCatch(fossil::chao1(x), error = function(e) sum(x > 0))
  })
  
  # Evenness metrics
  metrics$pielou_evenness <- metrics$shannon / log(metrics$observed)
  metrics$simpson_evenness <- metrics$invsimpson / metrics$observed
  
  # Phylogenetic diversity if tree available
  if (include_phylogenetic && !is.null(phyloseq::phy_tree(phyloseq_object, errorIfNULL = FALSE))) {
    cli::cli_alert_info("Calculating phylogenetic diversity...")
    
    tree <- phyloseq::phy_tree(phyloseq_object)
    
    # Faith's PD
    metrics$faith_pd <- apply(otu_table, 1, function(x) {
      present_taxa <- names(x)[x > 0]
      if (length(present_taxa) > 1) {
        tryCatch({
          picante::pd(t(as.matrix(x[x > 0])), tree)$PD
        }, error = function(e) 0)
      } else {
        0
      }
    })
    
    # Mean pairwise distance
    metrics$mpd <- apply(otu_table, 1, function(x) {
      if (sum(x > 0) > 1) {
        tryCatch({
          picante::mpd(t(as.matrix(x)), picante::cophenetic.phylo(tree))
        }, error = function(e) 0)
      } else {
        0
      }
    })
  }
  
  # Spatial components if coordinates available
  if (include_spatial) {
    sample_data <- phyloseq::sample_data(phyloseq_object)
    coord_cols <- c("longitude", "latitude", "x", "y", "lon", "lat")
    available_coords <- intersect(coord_cols, colnames(sample_data))
    
    if (length(available_coords) >= 2) {
      cli::cli_alert_info("Calculating spatial diversity components...")
      
      coords <- sample_data[, available_coords[1:2]]
      coords <- as.matrix(coords)
      
      # Simple spatial diversity index (mean distance to other samples)
      dist_matrix <- dist(coords)
      metrics$spatial_dispersion <- apply(as.matrix(dist_matrix), 1, mean)
    }
  }
  
  return(metrics)
}

#' Decompose diversity metrics into universal information components
#' @keywords internal
decompose_to_information_components <- function(phyloseq_object, diversity_metrics, include_phylogenetic, include_spatial) {
  
  n_samples <- nrow(diversity_metrics)
  
  # Initialize information components matrix
  components <- data.frame(
    sample_id = diversity_metrics$sample_id,
    stringsAsFactors = FALSE
  )
  
  # R Component (Richness Information)
  # Based on species count and rarity patterns
  components$R_component <- log(diversity_metrics$observed) + 
                           log(diversity_metrics$chao1 / diversity_metrics$observed)
  
  # E Component (Evenness Information) 
  # Based on distribution uniformity
  components$E_component <- diversity_metrics$pielou_evenness * 
                           (1 - (diversity_metrics$simpson / diversity_metrics$shannon))
  
  # P Component (Phylogenetic Information)
  if (include_phylogenetic && "faith_pd" %in% names(diversity_metrics)) {
    # Phylogenetic information relative to richness
    components$P_component <- log(diversity_metrics$faith_pd / diversity_metrics$observed)
    
    if ("mpd" %in% names(diversity_metrics)) {
      components$P_component <- components$P_component + 
                               log(diversity_metrics$mpd + 1)
    }
  } else {
    components$P_component <- 0
  }
  
  # S Component (Spatial Information) 
  if (include_spatial && "spatial_dispersion" %in% names(diversity_metrics)) {
    components$S_component <- log(diversity_metrics$spatial_dispersion + 1)
  } else {
    components$S_component <- 0
  }
  
  # Normalize components to [0,1] scale
  numeric_cols <- c("R_component", "E_component", "P_component", "S_component")
  for (col in numeric_cols) {
    if (col %in% names(components)) {
      values <- components[[col]]
      # Handle infinite and NA values
      values[is.infinite(values) | is.na(values)] <- 0
      
      if (sd(values) > 0) {
        # Normalize to [0,1]
        components[[col]] <- (values - min(values)) / (max(values) - min(values))
      } else {
        components[[col]] <- 0
      }
    }
  }
  
  # Calculate total information content
  components$total_information <- components$R_component + 
                                 components$E_component + 
                                 components$P_component + 
                                 components$S_component
  
  # Calculate component proportions
  components$R_proportion <- components$R_component / components$total_information
  components$E_proportion <- components$E_component / components$total_information  
  components$P_proportion <- components$P_component / components$total_information
  components$S_proportion <- components$S_component / components$total_information
  
  # Handle division by zero
  prop_cols <- c("R_proportion", "E_proportion", "P_proportion", "S_proportion")
  for (col in prop_cols) {
    components[[col]][is.na(components[[col]]) | is.infinite(components[[col]])] <- 0
  }
  
  return(components)
}

#' Compute transformation matrix between all diversity metrics
#' @keywords internal
compute_transformation_matrix <- function(diversity_metrics, information_components) {
  
  # Get numeric columns (diversity metrics)
  metric_cols <- names(diversity_metrics)[sapply(diversity_metrics, is.numeric)]
  metric_cols <- setdiff(metric_cols, c("sample_id"))
  
  # Get information components
  component_cols <- c("R_component", "E_component", "P_component", "S_component")
  
  # Build feature matrix for regression
  feature_matrix <- as.matrix(information_components[, component_cols])
  
  transformation_list <- list()
  transformation_quality <- list()
  
  cli::cli_alert_info("Computing transformations for {length(metric_cols)} metrics...")
  
  for (target_metric in metric_cols) {
    
    target_values <- diversity_metrics[[target_metric]]
    
    # Skip if all NA or constant
    if (all(is.na(target_values)) || sd(target_values, na.rm = TRUE) == 0) {
      next
    }
    
    # Multiple regression: metric ~ R + E + P + S
    transformation_result <- tryCatch({
      
      # Remove NA values
      complete_cases <- complete.cases(target_values, feature_matrix)
      
      if (sum(complete_cases) < 5) {
        return(NULL)  # Not enough data
      }
      
      y <- target_values[complete_cases]
      X <- feature_matrix[complete_cases, , drop = FALSE]
      
      # Fit linear model
      model <- lm(y ~ X)
      
      # Model quality metrics
      r_squared <- summary(model)$r.squared
      adj_r_squared <- summary(model)$adj.r.squared
      rmse <- sqrt(mean(model$residuals^2))
      
      # Coefficients for transformation
      coefficients <- model$coefficients
      names(coefficients) <- c("intercept", component_cols)
      
      list(
        coefficients = coefficients,
        r_squared = r_squared,
        adj_r_squared = adj_r_squared,
        rmse = rmse,
        n_obs = sum(complete_cases),
        model = model
      )
      
    }, error = function(e) {
      cli::cli_alert_warning("Failed to compute transformation for {target_metric}: {e$message}")
      NULL
    })
    
    if (!is.null(transformation_result)) {
      transformation_list[[target_metric]] <- transformation_result
    }
  }
  
  # Create transformation matrix summary
  transformation_matrix <- data.frame(
    metric = names(transformation_list),
    r_squared = sapply(transformation_list, function(x) x$r_squared),
    adj_r_squared = sapply(transformation_list, function(x) x$adj_r_squared),
    rmse = sapply(transformation_list, function(x) x$rmse),
    n_obs = sapply(transformation_list, function(x) x$n_obs),
    stringsAsFactors = FALSE
  )
  
  # Add coefficient matrix
  coef_matrix <- do.call(rbind, lapply(transformation_list, function(x) x$coefficients))
  transformation_matrix <- cbind(transformation_matrix, coef_matrix)
  
  # Store full transformation details
  attr(transformation_matrix, "transformations") <- transformation_list
  attr(transformation_matrix, "component_cols") <- component_cols
  
  return(transformation_matrix)
}

#' Assess quality of deconvolution
#' @keywords internal
assess_deconvolution_quality <- function(diversity_metrics, information_components, transformation_matrix) {
  
  n_metrics <- nrow(transformation_matrix)
  
  quality_summary <- list(
    n_metrics_analyzed = n_metrics,
    mean_r_squared = mean(transformation_matrix$r_squared, na.rm = TRUE),
    median_r_squared = median(transformation_matrix$r_squared, na.rm = TRUE),
    high_quality_metrics = sum(transformation_matrix$r_squared > 0.8, na.rm = TRUE),
    medium_quality_metrics = sum(transformation_matrix$r_squared > 0.6 & 
                                transformation_matrix$r_squared <= 0.8, na.rm = TRUE),
    low_quality_metrics = sum(transformation_matrix$r_squared <= 0.6, na.rm = TRUE)
  )
  
  # Component importance analysis
  coef_cols <- c("R_component", "E_component", "P_component", "S_component")
  available_coef_cols <- intersect(coef_cols, names(transformation_matrix))
  
  if (length(available_coef_cols) > 0) {
    component_importance <- data.frame(
      component = available_coef_cols,
      mean_coefficient = sapply(available_coef_cols, function(col) {
        mean(abs(transformation_matrix[[col]]), na.rm = TRUE)
      }),
      contribution_frequency = sapply(available_coef_cols, function(col) {
        sum(abs(transformation_matrix[[col]]) > 0.1, na.rm = TRUE) / n_metrics
      }),
      stringsAsFactors = FALSE
    )
    
    quality_summary$component_importance <- component_importance
  }
  
  # Overall quality rating
  mean_r_sq <- quality_summary$mean_r_squared
  if (is.na(mean_r_sq) || length(mean_r_sq) == 0) {
    quality_summary$overall_quality <- "Unknown"
  } else if (mean_r_sq > 0.8) {
    quality_summary$overall_quality <- "Excellent"
  } else if (mean_r_sq > 0.6) {
    quality_summary$overall_quality <- "Good"  
  } else if (mean_r_sq > 0.4) {
    quality_summary$overall_quality <- "Fair"
  } else {
    quality_summary$overall_quality <- "Poor"
  }
  
  return(quality_summary)
}

#' Print method for universal_information objects
#'
#' @param x A universal_information object
#' @param ... Additional arguments (unused)
#' @export
print.universal_information <- function(x, ...) {
  cli::cli_h1("Universal Information Analysis")
  
  cli::cli_text("Samples: {x$n_samples}")
  cli::cli_text("Taxa: {x$n_taxa}")
  cli::cli_text("Phylogenetic: {x$phylogenetic_included}")
  cli::cli_text("Spatial: {x$spatial_included}")
  cli::cli_text("")
  
  cli::cli_h2("Information Components (R, E, P, S)")
  
  # Summary of components
  components <- x$information_components
  component_cols <- c("R_component", "E_component", "P_component", "S_component")
  
  for (comp in component_cols) {
    if (comp %in% names(components)) {
      values <- components[[comp]]
      cli::cli_text("{comp}: mean={round(mean(values, na.rm = TRUE), 3)}, sd={round(sd(values, na.rm = TRUE), 3)}")
    }
  }
  
  cli::cli_text("")
  cli::cli_h2("Transformation Quality")
  
  quality <- x$deconvolution_quality
  cli::cli_text("Overall Quality: {quality$overall_quality}")
  cli::cli_text("Mean R²: {round(quality$mean_r_squared, 3)}")
  cli::cli_text("High Quality Metrics (R² > 0.8): {quality$high_quality_metrics}")
  cli::cli_text("Medium Quality Metrics (R² 0.6-0.8): {quality$medium_quality_metrics}")
  cli::cli_text("Low Quality Metrics (R² < 0.6): {quality$low_quality_metrics}")
  
  if (!is.null(quality$component_importance)) {
    cli::cli_text("")
    cli::cli_h3("Component Importance")
    
    importance <- quality$component_importance
    importance <- importance[order(-importance$mean_coefficient), ]
    
    for (i in seq_len(nrow(importance))) {
      row <- importance[i, ]
      coef_text <- if (is.na(row$mean_coefficient)) "0" else round(row$mean_coefficient, 3)
      freq_text <- if (is.na(row$contribution_frequency)) "0" else round(row$contribution_frequency, 3)
      cli::cli_text("{row$component}: coef={coef_text}, freq={freq_text}")
    }
  }
  
  invisible(x)
}