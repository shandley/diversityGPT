#' Null Model Validation for Taxa Indicators
#'
#' @description Comprehensive null model testing framework to validate taxa-component relationships
#' @name null_model_validation
#' @keywords null models statistics
NULL

#' Validate Taxa Indicators with Null Models
#'
#' Tests whether observed taxa-component relationships exceed random expectation
#' using multiple null model algorithms.
#'
#' @param physeq A phyloseq object
#' @param indicators Taxa indicators object (from identify_taxa_drivers)
#' @param null_models Character vector of null models to use:
#'   "row_shuffle", "column_shuffle", "curveball", "phylogenetic"
#' @param n_permutations Number of permutations (default: 999)
#' @param parallel Use parallel processing (default: FALSE)
#' @param n_cores Number of cores for parallel processing
#' @param verbose Print progress messages
#'
#' @return A validated_indicators object containing:
#'   \item{observed}{Original indicators with p-values and effect sizes}
#'   \item{null_distributions}{Null model distributions for each indicator}
#'   \item{summary_statistics}{Summary of validation results}
#'   \item{significant_indicators}{Indicators that pass significance threshold}
#'   \item{null_models_used}{Which null models were applied}
#'
#' @export
#' @examples
#' # Create demo data
#' demo_data <- create_demo_phyloseq()
#' 
#' # Get indicators
#' indicators <- identify_taxa_drivers(demo_data, top_n = 10)
#' 
#' # Validate with null models
#' validated <- validate_taxa_indicators(demo_data, indicators, 
#'                                     null_models = c("row_shuffle", "curveball"),
#'                                     n_permutations = 99)
#' 
#' # View significant indicators
#' print(validated$significant_indicators)
validate_taxa_indicators <- function(physeq,
                                   indicators,
                                   null_models = c("row_shuffle", "curveball"),
                                   n_permutations = 999,
                                   parallel = FALSE,
                                   n_cores = NULL,
                                   verbose = TRUE) {
  
  # Validate inputs
  null_models <- match.arg(null_models, 
                          c("row_shuffle", "column_shuffle", 
                            "curveball", "phylogenetic"),
                          several.ok = TRUE)
  
  if (verbose) {
    message("Validating taxa indicators with ", n_permutations, 
            " permutations using ", length(null_models), " null model(s)")
  }
  
  # Get OTU table
  otu_mat <- as.matrix(phyloseq::otu_table(physeq))
  if (!phyloseq::taxa_are_rows(physeq)) {
    otu_mat <- t(otu_mat)
  }
  
  # Initialize storage for null distributions
  null_distributions <- list()
  
  # Run each null model
  for (null_model in null_models) {
    if (verbose) message("Running ", null_model, " null model...")
    
    null_results <- switch(null_model,
      "row_shuffle" = run_row_shuffle_null(physeq, indicators, n_permutations, parallel, n_cores),
      "column_shuffle" = run_column_shuffle_null(physeq, indicators, n_permutations, parallel, n_cores),
      "curveball" = run_curveball_null(physeq, indicators, n_permutations, parallel, n_cores),
      "phylogenetic" = run_phylogenetic_null(physeq, indicators, n_permutations, parallel, n_cores)
    )
    
    null_distributions[[null_model]] <- null_results
  }
  
  # Calculate p-values and effect sizes
  if (verbose) message("Calculating significance and effect sizes...")
  validation_results <- calculate_significance(indicators, null_distributions)
  
  # Identify significant indicators
  significant_indicators <- extract_significant_indicators(validation_results, 
                                                         alpha = 0.05)
  
  # Create summary statistics
  summary_stats <- summarize_validation(validation_results, null_models)
  
  # Create output object
  results <- list(
    observed = validation_results,
    null_distributions = null_distributions,
    summary_statistics = summary_stats,
    significant_indicators = significant_indicators,
    null_models_used = null_models,
    n_permutations = n_permutations,
    call = match.call()
  )
  
  class(results) <- c("validated_indicators", "list")
  return(results)
}

#' Row Shuffle Null Model
#'
#' Randomizes taxon occurrences while maintaining row sums (total abundances)
#'
#' @param physeq Phyloseq object
#' @param indicators Original indicators
#' @param n_perm Number of permutations
#' @param parallel Use parallel processing
#' @param n_cores Number of cores
#'
#' @return List of null indicator distributions
#' @keywords internal
run_row_shuffle_null <- function(physeq, indicators, n_perm, parallel, n_cores) {
  
  otu_mat <- as.matrix(phyloseq::otu_table(physeq))
  if (!phyloseq::taxa_are_rows(physeq)) {
    otu_mat <- t(otu_mat)
  }
  
  # Function to run single permutation
  single_perm <- function(i) {
    # Shuffle within each row (maintaining taxon abundances)
    null_otu <- t(apply(otu_mat, 1, sample))
    colnames(null_otu) <- colnames(otu_mat)
    
    # Create null phyloseq
    null_physeq <- phyloseq::phyloseq(
      phyloseq::otu_table(null_otu, taxa_are_rows = TRUE),
      phyloseq::sample_data(physeq),
      phyloseq::tax_table(physeq)
    )
    
    # Add tree if present
    if (!is.null(phyloseq::phy_tree(physeq, errorIfNULL = FALSE))) {
      phyloseq::phy_tree(null_physeq) <- phyloseq::phy_tree(physeq)
    }
    
    # Calculate indicators for null community
    null_indicators <- identify_taxa_drivers(null_physeq, 
                                           top_n = nrow(indicators$richness_drivers),
                                           method = indicators$method,
                                           verbose = FALSE)
    
    # Extract contribution values
    extract_contribution_values(null_indicators)
  }
  
  # Run permutations
  if (parallel && !is.null(n_cores)) {
    null_results <- parallel::mclapply(1:n_perm, single_perm, 
                                     mc.cores = n_cores)
  } else {
    null_results <- lapply(1:n_perm, single_perm)
  }
  
  return(null_results)
}

#' Column Shuffle Null Model
#'
#' Randomizes samples while maintaining column sums (sample totals)
#'
#' @inheritParams run_row_shuffle_null
#' @return List of null indicator distributions
#' @keywords internal
run_column_shuffle_null <- function(physeq, indicators, n_perm, parallel, n_cores) {
  
  otu_mat <- as.matrix(phyloseq::otu_table(physeq))
  if (!phyloseq::taxa_are_rows(physeq)) {
    otu_mat <- t(otu_mat)
  }
  
  single_perm <- function(i) {
    # Shuffle within each column (maintaining sample totals)
    null_otu <- apply(otu_mat, 2, sample)
    rownames(null_otu) <- rownames(otu_mat)
    
    # Create null phyloseq
    null_physeq <- phyloseq::phyloseq(
      phyloseq::otu_table(null_otu, taxa_are_rows = TRUE),
      phyloseq::sample_data(physeq),
      phyloseq::tax_table(physeq)
    )
    
    if (!is.null(phyloseq::phy_tree(physeq, errorIfNULL = FALSE))) {
      phyloseq::phy_tree(null_physeq) <- phyloseq::phy_tree(physeq)
    }
    
    # Calculate indicators
    null_indicators <- identify_taxa_drivers(null_physeq,
                                           top_n = nrow(indicators$richness_drivers),
                                           method = indicators$method,
                                           verbose = FALSE)
    
    extract_contribution_values(null_indicators)
  }
  
  # Run permutations
  if (parallel && !is.null(n_cores)) {
    null_results <- parallel::mclapply(1:n_perm, single_perm, mc.cores = n_cores)
  } else {
    null_results <- lapply(1:n_perm, single_perm)
  }
  
  return(null_results)
}

#' Curveball Null Model
#'
#' Maintains both row and column sums using the curveball algorithm
#'
#' @inheritParams run_row_shuffle_null
#' @return List of null indicator distributions
#' @keywords internal
run_curveball_null <- function(physeq, indicators, n_perm, parallel, n_cores) {
  
  otu_mat <- as.matrix(phyloseq::otu_table(physeq))
  if (!phyloseq::taxa_are_rows(physeq)) {
    otu_mat <- t(otu_mat)
  }
  
  # Convert to presence/absence for curveball
  pa_mat <- otu_mat > 0
  
  single_perm <- function(i) {
    # Run curveball algorithm
    null_pa <- curveball_randomize(pa_mat)
    
    # Apply original abundances to randomized occurrences
    null_otu <- otu_mat
    null_otu[!null_pa] <- 0
    
    # Redistribute abundances proportionally where present
    for (j in 1:ncol(null_otu)) {
      present_orig <- pa_mat[, j]
      present_null <- null_pa[, j]
      
      if (sum(present_null) > 0 && sum(present_orig) > 0) {
        # Get abundances from original present taxa
        orig_abundances <- otu_mat[present_orig, j]
        # Randomly assign to null present taxa
        null_otu[present_null, j] <- sample(orig_abundances, 
                                           sum(present_null), 
                                           replace = TRUE)
      }
    }
    
    # Create null phyloseq
    null_physeq <- phyloseq::phyloseq(
      phyloseq::otu_table(null_otu, taxa_are_rows = TRUE),
      phyloseq::sample_data(physeq),
      phyloseq::tax_table(physeq)
    )
    
    if (!is.null(phyloseq::phy_tree(physeq, errorIfNULL = FALSE))) {
      phyloseq::phy_tree(null_physeq) <- phyloseq::phy_tree(physeq)
    }
    
    # Calculate indicators
    null_indicators <- identify_taxa_drivers(null_physeq,
                                           top_n = nrow(indicators$richness_drivers),
                                           method = indicators$method,
                                           verbose = FALSE)
    
    extract_contribution_values(null_indicators)
  }
  
  # Run permutations
  if (parallel && !is.null(n_cores)) {
    null_results <- parallel::mclapply(1:n_perm, single_perm, mc.cores = n_cores)
  } else {
    null_results <- lapply(1:n_perm, single_perm)
  }
  
  return(null_results)
}

#' Curveball Randomization Algorithm
#'
#' Implements the curveball algorithm for maintaining row and column sums
#'
#' @param pa_mat Presence/absence matrix
#' @param n_swaps Number of swap attempts (default: 5 * number of edges)
#'
#' @return Randomized presence/absence matrix
#' @keywords internal
curveball_randomize <- function(pa_mat, n_swaps = NULL) {
  if (is.null(n_swaps)) {
    n_swaps <- 5 * sum(pa_mat)
  }
  
  # Work with a copy
  result <- pa_mat
  n_rows <- nrow(result)
  n_cols <- ncol(result)
  
  for (i in 1:n_swaps) {
    # Select two random rows and columns
    rows <- sample(n_rows, 2)
    cols <- sample(n_cols, 2)
    
    # Check if we have a checkboard pattern
    # Pattern 1: 1 0
    #           0 1
    if (result[rows[1], cols[1]] && !result[rows[1], cols[2]] &&
        !result[rows[2], cols[1]] && result[rows[2], cols[2]]) {
      # Swap to: 0 1
      #          1 0
      result[rows[1], cols[1]] <- FALSE
      result[rows[1], cols[2]] <- TRUE
      result[rows[2], cols[1]] <- TRUE
      result[rows[2], cols[2]] <- FALSE
    }
    # Pattern 2: 0 1
    #           1 0
    else if (!result[rows[1], cols[1]] && result[rows[1], cols[2]] &&
             result[rows[2], cols[1]] && !result[rows[2], cols[2]]) {
      # Swap to: 1 0
      #          0 1
      result[rows[1], cols[1]] <- TRUE
      result[rows[1], cols[2]] <- FALSE
      result[rows[2], cols[1]] <- FALSE
      result[rows[2], cols[2]] <- TRUE
    }
  }
  
  return(result)
}

#' Phylogenetic Null Model
#'
#' Shuffles taxa labels on phylogenetic tree
#'
#' @inheritParams run_row_shuffle_null
#' @return List of null indicator distributions
#' @keywords internal
run_phylogenetic_null <- function(physeq, indicators, n_perm, parallel, n_cores) {
  
  # Check if tree exists
  if (is.null(phyloseq::phy_tree(physeq, errorIfNULL = FALSE))) {
    warning("No phylogenetic tree found. Skipping phylogenetic null model.")
    return(NULL)
  }
  
  tree <- phyloseq::phy_tree(physeq)
  otu_mat <- as.matrix(phyloseq::otu_table(physeq))
  if (!phyloseq::taxa_are_rows(physeq)) {
    otu_mat <- t(otu_mat)
  }
  
  single_perm <- function(i) {
    # Shuffle tip labels on tree
    shuffled_tips <- sample(tree$tip.label)
    
    # Create new OTU table with shuffled taxa
    null_otu <- otu_mat
    rownames(null_otu) <- shuffled_tips[match(rownames(otu_mat), tree$tip.label)]
    
    # Create null phyloseq
    null_physeq <- phyloseq::phyloseq(
      phyloseq::otu_table(null_otu, taxa_are_rows = TRUE),
      phyloseq::sample_data(physeq),
      phyloseq::tax_table(physeq)
    )
    
    phyloseq::phy_tree(null_physeq) <- tree
    
    # Calculate indicators
    null_indicators <- identify_taxa_drivers(null_physeq,
                                           top_n = nrow(indicators$richness_drivers),
                                           method = indicators$method,
                                           verbose = FALSE)
    
    extract_contribution_values(null_indicators)
  }
  
  # Run permutations
  if (parallel && !is.null(n_cores)) {
    null_results <- parallel::mclapply(1:n_perm, single_perm, mc.cores = n_cores)
  } else {
    null_results <- lapply(1:n_perm, single_perm)
  }
  
  return(null_results)
}

#' Extract Contribution Values
#'
#' Extracts contribution values from indicators for null model comparison
#'
#' @param indicators Taxa indicators object
#' @return Named list of contribution vectors
#' @keywords internal
extract_contribution_values <- function(indicators) {
  list(
    richness = indicators$richness_drivers$contribution,
    evenness = indicators$evenness_drivers$contribution,
    phylogenetic = indicators$phylogenetic_drivers$contribution,
    spatial = indicators$spatial_drivers$contribution
  )
}

#' Calculate Significance from Null Distributions
#'
#' Calculates p-values and effect sizes by comparing observed to null distributions
#'
#' @param indicators Original indicators
#' @param null_distributions List of null model results
#' @return Updated indicators with p-values and effect sizes
#' @keywords internal
calculate_significance <- function(indicators, null_distributions) {
  
  # Process each component
  components <- c("richness", "evenness", "phylogenetic", "spatial")
  
  for (component in components) {
    comp_name <- paste0(component, "_drivers")
    if (!is.null(indicators[[comp_name]])) {
      
      observed_values <- indicators[[comp_name]]$contribution
      n_taxa <- length(observed_values)
      
      # Initialize p-values and effect sizes
      p_values <- numeric(n_taxa)
      effect_sizes <- numeric(n_taxa)
      
      # For each taxon
      for (i in 1:n_taxa) {
        obs_val <- observed_values[i]
        
        # Collect null values across all null models
        null_vals <- numeric()
        
        for (null_model in names(null_distributions)) {
          if (!is.null(null_distributions[[null_model]])) {
            # Extract values for this component and position
            model_nulls <- sapply(null_distributions[[null_model]], 
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
          # Calculate two-tailed p-value
          p_values[i] <- (sum(abs(null_vals) >= abs(obs_val)) + 1) / 
                        (length(null_vals) + 1)
          
          # Calculate effect size (standardized)
          effect_sizes[i] <- (obs_val - mean(null_vals)) / sd(null_vals)
        } else {
          p_values[i] <- NA
          effect_sizes[i] <- NA
        }
      }
      
      # Add to indicators
      indicators[[comp_name]]$p_value <- p_values
      indicators[[comp_name]]$effect_size <- effect_sizes
    }
  }
  
  return(indicators)
}

#' Extract Significant Indicators
#'
#' Filters indicators to keep only statistically significant ones
#'
#' @param indicators Indicators with p-values
#' @param alpha Significance threshold (default: 0.05)
#' @return List of significant indicators per component
#' @keywords internal
extract_significant_indicators <- function(indicators, alpha = 0.05) {
  
  significant <- list()
  
  components <- c("richness", "evenness", "phylogenetic", "spatial")
  
  for (component in components) {
    comp_name <- paste0(component, "_drivers")
    if (!is.null(indicators[[comp_name]]) && "p_value" %in% names(indicators[[comp_name]])) {
      comp_df <- indicators[[comp_name]]
      sig_rows <- which(comp_df$p_value <= alpha)
      
      if (length(sig_rows) > 0) {
        significant[[comp_name]] <- comp_df[sig_rows, ]
      } else {
        significant[[comp_name]] <- data.frame()
      }
    }
  }
  
  return(significant)
}

#' Summarize Validation Results
#'
#' Creates summary statistics from validation
#'
#' @param validation_results Validated indicators
#' @param null_models Models used
#' @return Summary list
#' @keywords internal
summarize_validation <- function(validation_results, null_models) {
  
  summary_stats <- list(
    null_models_used = null_models,
    validation_date = Sys.Date()
  )
  
  # Summarize each component
  components <- c("richness", "evenness", "phylogenetic", "spatial")
  
  for (component in components) {
    comp_name <- paste0(component, "_drivers")
    if (!is.null(validation_results[[comp_name]]) && 
        "p_value" %in% names(validation_results[[comp_name]])) {
      
      comp_df <- validation_results[[comp_name]]
      
      summary_stats[[component]] <- list(
        n_total = nrow(comp_df),
        n_significant = sum(comp_df$p_value <= 0.05, na.rm = TRUE),
        prop_significant = mean(comp_df$p_value <= 0.05, na.rm = TRUE),
        mean_effect_size = mean(abs(comp_df$effect_size), na.rm = TRUE),
        max_effect_size = max(abs(comp_df$effect_size), na.rm = TRUE)
      )
    }
  }
  
  return(summary_stats)
}

#' Print Validated Indicators
#'
#' @param x A validated_indicators object
#' @param ... Additional arguments
#' @export
print.validated_indicators <- function(x, ...) {
  cat("Validated Taxa Indicators\n")
  cat("========================\n")
  cat("Null models used:", paste(x$null_models_used, collapse = ", "), "\n")
  cat("Permutations:", x$n_permutations, "\n\n")
  
  # Summary by component
  components <- c("richness", "evenness", "phylogenetic", "spatial")
  
  for (comp in components) {
    if (!is.null(x$summary_statistics[[comp]])) {
      stats <- x$summary_statistics[[comp]]
      cat(toupper(comp), "COMPONENT:\n")
      cat(sprintf("  Significant indicators: %d/%d (%.1f%%)\n",
                 stats$n_significant, stats$n_total,
                 stats$prop_significant * 100))
      cat(sprintf("  Mean effect size: %.2f\n", stats$mean_effect_size))
      cat("\n")
    }
  }
  
  invisible(x)
}

#' Plot Null Model Validation Results
#'
#' @param x A validated_indicators object
#' @param type Plot type: "pvalues", "effects", "null_dist"
#' @param component Which component to plot
#' @param ... Additional arguments
#' @export
plot.validated_indicators <- function(x, 
                                    type = c("pvalues", "effects", "null_dist"),
                                    component = "richness",
                                    ...) {
  
  type <- match.arg(type)
  
  if (type == "pvalues") {
    plot_validation_pvalues(x, component)
  } else if (type == "effects") {
    plot_validation_effects(x, component)
  } else if (type == "null_dist") {
    plot_null_distributions(x, component)
  }
}