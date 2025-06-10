#' Bootstrap Uncertainty Quantification for Taxa Indicators
#'
#' @description Provides bootstrap confidence intervals and uncertainty metrics
#' for all taxa indicator analyses
#' @name bootstrap_uncertainty
#' @keywords bootstrap, uncertainty, confidence intervals
NULL

#' Bootstrap Confidence Intervals for Taxa Indicators
#'
#' Calculates bootstrap confidence intervals for all taxa indicator methods:
#' null model validation, mutual information, and Shapley values. This provides
#' rigorous uncertainty quantification for indicator rankings and allows
#' assessment of statistical robustness.
#'
#' @param physeq A phyloseq object
#' @param components Universal information components (output from extract_universal_information)
#' @param indicator_methods Methods to bootstrap: "null_models", "mutual_info", "shapley"
#' @param n_bootstrap Number of bootstrap replicates (default: 999)
#' @param confidence_level Confidence level for intervals (default: 0.95)
#' @param bootstrap_method Bootstrap sampling method: "parametric", "nonparametric", "block"
#' @param stratify_by Variable to stratify bootstrap sampling (optional)
#' @param parallel Use parallel processing for bootstrap
#' @param seed Random seed for reproducibility
#' @param verbose Print progress messages
#'
#' @return A bootstrap_indicators object containing:
#'   \item{confidence_intervals}{CI matrices for each method and component}
#'   \item{bootstrap_distributions}{Full bootstrap distributions}
#'   \item{uncertainty_metrics}{Variance, CV, stability indices}
#'   \item{consensus_rankings}{Combined rankings with uncertainty}
#'   \item{reliability_assessment}{Bootstrap reliability metrics}
#'   \item{method_comparison}{Comparison across indicator methods}
#'
#' @export
#' @examples
#' # Create demo data
#' demo_data <- create_demo_phyloseq()
#' 
#' # Extract universal information
#' info <- extract_universal_information(demo_data)
#' 
#' # Bootstrap all indicator methods
#' boot_results <- bootstrap_taxa_indicators(
#'   demo_data, info,
#'   indicator_methods = c("mutual_info", "shapley"),
#'   n_bootstrap = 199,
#'   confidence_level = 0.95
#' )
#' 
#' # View uncertainty metrics
#' print(boot_results)
#' 
#' # Plot confidence intervals
#' plot(boot_results, type = "confidence_intervals")
bootstrap_taxa_indicators <- function(physeq,
                                    components = NULL,
                                    indicator_methods = c("null_models", "mutual_info", "shapley"),
                                    n_bootstrap = 999,
                                    confidence_level = 0.95,
                                    bootstrap_method = c("nonparametric", "parametric", "block"),
                                    stratify_by = NULL,
                                    parallel = FALSE,
                                    seed = 123,
                                    verbose = TRUE) {
  
  bootstrap_method <- match.arg(bootstrap_method)
  
  # Set seed for reproducibility
  set.seed(seed)
  
  # Extract components if not provided
  if (is.null(components)) {
    if (verbose) message("Extracting universal information components...")
    components <- extract_universal_information(physeq)
  }
  
  if (verbose) {
    message("Bootstrap uncertainty analysis with ", n_bootstrap, " replicates")
    message("Methods: ", paste(indicator_methods, collapse = ", "))
    message("Bootstrap method: ", bootstrap_method)
  }
  
  # Initialize results storage
  bootstrap_results <- list()
  confidence_intervals <- list()
  uncertainty_metrics <- list()
  
  # Bootstrap each indicator method
  for (method in indicator_methods) {
    if (verbose) message("Bootstrapping ", method, " indicators...")
    
    if (method == "null_models") {
      method_results <- bootstrap_null_model_indicators(
        physeq, components, n_bootstrap, bootstrap_method, 
        stratify_by, parallel, verbose
      )
    } else if (method == "mutual_info") {
      method_results <- bootstrap_mutual_information_indicators(
        physeq, components, n_bootstrap, bootstrap_method,
        stratify_by, parallel, verbose
      )
    } else if (method == "shapley") {
      method_results <- bootstrap_shapley_indicators(
        physeq, components, n_bootstrap, bootstrap_method,
        stratify_by, parallel, verbose
      )
    }
    
    bootstrap_results[[method]] <- method_results$distributions
    confidence_intervals[[method]] <- calculate_confidence_intervals(
      method_results$distributions, confidence_level
    )
    uncertainty_metrics[[method]] <- calculate_uncertainty_metrics(
      method_results$distributions, method
    )
  }
  
  # Create consensus rankings with uncertainty
  consensus_rankings <- create_consensus_rankings_with_uncertainty(
    bootstrap_results, confidence_intervals, indicator_methods
  )
  
  # Assess reliability across methods
  reliability_assessment <- assess_bootstrap_reliability(
    bootstrap_results, uncertainty_metrics
  )
  
  # Compare methods
  method_comparison <- compare_indicator_methods(
    bootstrap_results, uncertainty_metrics
  )
  
  # Create results object
  results <- list(
    confidence_intervals = confidence_intervals,
    bootstrap_distributions = bootstrap_results,
    uncertainty_metrics = uncertainty_metrics,
    consensus_rankings = consensus_rankings,
    reliability_assessment = reliability_assessment,
    method_comparison = method_comparison,
    n_bootstrap = n_bootstrap,
    confidence_level = confidence_level,
    bootstrap_method = bootstrap_method,
    indicator_methods = indicator_methods,
    call = match.call()
  )
  
  class(results) <- c("bootstrap_indicators", "list")
  return(results)
}

#' Bootstrap Null Model Indicators
#'
#' Bootstrap confidence intervals for null model validation results
#'
#' @param physeq Phyloseq object
#' @param components Universal information components
#' @param n_bootstrap Number of bootstrap replicates
#' @param bootstrap_method Bootstrap sampling method
#' @param stratify_by Stratification variable
#' @param parallel Use parallel processing
#' @param verbose Print progress
#'
#' @return List with bootstrap distributions
#' @keywords internal
bootstrap_null_model_indicators <- function(physeq, components, n_bootstrap, 
                                           bootstrap_method, stratify_by, 
                                           parallel, verbose) {
  
  # Get OTU matrix
  otu_mat <- as.matrix(phyloseq::otu_table(physeq))
  if (!phyloseq::taxa_are_rows(physeq)) otu_mat <- t(otu_mat)
  
  n_taxa <- nrow(otu_mat)
  n_samples <- ncol(otu_mat)
  component_names <- c("richness", "evenness", "phylogenetic", "spatial")
  
  # Initialize bootstrap storage
  bootstrap_distributions <- array(
    NA, 
    dim = c(n_bootstrap, n_taxa, length(component_names)),
    dimnames = list(NULL, rownames(otu_mat), component_names)
  )
  
  # Progress tracking
  if (verbose) {
    pb <- txtProgressBar(min = 0, max = n_bootstrap, style = 3)
  }
  
  # Bootstrap loop
  for (i in 1:n_bootstrap) {
    
    # Create bootstrap sample
    if (bootstrap_method == "nonparametric") {
      # Resample samples with replacement
      boot_indices <- sample(1:n_samples, n_samples, replace = TRUE)
      boot_otu <- otu_mat[, boot_indices, drop = FALSE]
    } else if (bootstrap_method == "parametric") {
      # Parametric bootstrap based on negative binomial
      boot_otu <- generate_parametric_bootstrap_microbiome(otu_mat)
    } else if (bootstrap_method == "block") {
      # Block bootstrap for temporal/spatial structure
      boot_otu <- generate_block_bootstrap_microbiome(otu_mat, stratify_by)
    }
    
    # Create bootstrap phyloseq object
    boot_physeq <- create_bootstrap_phyloseq(boot_otu, physeq)
    
    # Calculate null model indicators
    tryCatch({
      boot_null_results <- validate_taxa_indicators(
        boot_physeq,
        indicators = NULL,  # Calculate fresh indicators
        null_models = c("row_shuffle", "curveball"),
        n_permutations = 99,  # Reduced for bootstrap speed
        verbose = FALSE
      )
      
      # Extract effect sizes for each component
      for (comp in component_names) {
        if (comp %in% names(boot_null_results$effect_sizes)) {
          effect_sizes <- boot_null_results$effect_sizes[[comp]]
          bootstrap_distributions[i, names(effect_sizes), comp] <- effect_sizes
        }
      }
      
    }, error = function(e) {
      if (verbose) message("Bootstrap replicate ", i, " failed: ", e$message)
    })
    
    if (verbose) setTxtProgressBar(pb, i)
  }
  
  if (verbose) close(pb)
  
  return(list(distributions = bootstrap_distributions))
}

#' Bootstrap Mutual Information Indicators
#'
#' Bootstrap confidence intervals for mutual information results
#'
#' @param physeq Phyloseq object
#' @param components Universal information components
#' @param n_bootstrap Number of bootstrap replicates
#' @param bootstrap_method Bootstrap sampling method
#' @param stratify_by Stratification variable
#' @param parallel Use parallel processing
#' @param verbose Print progress
#'
#' @return List with bootstrap distributions
#' @keywords internal
bootstrap_mutual_information_indicators <- function(physeq, components, n_bootstrap,
                                                   bootstrap_method, stratify_by,
                                                   parallel, verbose) {
  
  # Get OTU matrix
  otu_mat <- as.matrix(phyloseq::otu_table(physeq))
  if (!phyloseq::taxa_are_rows(physeq)) otu_mat <- t(otu_mat)
  
  n_taxa <- nrow(otu_mat)
  n_samples <- ncol(otu_mat)
  component_names <- c("richness", "evenness", "phylogenetic", "spatial")
  
  # Initialize bootstrap storage
  bootstrap_distributions <- array(
    NA,
    dim = c(n_bootstrap, n_taxa, length(component_names)),
    dimnames = list(NULL, rownames(otu_mat), component_names)
  )
  
  # Progress tracking
  if (verbose) {
    pb <- txtProgressBar(min = 0, max = n_bootstrap, style = 3)
  }
  
  # Bootstrap loop
  for (i in 1:n_bootstrap) {
    
    # Create bootstrap sample
    if (bootstrap_method == "nonparametric") {
      boot_indices <- sample(1:n_samples, n_samples, replace = TRUE)
      boot_otu <- otu_mat[, boot_indices, drop = FALSE]
    } else if (bootstrap_method == "parametric") {
      boot_otu <- generate_parametric_bootstrap_microbiome(otu_mat)
    } else if (bootstrap_method == "block") {
      boot_otu <- generate_block_bootstrap_microbiome(otu_mat, stratify_by)
    }
    
    # Create bootstrap phyloseq object
    boot_physeq <- create_bootstrap_phyloseq(boot_otu, physeq)
    
    # Calculate mutual information indicators
    tryCatch({
      boot_info <- extract_universal_information(boot_physeq)
      boot_mi_results <- calculate_taxa_mutual_information(
        boot_physeq,
        components = boot_info,
        discretization = "equal_width",
        method = "emp",
        verbose = FALSE
      )
      
      # Extract normalized mutual information for each component
      for (comp in component_names) {
        if (comp %in% names(boot_mi_results$taxa_rankings)) {
          ranking <- boot_mi_results$taxa_rankings[[comp]]
          nmi_values <- setNames(ranking$normalized_mi, ranking$taxon)
          
          # Match to full taxa list
          bootstrap_distributions[i, names(nmi_values), comp] <- nmi_values
        }
      }
      
    }, error = function(e) {
      if (verbose) message("Bootstrap replicate ", i, " failed: ", e$message)
    })
    
    if (verbose) setTxtProgressBar(pb, i)
  }
  
  if (verbose) close(pb)
  
  return(list(distributions = bootstrap_distributions))
}

#' Bootstrap Shapley Value Indicators
#'
#' Bootstrap confidence intervals for Shapley value results
#'
#' @param physeq Phyloseq object
#' @param components Universal information components
#' @param n_bootstrap Number of bootstrap replicates
#' @param bootstrap_method Bootstrap sampling method
#' @param stratify_by Stratification variable
#' @param parallel Use parallel processing
#' @param verbose Print progress
#'
#' @return List with bootstrap distributions
#' @keywords internal
bootstrap_shapley_indicators <- function(physeq, components, n_bootstrap,
                                        bootstrap_method, stratify_by,
                                        parallel, verbose) {
  
  # Get OTU matrix
  otu_mat <- as.matrix(phyloseq::otu_table(physeq))
  if (!phyloseq::taxa_are_rows(physeq)) otu_mat <- t(otu_mat)
  
  # Select top taxa for computational efficiency
  taxa_abundance <- rowMeans(otu_mat)
  top_taxa_indices <- order(taxa_abundance, decreasing = TRUE)[1:min(15, nrow(otu_mat))]
  selected_taxa <- rownames(otu_mat)[top_taxa_indices]
  
  n_selected <- length(selected_taxa)
  n_samples <- ncol(otu_mat)
  component_names <- c("richness", "evenness", "phylogenetic", "spatial")
  
  # Initialize bootstrap storage
  bootstrap_distributions <- array(
    NA,
    dim = c(n_bootstrap, n_selected, length(component_names)),
    dimnames = list(NULL, selected_taxa, component_names)
  )
  
  # Progress tracking
  if (verbose) {
    pb <- txtProgressBar(min = 0, max = n_bootstrap, style = 3)
  }
  
  # Bootstrap loop
  for (i in 1:n_bootstrap) {
    
    # Create bootstrap sample
    if (bootstrap_method == "nonparametric") {
      boot_indices <- sample(1:n_samples, n_samples, replace = TRUE)
      boot_otu <- otu_mat[, boot_indices, drop = FALSE]
    } else if (bootstrap_method == "parametric") {
      boot_otu <- generate_parametric_bootstrap_microbiome(otu_mat)
    } else if (bootstrap_method == "block") {
      boot_otu <- generate_block_bootstrap_microbiome(otu_mat, stratify_by)
    }
    
    # Create bootstrap phyloseq object
    boot_physeq <- create_bootstrap_phyloseq(boot_otu, physeq)
    
    # Calculate Shapley value indicators
    tryCatch({
      boot_info <- extract_universal_information(boot_physeq)
      boot_shapley_results <- calculate_taxa_shapley_values(
        boot_physeq,
        components = boot_info,
        top_n = n_selected,
        method = "sampling",  # Use fast approximation for bootstrap
        n_samples = 100,
        verbose = FALSE
      )
      
      # Extract Shapley values for each component
      shapley_matrix <- boot_shapley_results$shapley_matrix
      for (comp in component_names) {
        if (comp %in% colnames(shapley_matrix)) {
          common_taxa <- intersect(rownames(shapley_matrix), selected_taxa)
          bootstrap_distributions[i, common_taxa, comp] <- shapley_matrix[common_taxa, comp]
        }
      }
      
    }, error = function(e) {
      if (verbose) message("Bootstrap replicate ", i, " failed: ", e$message)
    })
    
    if (verbose) setTxtProgressBar(pb, i)
  }
  
  if (verbose) close(pb)
  
  return(list(distributions = bootstrap_distributions))
}

#' Generate Parametric Bootstrap Microbiome
#'
#' Creates parametric bootstrap sample using negative binomial model
#'
#' @param otu_mat Original OTU matrix
#'
#' @return Bootstrap OTU matrix
#' @keywords internal
generate_parametric_bootstrap_microbiome <- function(otu_mat) {
  
  n_taxa <- nrow(otu_mat)
  n_samples <- ncol(otu_mat)
  
  # Initialize bootstrap matrix
  boot_otu <- matrix(0, nrow = n_taxa, ncol = n_samples)
  rownames(boot_otu) <- rownames(otu_mat)
  colnames(boot_otu) <- colnames(otu_mat)
  
  # For each taxon, fit negative binomial and generate
  for (i in 1:n_taxa) {
    taxon_counts <- otu_mat[i, ]
    
    if (all(taxon_counts == 0)) {
      boot_otu[i, ] <- 0
      next
    }
    
    # Estimate negative binomial parameters
    non_zero_counts <- taxon_counts[taxon_counts > 0]
    if (length(non_zero_counts) > 1) {
      mean_count <- mean(non_zero_counts)
      var_count <- var(non_zero_counts)
      
      # Method of moments estimation
      if (var_count > mean_count) {
        size_param <- mean_count^2 / (var_count - mean_count)
        prob_param <- mean_count / var_count
        
        # Generate bootstrap counts
        boot_otu[i, ] <- rnbinom(n_samples, size = size_param, prob = prob_param)
      } else {
        # Fall back to Poisson if variance <= mean
        boot_otu[i, ] <- rpois(n_samples, lambda = mean_count)
      }
    } else {
      # Single non-zero value - use Poisson
      boot_otu[i, ] <- rpois(n_samples, lambda = mean(taxon_counts))
    }
  }
  
  return(boot_otu)
}

#' Generate Block Bootstrap Microbiome
#'
#' Creates block bootstrap for samples with temporal/spatial structure
#'
#' @param otu_mat Original OTU matrix
#' @param stratify_by Stratification variable
#'
#' @return Bootstrap OTU matrix
#' @keywords internal
generate_block_bootstrap_microbiome <- function(otu_mat, stratify_by) {
  
  n_samples <- ncol(otu_mat)
  
  if (is.null(stratify_by)) {
    # Simple block bootstrap with block size = sqrt(n)
    block_size <- max(1, floor(sqrt(n_samples)))
    n_blocks <- ceiling(n_samples / block_size)
    
    # Generate bootstrap indices
    boot_indices <- c()
    for (i in 1:n_blocks) {
      start_idx <- sample(1:(n_samples - block_size + 1), 1)
      block_indices <- start_idx:(start_idx + block_size - 1)
      block_indices <- block_indices[block_indices <= n_samples]
      boot_indices <- c(boot_indices, block_indices)
    }
    
    # Trim to original sample size
    boot_indices <- boot_indices[1:n_samples]
    
  } else {
    # Stratified sampling
    boot_indices <- sample(1:n_samples, n_samples, replace = TRUE)
  }
  
  return(otu_mat[, boot_indices, drop = FALSE])
}

#' Create Bootstrap Phyloseq Object
#'
#' Creates phyloseq object from bootstrap OTU matrix
#'
#' @param boot_otu Bootstrap OTU matrix
#' @param original_physeq Original phyloseq object
#'
#' @return Bootstrap phyloseq object
#' @keywords internal
create_bootstrap_phyloseq <- function(boot_otu, original_physeq) {
  
  # Create OTU table
  boot_otu_table <- phyloseq::otu_table(boot_otu, taxa_are_rows = TRUE)
  
  # Copy other components if available
  components <- list(boot_otu_table)
  
  if (!is.null(phyloseq::tax_table(original_physeq, errorIfNULL = FALSE))) {
    components[[length(components) + 1]] <- phyloseq::tax_table(original_physeq)
  }
  
  if (!is.null(phyloseq::sample_data(original_physeq, errorIfNULL = FALSE))) {
    # Subset sample data to match bootstrap samples
    original_sample_data <- phyloseq::sample_data(original_physeq)
    boot_sample_names <- colnames(boot_otu)
    
    if (all(boot_sample_names %in% rownames(original_sample_data))) {
      boot_sample_data <- original_sample_data[boot_sample_names, ]
      components[[length(components) + 1]] <- boot_sample_data
    }
  }
  
  if (!is.null(phyloseq::phy_tree(original_physeq, errorIfNULL = FALSE))) {
    components[[length(components) + 1]] <- phyloseq::phy_tree(original_physeq)
  }
  
  # Create phyloseq object
  boot_physeq <- do.call(phyloseq::phyloseq, components)
  
  return(boot_physeq)
}

#' Calculate Confidence Intervals
#'
#' Calculates confidence intervals from bootstrap distributions
#'
#' @param distributions Bootstrap distribution array
#' @param confidence_level Confidence level (default: 0.95)
#'
#' @return List of confidence interval matrices
#' @keywords internal
calculate_confidence_intervals <- function(distributions, confidence_level = 0.95) {
  
  alpha <- 1 - confidence_level
  lower_quantile <- alpha / 2
  upper_quantile <- 1 - alpha / 2
  
  # Calculate quantiles for each taxon and component
  ci_lower <- apply(distributions, c(2, 3), quantile, probs = lower_quantile, na.rm = TRUE)
  ci_upper <- apply(distributions, c(2, 3), quantile, probs = upper_quantile, na.rm = TRUE)
  ci_median <- apply(distributions, c(2, 3), median, na.rm = TRUE)
  
  return(list(
    lower = ci_lower,
    upper = ci_upper,
    median = ci_median,
    confidence_level = confidence_level
  ))
}

#' Calculate Uncertainty Metrics
#'
#' Calculates comprehensive uncertainty metrics from bootstrap distributions
#'
#' @param distributions Bootstrap distribution array
#' @param method Indicator method name
#'
#' @return List of uncertainty metrics
#' @keywords internal
calculate_uncertainty_metrics <- function(distributions, method) {
  
  # Calculate basic statistics
  means <- apply(distributions, c(2, 3), mean, na.rm = TRUE)
  variances <- apply(distributions, c(2, 3), var, na.rm = TRUE)
  std_devs <- sqrt(variances)
  
  # Coefficient of variation
  cv <- std_devs / abs(means)
  cv[is.infinite(cv) | is.nan(cv)] <- NA
  
  # Bootstrap reliability (proportion of non-NA values)
  reliability <- apply(distributions, c(2, 3), function(x) mean(!is.na(x)))
  
  # Stability index (1 - CV, bounded [0,1])
  stability <- pmax(0, 1 - pmin(cv, 1))
  
  # Rank stability (correlation between bootstrap ranks)
  rank_stability <- calculate_rank_stability(distributions)
  
  return(list(
    means = means,
    variances = variances,
    std_devs = std_devs,
    cv = cv,
    reliability = reliability,
    stability = stability,
    rank_stability = rank_stability,
    method = method
  ))
}

#' Calculate Rank Stability
#'
#' Calculates rank stability across bootstrap replicates
#'
#' @param distributions Bootstrap distribution array
#'
#' @return Matrix of rank stability indices
#' @keywords internal
calculate_rank_stability <- function(distributions) {
  
  n_bootstrap <- dim(distributions)[1]
  n_taxa <- dim(distributions)[2]
  n_components <- dim(distributions)[3]
  component_names <- dimnames(distributions)[[3]]
  
  # Initialize rank stability matrix
  rank_stability <- matrix(NA, nrow = n_taxa, ncol = n_components)
  rownames(rank_stability) <- dimnames(distributions)[[2]]
  colnames(rank_stability) <- component_names
  
  for (comp in 1:n_components) {
    
    # Extract component data
    comp_data <- distributions[, , comp]
    
    # Calculate ranks for each bootstrap replicate
    rank_matrix <- apply(comp_data, 1, function(x) {
      if (all(is.na(x))) return(rep(NA, length(x)))
      rank(-x, ties.method = "average", na.last = "keep")
    })
    
    # rank_matrix is now n_taxa x n_bootstrap
    rank_matrix <- t(rank_matrix)
    
    # Calculate rank stability for each taxon
    for (taxon in 1:n_taxa) {
      taxon_ranks <- rank_matrix[taxon, ]
      valid_ranks <- taxon_ranks[!is.na(taxon_ranks)]
      
      if (length(valid_ranks) > 1) {
        # Stability = 1 - (CV of ranks)
        rank_cv <- sd(valid_ranks) / mean(valid_ranks)
        rank_stability[taxon, comp] <- max(0, 1 - rank_cv)
      }
    }
  }
  
  return(rank_stability)
}

#' Create Consensus Rankings with Uncertainty
#'
#' Creates consensus rankings across methods with uncertainty quantification
#'
#' @param bootstrap_results Bootstrap results for all methods
#' @param confidence_intervals Confidence intervals for all methods
#' @param indicator_methods Methods included
#'
#' @return List of consensus rankings with uncertainty
#' @keywords internal
create_consensus_rankings_with_uncertainty <- function(bootstrap_results,
                                                      confidence_intervals,
                                                      indicator_methods) {
  
  component_names <- c("richness", "evenness", "phylogenetic", "spatial")
  consensus_rankings <- list()
  
  for (comp in component_names) {
    
    # Get all taxa across methods
    all_taxa <- unique(unlist(lapply(bootstrap_results, function(x) {
      dimnames(x)[[2]]
    })))
    
    # Initialize consensus data frame
    consensus_df <- data.frame(
      taxon = all_taxa,
      consensus_score = NA,
      consensus_rank = NA,
      uncertainty = NA,
      n_methods = 0,
      stringsAsFactors = FALSE
    )
    
    # Add method-specific scores and uncertainties
    for (method in indicator_methods) {
      if (method %in% names(bootstrap_results)) {
        distributions <- bootstrap_results[[method]]
        
        if (comp %in% dimnames(distributions)[[3]]) {
          comp_data <- distributions[, , comp]
          
          # Calculate median scores and uncertainties
          medians <- apply(comp_data, 2, median, na.rm = TRUE)
          uncertainties <- apply(comp_data, 2, function(x) {
            sd(x, na.rm = TRUE) / abs(median(x, na.rm = TRUE))
          })
          
          # Add to consensus data frame
          method_col_score <- paste0(method, "_score")
          method_col_uncertainty <- paste0(method, "_uncertainty")
          
          consensus_df[[method_col_score]] <- NA
          consensus_df[[method_col_uncertainty]] <- NA
          
          # Match taxa
          common_taxa <- intersect(consensus_df$taxon, names(medians))
          consensus_df[consensus_df$taxon %in% common_taxa, method_col_score] <- medians[common_taxa]
          consensus_df[consensus_df$taxon %in% common_taxa, method_col_uncertainty] <- uncertainties[common_taxa]
          
          # Count methods per taxon
          consensus_df[consensus_df$taxon %in% common_taxa, "n_methods"] <- 
            consensus_df[consensus_df$taxon %in% common_taxa, "n_methods"] + 1
        }
      }
    }
    
    # Calculate consensus scores (weighted average, weights = 1/uncertainty)
    score_cols <- grep("_score$", names(consensus_df), value = TRUE)
    uncertainty_cols <- grep("_uncertainty$", names(consensus_df), value = TRUE)
    
    for (i in 1:nrow(consensus_df)) {
      scores <- as.numeric(consensus_df[i, score_cols])
      uncertainties <- as.numeric(consensus_df[i, uncertainty_cols])
      
      valid_idx <- !is.na(scores) & !is.na(uncertainties) & uncertainties > 0
      
      if (sum(valid_idx) > 0) {
        weights <- 1 / uncertainties[valid_idx]
        weights <- weights / sum(weights)  # Normalize
        
        consensus_df$consensus_score[i] <- sum(scores[valid_idx] * weights)
        consensus_df$uncertainty[i] <- sqrt(sum((uncertainties[valid_idx] * weights)^2))
      }
    }
    
    # Rank by consensus score
    valid_consensus <- !is.na(consensus_df$consensus_score)
    consensus_df$consensus_rank[valid_consensus] <- rank(
      -consensus_df$consensus_score[valid_consensus], 
      ties.method = "average"
    )
    
    # Sort by consensus rank
    consensus_df <- consensus_df[order(consensus_df$consensus_rank), ]
    
    consensus_rankings[[comp]] <- consensus_df
  }
  
  return(consensus_rankings)
}

#' Assess Bootstrap Reliability
#'
#' Assesses overall reliability of bootstrap results
#'
#' @param bootstrap_results Bootstrap results for all methods
#' @param uncertainty_metrics Uncertainty metrics for all methods
#'
#' @return List of reliability assessments
#' @keywords internal
assess_bootstrap_reliability <- function(bootstrap_results, uncertainty_metrics) {
  
  reliability_assessment <- list()
  
  # Overall convergence assessment
  convergence_stats <- list()
  
  for (method in names(bootstrap_results)) {
    distributions <- bootstrap_results[[method]]
    
    # Check for convergence (stability of running means)
    convergence_stats[[method]] <- assess_bootstrap_convergence(distributions)
  }
  
  reliability_assessment$convergence = convergence_stats
  
  # Reliability by component
  component_reliability <- list()
  components <- c("richness", "evenness", "phylogenetic", "spatial")
  
  for (comp in components) {
    comp_reliability <- list()
    
    for (method in names(uncertainty_metrics)) {
      if (comp %in% colnames(uncertainty_metrics[[method]]$reliability)) {
        comp_reliability[[method]] <- mean(
          uncertainty_metrics[[method]]$reliability[, comp], 
          na.rm = TRUE
        )
      }
    }
    
    component_reliability[[comp]] <- comp_reliability
  }
  
  reliability_assessment$component_reliability <- component_reliability
  
  # Method stability comparison
  method_stability <- list()
  for (method in names(uncertainty_metrics)) {
    stability_matrix <- uncertainty_metrics[[method]]$stability
    method_stability[[method]] <- mean(stability_matrix, na.rm = TRUE)
  }
  
  reliability_assessment$method_stability <- method_stability
  
  return(reliability_assessment)
}

#' Assess Bootstrap Convergence
#'
#' Checks convergence of bootstrap estimates
#'
#' @param distributions Bootstrap distribution array
#'
#' @return Convergence diagnostics
#' @keywords internal
assess_bootstrap_convergence <- function(distributions) {
  
  n_bootstrap <- dim(distributions)[1]
  
  # Calculate running means for first taxon, first component as diagnostic
  if (n_bootstrap < 10) {
    return(list(converged = FALSE, message = "Too few bootstrap replicates"))
  }
  
  first_series <- distributions[, 1, 1]
  valid_series <- first_series[!is.na(first_series)]
  
  if (length(valid_series) < 10) {
    return(list(converged = FALSE, message = "Too many missing values"))
  }
  
  # Calculate running means
  running_means <- cumsum(valid_series) / seq_along(valid_series)
  
  # Check stability of final 20% of running means
  final_portion <- floor(0.8 * length(running_means)):length(running_means)
  final_means <- running_means[final_portion]
  
  # Convergence criterion: CV of final means < 0.05
  cv_final <- sd(final_means) / abs(mean(final_means))
  converged <- cv_final < 0.05
  
  return(list(
    converged = converged,
    cv_final = cv_final,
    n_valid = length(valid_series),
    running_means = running_means
  ))
}

#' Compare Indicator Methods
#'
#' Compares different indicator methods using bootstrap results
#'
#' @param bootstrap_results Bootstrap results for all methods
#' @param uncertainty_metrics Uncertainty metrics for all methods
#'
#' @return Method comparison summary
#' @keywords internal
compare_indicator_methods <- function(bootstrap_results, uncertainty_metrics) {
  
  method_comparison <- list()
  
  # Method characteristics
  method_characteristics <- data.frame(
    method = names(bootstrap_results),
    mean_uncertainty = NA,
    mean_stability = NA,
    computational_complexity = NA,
    stringsAsFactors = FALSE
  )
  
  for (i in 1:nrow(method_characteristics)) {
    method <- method_characteristics$method[i]
    
    if (method %in% names(uncertainty_metrics)) {
      metrics <- uncertainty_metrics[[method]]
      method_characteristics$mean_uncertainty[i] <- mean(metrics$cv, na.rm = TRUE)
      method_characteristics$mean_stability[i] <- mean(metrics$stability, na.rm = TRUE)
    }
    
    # Assign complexity categories
    if (method == "null_models") {
      method_characteristics$computational_complexity[i] <- "Medium"
    } else if (method == "mutual_info") {
      method_characteristics$computational_complexity[i] <- "Low"
    } else if (method == "shapley") {
      method_characteristics$computational_complexity[i] <- "High"
    }
  }
  
  method_comparison$characteristics <- method_characteristics
  
  # Pairwise correlations between methods
  pairwise_correlations <- calculate_method_correlations(bootstrap_results)
  method_comparison$correlations <- pairwise_correlations
  
  # Method agreement assessment
  agreement_assessment <- assess_method_agreement(bootstrap_results)
  method_comparison$agreement <- agreement_assessment
  
  return(method_comparison)
}

#' Calculate Method Correlations
#'
#' Calculates correlations between different indicator methods
#'
#' @param bootstrap_results Bootstrap results for all methods
#'
#' @return Correlation matrices by component
#' @keywords internal
calculate_method_correlations <- function(bootstrap_results) {
  
  components <- c("richness", "evenness", "phylogenetic", "spatial")
  methods <- names(bootstrap_results)
  
  correlations <- list()
  
  for (comp in components) {
    
    # Extract median scores for each method
    method_scores <- list()
    
    for (method in methods) {
      distributions <- bootstrap_results[[method]]
      
      if (comp %in% dimnames(distributions)[[3]]) {
        comp_data <- distributions[, , comp]
        medians <- apply(comp_data, 2, median, na.rm = TRUE)
        method_scores[[method]] <- medians
      }
    }
    
    if (length(method_scores) > 1) {
      
      # Get common taxa
      all_taxa <- Reduce(intersect, lapply(method_scores, names))
      
      if (length(all_taxa) > 3) {
        
        # Create score matrix
        score_matrix <- matrix(NA, nrow = length(all_taxa), ncol = length(method_scores))
        rownames(score_matrix) <- all_taxa
        colnames(score_matrix) <- names(method_scores)
        
        for (method in names(method_scores)) {
          score_matrix[, method] <- method_scores[[method]][all_taxa]
        }
        
        # Calculate correlation matrix
        cor_matrix <- cor(score_matrix, use = "complete.obs")
        correlations[[comp]] <- cor_matrix
      }
    }
  }
  
  return(correlations)
}

#' Assess Method Agreement
#'
#' Assesses agreement between indicator methods
#'
#' @param bootstrap_results Bootstrap results for all methods
#'
#' @return Agreement assessment
#' @keywords internal
assess_method_agreement <- function(bootstrap_results) {
  
  components <- c("richness", "evenness", "phylogenetic", "spatial")
  methods <- names(bootstrap_results)
  
  agreement <- list()
  
  for (comp in components) {
    
    # Get top 10 taxa from each method
    top_taxa_lists <- list()
    
    for (method in methods) {
      distributions <- bootstrap_results[[method]]
      
      if (comp %in% dimnames(distributions)[[3]]) {
        comp_data <- distributions[, , comp]
        medians <- apply(comp_data, 2, median, na.rm = TRUE)
        
        # Get top 10 taxa
        top_10_idx <- order(medians, decreasing = TRUE)[1:min(10, length(medians))]
        top_taxa_lists[[method]] <- names(medians)[top_10_idx]
      }
    }
    
    if (length(top_taxa_lists) > 1) {
      
      # Calculate Jaccard indices for all pairs
      n_methods <- length(top_taxa_lists)
      jaccard_matrix <- matrix(1, nrow = n_methods, ncol = n_methods)
      rownames(jaccard_matrix) <- names(top_taxa_lists)
      colnames(jaccard_matrix) <- names(top_taxa_lists)
      
      for (i in 1:(n_methods - 1)) {
        for (j in (i + 1):n_methods) {
          list1 <- top_taxa_lists[[i]]
          list2 <- top_taxa_lists[[j]]
          
          intersection_size <- length(intersect(list1, list2))
          union_size <- length(union(list1, list2))
          
          jaccard_index <- intersection_size / union_size
          jaccard_matrix[i, j] <- jaccard_index
          jaccard_matrix[j, i] <- jaccard_index
        }
      }
      
      agreement[[comp]] <- list(
        jaccard_matrix = jaccard_matrix,
        mean_agreement = mean(jaccard_matrix[upper.tri(jaccard_matrix)]),
        top_taxa_lists = top_taxa_lists
      )
    }
  }
  
  return(agreement)
}

#' Print Method for Bootstrap Indicators
#'
#' @param x A bootstrap_indicators object
#' @param ... Additional arguments (ignored)
#'
#' @export
print.bootstrap_indicators <- function(x, ...) {
  cat("Bootstrap Uncertainty Analysis Results\n")
  cat("=====================================\n\n")
  
  cat("BOOTSTRAP CONFIGURATION:\n")
  cat("  Replicates:", x$n_bootstrap, "\n")
  cat("  Confidence level:", x$confidence_level * 100, "%\n")
  cat("  Bootstrap method:", x$bootstrap_method, "\n")
  cat("  Indicator methods:", paste(x$indicator_methods, collapse = ", "), "\n\n")
  
  # Print reliability summary
  cat("RELIABILITY SUMMARY:\n")
  if (!is.null(x$reliability_assessment$method_stability)) {
    for (method in names(x$reliability_assessment$method_stability)) {
      stability <- x$reliability_assessment$method_stability[[method]]
      cat(sprintf("  %s stability: %.3f\n", method, stability))
    }
  }
  cat("\n")
  
  # Print method comparison
  if (!is.null(x$method_comparison$characteristics)) {
    cat("METHOD CHARACTERISTICS:\n")
    chars <- x$method_comparison$characteristics
    print(chars)
    cat("\n")
  }
  
  # Print consensus rankings for richness
  if (!is.null(x$consensus_rankings$richness)) {
    cat("TOP 5 CONSENSUS RICHNESS INDICATORS:\n")
    richness_ranking <- head(x$consensus_rankings$richness, 5)
    for (i in 1:nrow(richness_ranking)) {
      cat(sprintf("  %d. %s (score: %.3f, uncertainty: %.3f)\n",
                 i,
                 richness_ranking$taxon[i],
                 richness_ranking$consensus_score[i],
                 richness_ranking$uncertainty[i]))
    }
  }
  
  invisible(x)
}

#' Plot Bootstrap Indicators
#'
#' Creates visualizations of bootstrap uncertainty analysis
#'
#' @param x A bootstrap_indicators object
#' @param type Plot type: "confidence_intervals", "uncertainty", "consensus", "method_comparison"
#' @param component Component to visualize (default: "richness")
#' @param top_n Number of top taxa to show
#' @param interactive Create interactive plot using plotly
#' @param ... Additional arguments passed to plotting functions
#'
#' @return A ggplot2 or plotly object
#' @export
plot.bootstrap_indicators <- function(x,
                                     type = c("confidence_intervals", "uncertainty", "consensus", "method_comparison"),
                                     component = "richness",
                                     top_n = 15,
                                     interactive = FALSE,
                                     ...) {
  
  type <- match.arg(type)
  
  if (type == "confidence_intervals") {
    p <- plot_bootstrap_confidence_intervals(x, component, top_n, interactive)
  } else if (type == "uncertainty") {
    p <- plot_bootstrap_uncertainty(x, component, top_n, interactive)
  } else if (type == "consensus") {
    p <- plot_consensus_rankings(x, top_n, interactive)
  } else if (type == "method_comparison") {
    p <- plot_method_comparison(x, component, interactive)
  }
  
  return(p)
}

#' Plot Bootstrap Confidence Intervals
#'
#' @param x Bootstrap indicators object
#' @param component Component to plot
#' @param top_n Number of taxa to show
#' @param interactive Use plotly
#'
#' @return Plot object
#' @keywords internal
plot_bootstrap_confidence_intervals <- function(x, component, top_n, interactive) {
  
  # Get the best method (highest stability)
  best_method <- names(which.max(x$reliability_assessment$method_stability))
  
  if (best_method %in% names(x$confidence_intervals)) {
    ci_data <- x$confidence_intervals[[best_method]]
    
    if (component %in% colnames(ci_data$median)) {
      
      # Get top taxa
      medians <- ci_data$median[, component]
      top_taxa_idx <- order(medians, decreasing = TRUE)[1:min(top_n, length(medians))]
      top_taxa <- names(medians)[top_taxa_idx]
      
      # Create plot data
      plot_data <- data.frame(
        taxon = top_taxa,
        median = ci_data$median[top_taxa, component],
        lower = ci_data$lower[top_taxa, component],
        upper = ci_data$upper[top_taxa, component],
        stringsAsFactors = FALSE
      )
      
      # Order by median
      plot_data <- plot_data[order(plot_data$median, decreasing = TRUE), ]
      plot_data$taxon <- factor(plot_data$taxon, levels = plot_data$taxon)
      
      # Create plot
      p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = taxon, y = median)) +
        ggplot2::geom_point(size = 2) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper), width = 0.2) +
        ggplot2::labs(
          title = paste("Bootstrap Confidence Intervals -", tools::toTitleCase(component)),
          subtitle = paste("Method:", best_method, "| Confidence level:", x$confidence_level * 100, "%"),
          x = "Taxon",
          y = "Indicator Value"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
      
      if (interactive) {
        p <- plotly::ggplotly(p)
      }
      
      return(p)
    }
  }
  
  # Fallback message
  message("No confidence interval data available for ", component, " component")
  return(NULL)
}

#' Plot Bootstrap Uncertainty
#'
#' @param x Bootstrap indicators object
#' @param component Component to plot
#' @param top_n Number of taxa to show
#' @param interactive Use plotly
#'
#' @return Plot object
#' @keywords internal
plot_bootstrap_uncertainty <- function(x, component, top_n, interactive) {
  
  # Combine uncertainty data from all methods
  uncertainty_data <- data.frame(
    taxon = character(0),
    method = character(0),
    cv = numeric(0),
    stability = numeric(0),
    stringsAsFactors = FALSE
  )
  
  for (method in names(x$uncertainty_metrics)) {
    metrics <- x$uncertainty_metrics[[method]]
    
    if (component %in% colnames(metrics$cv)) {
      
      # Get top taxa for this method
      cv_values <- metrics$cv[, component]
      stability_values <- metrics$stability[, component]
      
      valid_idx <- !is.na(cv_values) & !is.na(stability_values)
      
      if (sum(valid_idx) > 0) {
        method_data <- data.frame(
          taxon = names(cv_values)[valid_idx],
          method = method,
          cv = cv_values[valid_idx],
          stability = stability_values[valid_idx],
          stringsAsFactors = FALSE
        )
        
        uncertainty_data <- rbind(uncertainty_data, method_data)
      }
    }
  }
  
  if (nrow(uncertainty_data) > 0) {
    
    # Get top taxa overall
    top_taxa <- head(unique(uncertainty_data$taxon[order(uncertainty_data$stability, decreasing = TRUE)]), top_n)
    uncertainty_data <- uncertainty_data[uncertainty_data$taxon %in% top_taxa, ]
    
    # Create plot
    p <- ggplot2::ggplot(uncertainty_data, 
                        ggplot2::aes(x = cv, y = stability, color = method)) +
      ggplot2::geom_point(alpha = 0.7, size = 2) +
      ggplot2::labs(
        title = paste("Bootstrap Uncertainty Analysis -", tools::toTitleCase(component)),
        x = "Coefficient of Variation",
        y = "Stability Index",
        color = "Method"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::scale_x_log10()
    
    if (interactive) {
      p <- plotly::ggplotly(p)
    }
    
    return(p)
  }
  
  # Fallback message
  message("No uncertainty data available for ", component, " component")
  return(NULL)
}

#' Plot Consensus Rankings
#'
#' @param x Bootstrap indicators object
#' @param top_n Number of taxa to show
#' @param interactive Use plotly
#'
#' @return Plot object
#' @keywords internal
plot_consensus_rankings <- function(x, top_n, interactive) {
  
  # Combine consensus rankings for all components
  consensus_data <- data.frame(
    taxon = character(0),
    component = character(0),
    consensus_score = numeric(0),
    uncertainty = numeric(0),
    rank = numeric(0),
    stringsAsFactors = FALSE
  )
  
  components <- names(x$consensus_rankings)
  for (comp in components) {
    ranking <- x$consensus_rankings[[comp]]
    
    if (!is.null(ranking) && nrow(ranking) > 0) {
      top_comp <- head(ranking, top_n)
      
      comp_data <- data.frame(
        taxon = top_comp$taxon,
        component = comp,
        consensus_score = top_comp$consensus_score,
        uncertainty = top_comp$uncertainty,
        rank = top_comp$consensus_rank,
        stringsAsFactors = FALSE
      )
      
      consensus_data <- rbind(consensus_data, comp_data)
    }
  }
  
  if (nrow(consensus_data) > 0) {
    
    # Create plot
    p <- ggplot2::ggplot(consensus_data, 
                        ggplot2::aes(x = stats::reorder(taxon, consensus_score),
                                    y = consensus_score,
                                    fill = component)) +
      ggplot2::geom_bar(stat = "identity", alpha = 0.8) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = consensus_score - uncertainty,
                                         ymax = consensus_score + uncertainty),
                            width = 0.2, alpha = 0.6) +
      ggplot2::facet_wrap(~ component, scales = "free") +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = "Consensus Taxa Indicator Rankings",
        subtitle = "Error bars show bootstrap uncertainty",
        x = "Taxon",
        y = "Consensus Score",
        fill = "Component"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "none")
    
    if (interactive) {
      p <- plotly::ggplotly(p)
    }
    
    return(p)
  }
  
  # Fallback message
  message("No consensus ranking data available")
  return(NULL)
}

#' Plot Method Comparison
#'
#' @param x Bootstrap indicators object
#' @param component Component to plot
#' @param interactive Use plotly
#'
#' @return Plot object
#' @keywords internal
plot_method_comparison <- function(x, component, interactive) {
  
  if (!is.null(x$method_comparison$correlations) && 
      component %in% names(x$method_comparison$correlations)) {
    
    cor_matrix <- x$method_comparison$correlations[[component]]
    
    if (!is.null(cor_matrix)) {
      
      # Convert correlation matrix to long format
      cor_data <- expand.grid(
        Method1 = rownames(cor_matrix),
        Method2 = colnames(cor_matrix),
        stringsAsFactors = FALSE
      )
      cor_data$Correlation <- as.vector(cor_matrix)
      
      # Create heatmap
      p <- ggplot2::ggplot(cor_data, 
                          ggplot2::aes(x = Method1, y = Method2, fill = Correlation)) +
        ggplot2::geom_tile() +
        ggplot2::geom_text(ggplot2::aes(label = round(Correlation, 2)), color = "white") +
        ggplot2::scale_fill_gradient2(low = "red", mid = "white", high = "blue", 
                                     midpoint = 0, limits = c(-1, 1)) +
        ggplot2::labs(
          title = paste("Method Correlation Matrix -", tools::toTitleCase(component)),
          x = "Method 1",
          y = "Method 2"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
      
      if (interactive) {
        p <- plotly::ggplotly(p)
      }
      
      return(p)
    }
  }
  
  # Fallback message
  message("No method comparison data available for ", component, " component")
  return(NULL)
}