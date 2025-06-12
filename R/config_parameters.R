#' diversityGPT Configuration Parameters
#'
#' @description
#' Centralized configuration for all package parameters, thresholds,
#' and constants. This ensures transparency and allows users to
#' understand and modify default behaviors.
#'
#' @name config_parameters
#' @keywords internal
NULL

#' Get Package Configuration Parameters
#'
#' @description
#' Returns the default configuration parameters used throughout
#' the diversityGPT package. Users can modify these for their
#' analyses if needed.
#'
#' @param category Optional category to filter parameters
#'
#' @return List of configuration parameters
#' @export
#' @examples
#' # Get all parameters
#' params <- get_config_parameters()
#' 
#' # Get specific category
#' ecological_params <- get_config_parameters("ecological")
get_config_parameters <- function(category = NULL) {
  
  params <- list(
    
    # Information Theory Parameters
    information_theory = list(
      pseudocount = 1e-10,  # Small value to avoid log(0) in entropy calculations
      entropy_avg_weight = 0.5,  # Weight for averaging entropies (Jost 2007)
      discretization_bins = 5,  # Default bins for continuous to discrete conversion
      min_observations = 10  # Minimum observations for reliable MI estimation
    ),
    
    # Ecological Threshold Parameters
    ecological = list(
      # Environmental filtering thresholds
      env_correlation_threshold = 0.5,  # Minimum correlation to infer env filtering
      
      # Competition thresholds
      high_richness_threshold = 0.7,  # R component > this suggests competition
      low_evenness_threshold = 0.4,   # E component < this suggests competition
      
      # Neutral process thresholds
      variance_threshold = 0.5,       # Total variance > this suggests neutrality
      predictability_threshold = 0.6,  # Predictability < this suggests neutrality
      
      # Dispersal limitation
      spatial_signal_threshold = 0.3,  # S component > this suggests dispersal limitation
      
      # Confidence thresholds
      min_confidence = 0.1,  # Minimum confidence to report mechanism
      high_confidence = 0.8  # High confidence threshold
    ),
    
    # Statistical Parameters
    statistical = list(
      bootstrap_iterations = 1000,  # Default bootstrap iterations
      significance_level = 0.05,    # Default alpha for hypothesis tests
      ci_level = 0.95,             # Confidence interval level
      min_sample_size = 3,         # Minimum samples per group
      multiple_testing_method = "fdr"  # p-value adjustment method
    ),
    
    # Computational Parameters
    computational = list(
      max_exact_shapley = 12,      # Max taxa for exact Shapley calculation
      shapley_samples = 1000,      # Monte Carlo samples for Shapley
      parallel_threshold = 1000,   # When to use parallel processing
      cache_size_mb = 100,         # Maximum cache size in MB
      progress_bar_min = 10        # Minimum iterations to show progress bar
    ),
    
    # Beta Diversity Parameters
    beta_diversity = list(
      spatial_max_distance = NULL,  # NULL = estimate from data
      distance_decay_rate = 1.0,    # Exponential decay rate
      phylo_weight_branch_length = TRUE,  # Weight by branch length
      min_shared_species = 2        # Minimum shared species for calculations
    ),
    
    # Visualization Parameters
    visualization = list(
      network_min_edge_weight = 0.25,  # Minimum edge weight to display
      heatmap_color_threshold = 0.5,   # Color scale midpoint
      default_node_size = 0.5,         # Default node size
      max_plot_taxa = 50,              # Maximum taxa to plot
      color_palette = "viridis"        # Default color palette
    )
  )
  
  # Return specific category if requested
  if (!is.null(category)) {
    if (category %in% names(params)) {
      return(params[[category]])
    } else {
      cli::cli_warn("Category '{category}' not found. Available categories: {names(params)}")
      return(NULL)
    }
  }
  
  return(params)
}

#' Set Package Configuration Parameters
#'
#' @description
#' Allows users to modify default parameters for their session.
#' Changes are not persistent across sessions.
#'
#' @param ... Named parameters to set in format category.parameter = value
#'
#' @return Invisible list of previous values
#' @export
#' @examples
#' # Change bootstrap iterations
#' set_config_parameters(statistical.bootstrap_iterations = 2000)
#' 
#' # Change multiple parameters
#' set_config_parameters(
#'   ecological.spatial_signal_threshold = 0.4,
#'   statistical.significance_level = 0.01
#' )
set_config_parameters <- function(...) {
  
  # This would require a more complex implementation with
  # package-level environment to store modified parameters
  cli::cli_alert_info("Parameter modification not yet implemented")
  cli::cli_alert_info("For now, pass parameters directly to functions")
  
  invisible(NULL)
}

#' Validate Configuration Parameters
#'
#' @description
#' Checks that configuration parameters are within valid ranges.
#'
#' @param params Parameter list to validate
#'
#' @return Logical indicating if parameters are valid
#' @keywords internal
validate_config_parameters <- function(params) {
  
  valid <- TRUE
  
  # Check information theory parameters
  if (params$information_theory$pseudocount <= 0) {
    cli::cli_warn("Pseudocount must be positive")
    valid <- FALSE
  }
  
  if (params$information_theory$entropy_avg_weight < 0 || 
      params$information_theory$entropy_avg_weight > 1) {
    cli::cli_warn("Entropy averaging weight must be between 0 and 1")
    valid <- FALSE
  }
  
  # Check statistical parameters
  if (params$statistical$significance_level <= 0 || 
      params$statistical$significance_level >= 1) {
    cli::cli_warn("Significance level must be between 0 and 1")
    valid <- FALSE
  }
  
  # Check thresholds
  thresholds <- unlist(params$ecological)
  if (any(thresholds < 0 | thresholds > 1, na.rm = TRUE)) {
    cli::cli_warn("Ecological thresholds must be between 0 and 1")
    valid <- FALSE
  }
  
  return(valid)
}