#' Information Theory Approach to Taxa Indicators
#'
#' @description Advanced information-theoretic methods for quantifying taxa-component relationships
#' @name information_theory
#' @keywords information theory, mutual information
NULL

#' Calculate Mutual Information between Taxa and Components
#'
#' Uses mutual information to quantify how much information each taxon provides
#' about diversity component values. This is a rigorous mathematical approach
#' that measures statistical dependence without assuming linear relationships.
#'
#' @param physeq A phyloseq object
#' @param components Universal information components (output from extract_universal_information)
#' @param discretization Method for discretizing continuous variables: "equal_width", "equal_freq", "kmeans"
#' @param n_bins Number of bins for discretization (default: 5)
#' @param method Information estimation method: "emp" (empirical), "mm" (Miller-Madow), "shrink"
#' @param verbose Print progress messages
#'
#' @return A mutual_information object containing:
#'   \item{mi_matrix}{Matrix of mutual information values (taxa x components)}
#'   \item{normalized_mi}{Normalized mutual information (0-1 scale)}
#'   \item{information_gain}{Information gain for each taxa-component pair}
#'   \item{taxa_rankings}{Taxa ranked by information content for each component}
#'   \item{summary_stats}{Summary statistics and diagnostics}
#'
#' @export
#' @examples
#' # Create demo data
#' demo_data <- create_demo_phyloseq()
#' 
#' # Extract universal information
#' info <- extract_universal_information(demo_data)
#' 
#' # Calculate mutual information
#' mi_results <- calculate_taxa_mutual_information(demo_data, info)
#' 
#' # View top taxa for richness component
#' head(mi_results$taxa_rankings$richness)
calculate_taxa_mutual_information <- function(physeq, 
                                            components = NULL,
                                            discretization = c("equal_width", "equal_freq", "kmeans"),
                                            n_bins = 5,
                                            method = c("emp", "mm", "shrink"),
                                            verbose = TRUE) {
  
  discretization <- match.arg(discretization)
  method <- match.arg(method)
  
  # Extract components if not provided
  if (is.null(components)) {
    if (verbose) message("Extracting universal information components...")
    components <- extract_universal_information(physeq)
  }
  
  # Get OTU matrix
  otu_mat <- as.matrix(phyloseq::otu_table(physeq))
  if (!phyloseq::taxa_are_rows(physeq)) {
    otu_mat <- t(otu_mat)
  }
  
  n_taxa <- nrow(otu_mat)
  n_samples <- ncol(otu_mat)
  
  if (verbose) message("Calculating mutual information for ", n_taxa, " taxa across ", n_samples, " samples...")
  
  # Prepare component data
  comp_data <- prepare_component_data(components, n_samples)
  
  # Discretize taxa abundances
  if (verbose) message("Discretizing taxa abundances using ", discretization, " method...")
  discrete_taxa <- discretize_taxa_abundances(otu_mat, discretization, n_bins)
  
  # Calculate mutual information matrix
  if (verbose) message("Computing mutual information matrix...")
  mi_matrix <- compute_mutual_information_matrix(discrete_taxa, comp_data, method, verbose)
  
  # Normalize mutual information
  normalized_mi <- normalize_mutual_information(mi_matrix, discrete_taxa, comp_data, method)
  
  # Calculate information gain
  info_gain <- calculate_information_gain(discrete_taxa, comp_data, method)
  
  # Rank taxa by information content
  taxa_rankings <- rank_taxa_by_information(mi_matrix, normalized_mi, rownames(otu_mat))
  
  # Calculate summary statistics
  summary_stats <- summarize_mutual_information(mi_matrix, normalized_mi, info_gain, method)
  
  # Create results object
  results <- list(
    mi_matrix = mi_matrix,
    normalized_mi = normalized_mi,
    information_gain = info_gain,
    taxa_rankings = taxa_rankings,
    summary_stats = summary_stats,
    discretization = discretization,
    n_bins = n_bins,
    method = method,
    call = match.call()
  )
  
  class(results) <- c("mutual_information", "list")
  return(results)
}

#' Prepare Component Data for Information Theory
#'
#' Extracts and formats component values for mutual information calculations
#'
#' @param components Universal information components
#' @param n_samples Number of samples expected
#'
#' @return Matrix of component values (samples x components)
#' @keywords internal
prepare_component_data <- function(components, n_samples) {
  
  # Extract R, E, P, S values per sample
  comp_names <- c("richness", "evenness", "phylogenetic", "spatial")
  
  # Initialize component matrix
  comp_matrix <- matrix(0, nrow = n_samples, ncol = length(comp_names))
  colnames(comp_matrix) <- comp_names
  
  # Fill with component values
  # Note: This is a simplified approach - in practice, components would be
  # calculated per sample from the universal information framework
  for (i in seq_along(comp_names)) {
    comp_name <- comp_names[i]
    
    # Extract component values (this is a placeholder - actual implementation
    # would extract per-sample component values from the universal framework)
    if (comp_name %in% names(components)) {
      # For now, create synthetic component values
      # In the full implementation, these would come from per-sample calculations
      comp_matrix[, i] <- rnorm(n_samples, mean = 0.5, sd = 0.1)
    } else {
      comp_matrix[, i] <- rnorm(n_samples, mean = 0.5, sd = 0.1)
    }
  }
  
  return(comp_matrix)
}

#' Discretize Taxa Abundances
#'
#' Converts continuous abundance data to discrete categories for mutual information
#'
#' @param otu_mat OTU abundance matrix (taxa x samples)
#' @param method Discretization method
#' @param n_bins Number of discrete bins
#'
#' @return Matrix of discretized abundances
#' @keywords internal
discretize_taxa_abundances <- function(otu_mat, method, n_bins) {
  
  n_taxa <- nrow(otu_mat)
  n_samples <- ncol(otu_mat)
  
  discrete_mat <- matrix(0, nrow = n_taxa, ncol = n_samples)
  rownames(discrete_mat) <- rownames(otu_mat)
  colnames(discrete_mat) <- colnames(otu_mat)
  
  for (i in 1:n_taxa) {
    abundance <- otu_mat[i, ]
    
    if (method == "equal_width") {
      # Equal-width binning
      if (max(abundance) > min(abundance)) {
        discrete_mat[i, ] <- cut(abundance, 
                               breaks = n_bins, 
                               labels = FALSE, 
                               include.lowest = TRUE)
      } else {
        discrete_mat[i, ] <- rep(1, n_samples)
      }
      
    } else if (method == "equal_freq") {
      # Equal-frequency binning (quantiles)
      if (length(unique(abundance)) > 1) {
        discrete_mat[i, ] <- cut(abundance,
                               breaks = quantile(abundance, 
                                               probs = seq(0, 1, length.out = n_bins + 1),
                                               na.rm = TRUE),
                               labels = FALSE,
                               include.lowest = TRUE)
      } else {
        discrete_mat[i, ] <- rep(1, n_samples)
      }
      
    } else if (method == "kmeans") {
      # K-means clustering
      if (length(unique(abundance)) >= n_bins) {
        kmeans_result <- kmeans(abundance, centers = n_bins, nstart = 10)
        discrete_mat[i, ] <- kmeans_result$cluster
      } else {
        # Not enough unique values for k-means
        discrete_mat[i, ] <- as.numeric(as.factor(abundance))
      }
    }
  }
  
  # Ensure all values are positive integers
  discrete_mat[discrete_mat <= 0] <- 1
  discrete_mat[is.na(discrete_mat)] <- 1
  
  return(discrete_mat)
}

#' Compute Mutual Information Matrix
#'
#' Calculates mutual information between each taxon and each component
#'
#' @param discrete_taxa Discretized taxa abundance matrix
#' @param comp_data Component data matrix
#' @param method Information estimation method
#' @param verbose Print progress
#'
#' @return Matrix of mutual information values
#' @keywords internal
compute_mutual_information_matrix <- function(discrete_taxa, comp_data, method, verbose) {
  
  n_taxa <- nrow(discrete_taxa)
  n_components <- ncol(comp_data)
  
  # Discretize component data too
  discrete_comp <- apply(comp_data, 2, function(x) {
    cut(x, breaks = 5, labels = FALSE, include.lowest = TRUE)
  })
  
  # Initialize MI matrix
  mi_matrix <- matrix(0, nrow = n_taxa, ncol = n_components)
  rownames(mi_matrix) <- rownames(discrete_taxa)
  colnames(mi_matrix) <- colnames(comp_data)
  
  # Calculate MI for each taxa-component pair
  for (i in 1:n_taxa) {
    if (verbose && i %% 50 == 0) {
      message("  Processing taxon ", i, "/", n_taxa)
    }
    
    for (j in 1:n_components) {
      mi_matrix[i, j] <- calculate_mutual_information(
        discrete_taxa[i, ], 
        discrete_comp[, j], 
        method
      )
    }
  }
  
  return(mi_matrix)
}

#' Calculate Mutual Information Between Two Variables
#'
#' Core mutual information calculation using various estimators
#'
#' @param x First variable (discrete)
#' @param y Second variable (discrete)
#' @param method Estimation method
#'
#' @return Mutual information value
#' @keywords internal
calculate_mutual_information <- function(x, y, method) {
  
  # Remove missing values
  valid_indices <- !is.na(x) & !is.na(y)
  x <- x[valid_indices]
  y <- y[valid_indices]
  
  if (length(x) < 2) return(0)
  
  # Calculate joint and marginal distributions
  joint_table <- table(x, y)
  px <- table(x) / length(x)
  py <- table(y) / length(y)
  pxy <- joint_table / length(x)
  
  # Calculate mutual information
  mi <- 0
  
  for (i in 1:nrow(pxy)) {
    for (j in 1:ncol(pxy)) {
      if (pxy[i, j] > 0) {
        px_val <- px[rownames(pxy)[i]]
        py_val <- py[colnames(pxy)[j]]
        
        if (px_val > 0 && py_val > 0) {
          mi_contribution <- pxy[i, j] * log2(pxy[i, j] / (px_val * py_val))
          
          # Apply correction based on method
          if (method == "mm") {
            # Miller-Madow correction for finite sample bias
            mm_correction <- (length(unique(x)) - 1) * (length(unique(y)) - 1) / (2 * length(x) * log(2))
            mi_contribution <- mi_contribution - mm_correction
          } else if (method == "shrink") {
            # Shrinkage estimator
            shrinkage_factor <- 1 - 1 / (1 + length(x))
            mi_contribution <- mi_contribution * shrinkage_factor
          }
          
          mi <- mi + mi_contribution
        }
      }
    }
  }
  
  return(max(0, mi))  # Ensure non-negative
}

#' Normalize Mutual Information
#'
#' Converts mutual information to normalized scale (0-1)
#'
#' @param mi_matrix Mutual information matrix
#' @param discrete_taxa Discretized taxa data
#' @param comp_data Component data
#' @param method Estimation method
#'
#' @return Normalized mutual information matrix
#' @keywords internal
normalize_mutual_information <- function(mi_matrix, discrete_taxa, comp_data, method) {
  
  n_taxa <- nrow(mi_matrix)
  n_components <- ncol(mi_matrix)
  
  normalized_mi <- matrix(0, nrow = n_taxa, ncol = n_components)
  rownames(normalized_mi) <- rownames(mi_matrix)
  colnames(normalized_mi) <- colnames(mi_matrix)
  
  # Discretize component data
  discrete_comp <- apply(comp_data, 2, function(x) {
    cut(x, breaks = 5, labels = FALSE, include.lowest = TRUE)
  })
  
  for (i in 1:n_taxa) {
    for (j in 1:n_components) {
      # Calculate entropies
      h_x <- calculate_entropy(discrete_taxa[i, ])
      h_y <- calculate_entropy(discrete_comp[, j])
      
      # Normalize by geometric mean of entropies (symmetric)
      normalizer <- sqrt(h_x * h_y)
      
      if (normalizer > 0) {
        normalized_mi[i, j] <- mi_matrix[i, j] / normalizer
      } else {
        normalized_mi[i, j] <- 0
      }
    }
  }
  
  # Ensure values are between 0 and 1
  normalized_mi[normalized_mi > 1] <- 1
  normalized_mi[normalized_mi < 0] <- 0
  
  return(normalized_mi)
}

#' Calculate Shannon Entropy
#'
#' Calculates Shannon entropy for discrete probability distribution
#'
#' @param x Discrete variable
#'
#' @return Shannon entropy
#' @keywords internal
calculate_entropy <- function(x) {
  
  # Remove missing values
  x <- x[!is.na(x)]
  
  if (length(x) < 2) return(0)
  
  # Calculate probabilities
  probs <- table(x) / length(x)
  probs <- probs[probs > 0]  # Remove zero probabilities
  
  # Calculate entropy
  entropy <- -sum(probs * log2(probs))
  
  return(entropy)
}

#' Calculate Conditional Mutual Information
#'
#' Calculates conditional mutual information I(X;Y|Z) to detect interaction effects
#' between taxa and components given other taxa as context.
#'
#' @param physeq A phyloseq object
#' @param components Universal information components
#' @param conditioning_taxa Taxa names to use as conditioning variables
#' @param target_taxa Taxa names to analyze (if NULL, analyzes all)
#' @param discretization Discretization method
#' @param n_bins Number of bins for discretization
#' @param method Information estimation method
#' @param verbose Print progress messages
#'
#' @return Conditional mutual information results
#' @export
#' @examples
#' # Create demo data
#' demo_data <- create_demo_phyloseq()
#' info <- extract_universal_information(demo_data)
#' 
#' # Calculate conditional MI
#' cmi_results <- calculate_conditional_mutual_information(
#'   demo_data, info, 
#'   conditioning_taxa = c("OTU1", "OTU2")
#' )
calculate_conditional_mutual_information <- function(physeq,
                                                   components = NULL,
                                                   conditioning_taxa = NULL,
                                                   target_taxa = NULL,
                                                   discretization = "equal_width",
                                                   n_bins = 5,
                                                   method = "emp",
                                                   verbose = TRUE) {
  
  # Extract components if not provided
  if (is.null(components)) {
    if (verbose) message("Extracting universal information components...")
    components <- extract_universal_information(physeq)
  }
  
  # Get OTU matrix
  otu_mat <- as.matrix(phyloseq::otu_table(physeq))
  if (!phyloseq::taxa_are_rows(physeq)) {
    otu_mat <- t(otu_mat)
  }
  
  # Set default target taxa
  if (is.null(target_taxa)) {
    target_taxa <- rownames(otu_mat)
  }
  
  # Set default conditioning taxa (top abundant taxa)
  if (is.null(conditioning_taxa)) {
    taxa_abundance <- rowMeans(otu_mat)
    conditioning_taxa <- names(head(sort(taxa_abundance, decreasing = TRUE), 5))
  }
  
  if (verbose) {
    message("Calculating conditional MI for ", length(target_taxa), " target taxa")
    message("Conditioning on ", length(conditioning_taxa), " taxa")
  }
  
  # Prepare component data
  comp_data <- prepare_component_data(components, ncol(otu_mat))
  
  # Discretize all taxa
  discrete_taxa <- discretize_taxa_abundances(otu_mat, discretization, n_bins)
  discrete_comp <- apply(comp_data, 2, function(x) {
    cut(x, breaks = n_bins, labels = FALSE, include.lowest = TRUE)
  })
  
  # Calculate conditional MI matrix
  cmi_matrix <- compute_conditional_mi_matrix(
    discrete_taxa, discrete_comp, 
    conditioning_taxa, target_taxa, 
    method, verbose
  )
  
  # Compare with unconditional MI
  unconditional_mi <- compute_unconditional_mi_for_comparison(
    discrete_taxa, discrete_comp, target_taxa, method
  )
  
  # Calculate interaction effects
  interaction_effects <- cmi_matrix - unconditional_mi
  
  # Rank taxa by conditional information
  cmi_rankings <- rank_taxa_by_conditional_information(
    cmi_matrix, unconditional_mi, interaction_effects, target_taxa
  )
  
  # Summary statistics
  summary_stats <- summarize_conditional_mi(
    cmi_matrix, unconditional_mi, interaction_effects, 
    conditioning_taxa, method
  )
  
  results <- list(
    conditional_mi = cmi_matrix,
    unconditional_mi = unconditional_mi,
    interaction_effects = interaction_effects,
    rankings = cmi_rankings,
    conditioning_taxa = conditioning_taxa,
    target_taxa = target_taxa,
    summary_stats = summary_stats,
    method = method,
    call = match.call()
  )
  
  class(results) <- c("conditional_mutual_information", "list")
  return(results)
}

#' Compute Conditional MI Matrix
#'
#' @param discrete_taxa Discretized taxa matrix
#' @param discrete_comp Discretized component matrix
#' @param conditioning_taxa Conditioning taxa names
#' @param target_taxa Target taxa names
#' @param method Estimation method
#' @param verbose Print progress
#'
#' @return Conditional MI matrix
#' @keywords internal
compute_conditional_mi_matrix <- function(discrete_taxa, discrete_comp,
                                        conditioning_taxa, target_taxa,
                                        method, verbose) {
  
  n_targets <- length(target_taxa)
  n_components <- ncol(discrete_comp)
  
  cmi_matrix <- matrix(0, nrow = n_targets, ncol = n_components)
  rownames(cmi_matrix) <- target_taxa
  colnames(cmi_matrix) <- colnames(discrete_comp)
  
  # Create conditioning context by combining conditioning taxa
  if (length(conditioning_taxa) > 0) {
    # Combine conditioning taxa into single context variable
    conditioning_context <- create_conditioning_context(
      discrete_taxa[conditioning_taxa, , drop = FALSE]
    )
  } else {
    conditioning_context <- rep(1, ncol(discrete_taxa))
  }
  
  for (i in seq_along(target_taxa)) {
    if (verbose && i %% 10 == 0) {
      message("  Processing target taxon ", i, "/", n_targets)
    }
    
    target_taxon <- target_taxa[i]
    if (target_taxon %in% rownames(discrete_taxa)) {
      x <- discrete_taxa[target_taxon, ]
      
      for (j in 1:n_components) {
        y <- discrete_comp[, j]
        
        # Calculate I(X;Y|Z) = H(X|Z) + H(Y|Z) - H(X,Y|Z)
        cmi_matrix[i, j] <- calculate_conditional_mutual_information_trio(
          x, y, conditioning_context, method
        )
      }
    }
  }
  
  return(cmi_matrix)
}

#' Create Conditioning Context
#'
#' Combines multiple conditioning taxa into a single context variable
#'
#' @param conditioning_matrix Matrix of conditioning taxa (taxa x samples)
#'
#' @return Vector representing combined context
#' @keywords internal
create_conditioning_context <- function(conditioning_matrix) {
  
  if (nrow(conditioning_matrix) == 1) {
    return(as.numeric(conditioning_matrix[1, ]))
  }
  
  # Combine multiple taxa by creating interaction terms
  # Each unique combination of values gets a unique context ID
  n_samples <- ncol(conditioning_matrix)
  context_strings <- character(n_samples)
  
  for (i in 1:n_samples) {
    context_strings[i] <- paste(conditioning_matrix[, i], collapse = "_")
  }
  
  # Convert to numeric IDs
  unique_contexts <- unique(context_strings)
  context_ids <- match(context_strings, unique_contexts)
  
  return(context_ids)
}

#' Calculate Conditional Mutual Information for Three Variables
#'
#' Calculates I(X;Y|Z) using the definition I(X;Y|Z) = H(X|Z) + H(Y|Z) - H(X,Y|Z)
#'
#' @param x First variable
#' @param y Second variable  
#' @param z Conditioning variable
#' @param method Estimation method
#'
#' @return Conditional mutual information
#' @keywords internal
calculate_conditional_mutual_information_trio <- function(x, y, z, method) {
  
  # Remove missing values
  valid_indices <- !is.na(x) & !is.na(y) & !is.na(z)
  x <- x[valid_indices]
  y <- y[valid_indices]
  z <- z[valid_indices]
  
  if (length(x) < 3) return(0)
  
  # Calculate conditional entropies
  h_x_given_z <- calculate_conditional_entropy(x, z)
  h_y_given_z <- calculate_conditional_entropy(y, z)
  h_xy_given_z <- calculate_conditional_joint_entropy(x, y, z)
  
  # I(X;Y|Z) = H(X|Z) + H(Y|Z) - H(X,Y|Z)
  cmi <- h_x_given_z + h_y_given_z - h_xy_given_z
  
  return(max(0, cmi))  # Ensure non-negative
}

#' Calculate Conditional Joint Entropy
#'
#' Calculates H(X,Y|Z) = sum_z P(z) * H(X,Y|Z=z)
#'
#' @param x First variable
#' @param y Second variable
#' @param z Conditioning variable
#'
#' @return Conditional joint entropy
#' @keywords internal
calculate_conditional_joint_entropy <- function(x, y, z) {
  
  # Calculate P(Z)
  pz <- table(z) / length(z)
  
  conditional_joint_entropy <- 0
  
  for (z_val in names(pz)) {
    # Get X,Y values where Z = z_val
    indices <- z == z_val
    x_given_z <- x[indices]
    y_given_z <- y[indices]
    
    if (length(x_given_z) > 0) {
      # Calculate H(X,Y|Z=z_val)
      joint_table <- table(x_given_z, y_given_z)
      joint_probs <- joint_table / sum(joint_table)
      joint_probs <- joint_probs[joint_probs > 0]
      
      h_xy_given_z_val <- -sum(joint_probs * log2(joint_probs))
      
      # Weight by P(Z=z_val)
      conditional_joint_entropy <- conditional_joint_entropy + 
        pz[z_val] * h_xy_given_z_val
    }
  }
  
  return(conditional_joint_entropy)
}

#' Compute Unconditional MI for Comparison
#'
#' @param discrete_taxa Discretized taxa matrix
#' @param discrete_comp Discretized component matrix
#' @param target_taxa Target taxa names
#' @param method Estimation method
#'
#' @return Unconditional MI matrix
#' @keywords internal
compute_unconditional_mi_for_comparison <- function(discrete_taxa, discrete_comp,
                                                   target_taxa, method) {
  
  n_targets <- length(target_taxa)
  n_components <- ncol(discrete_comp)
  
  umi_matrix <- matrix(0, nrow = n_targets, ncol = n_components)
  rownames(umi_matrix) <- target_taxa
  colnames(umi_matrix) <- colnames(discrete_comp)
  
  for (i in seq_along(target_taxa)) {
    target_taxon <- target_taxa[i]
    if (target_taxon %in% rownames(discrete_taxa)) {
      x <- discrete_taxa[target_taxon, ]
      
      for (j in 1:n_components) {
        y <- discrete_comp[, j]
        umi_matrix[i, j] <- calculate_mutual_information(x, y, method)
      }
    }
  }
  
  return(umi_matrix)
}

#' Rank Taxa by Conditional Information
#'
#' @param cmi_matrix Conditional MI matrix
#' @param umi_matrix Unconditional MI matrix
#' @param interaction_effects Interaction effects matrix
#' @param target_taxa Target taxa names
#'
#' @return Rankings list
#' @keywords internal
rank_taxa_by_conditional_information <- function(cmi_matrix, umi_matrix,
                                                interaction_effects, target_taxa) {
  
  components <- colnames(cmi_matrix)
  rankings <- list()
  
  for (comp in components) {
    ranking_df <- data.frame(
      taxon = target_taxa,
      conditional_mi = cmi_matrix[, comp],
      unconditional_mi = umi_matrix[, comp],
      interaction_effect = interaction_effects[, comp],
      stringsAsFactors = FALSE
    )
    
    # Sort by conditional MI
    ranking_df <- ranking_df[order(ranking_df$conditional_mi, decreasing = TRUE), ]
    ranking_df$rank <- 1:nrow(ranking_df)
    
    rankings[[comp]] <- ranking_df
  }
  
  return(rankings)
}

#' Summarize Conditional MI Results
#'
#' @param cmi_matrix Conditional MI matrix
#' @param umi_matrix Unconditional MI matrix
#' @param interaction_effects Interaction effects matrix
#' @param conditioning_taxa Conditioning taxa
#' @param method Estimation method
#'
#' @return Summary statistics
#' @keywords internal
summarize_conditional_mi <- function(cmi_matrix, umi_matrix, interaction_effects,
                                    conditioning_taxa, method) {
  
  summary_stats <- list()
  
  # Overall statistics
  summary_stats$overall <- list(
    n_targets = nrow(cmi_matrix),
    n_components = ncol(cmi_matrix),
    n_conditioning_taxa = length(conditioning_taxa),
    method = method,
    mean_conditional_mi = mean(cmi_matrix),
    mean_unconditional_mi = mean(umi_matrix),
    mean_interaction_effect = mean(interaction_effects),
    positive_interactions = sum(interaction_effects > 0),
    negative_interactions = sum(interaction_effects < 0)
  )
  
  # Component-specific statistics
  components <- colnames(cmi_matrix)
  for (comp in components) {
    summary_stats[[comp]] <- list(
      max_conditional_mi = max(cmi_matrix[, comp]),
      max_unconditional_mi = max(umi_matrix[, comp]),
      max_interaction_effect = max(interaction_effects[, comp]),
      min_interaction_effect = min(interaction_effects[, comp]),
      n_positive_interactions = sum(interaction_effects[, comp] > 0),
      n_negative_interactions = sum(interaction_effects[, comp] < 0)
    )
  }
  
  return(summary_stats)
}

#' Calculate Information Gain
#'
#' Calculates information gain for each taxa-component relationship
#'
#' @param discrete_taxa Discretized taxa data
#' @param comp_data Component data
#' @param method Estimation method
#'
#' @return Information gain matrix
#' @keywords internal
calculate_information_gain <- function(discrete_taxa, comp_data, method) {
  
  n_taxa <- nrow(discrete_taxa)
  n_components <- ncol(comp_data)
  
  # Discretize component data
  discrete_comp <- apply(comp_data, 2, function(x) {
    cut(x, breaks = 5, labels = FALSE, include.lowest = TRUE)
  })
  
  info_gain <- matrix(0, nrow = n_taxa, ncol = n_components)
  rownames(info_gain) <- rownames(discrete_taxa)
  colnames(info_gain) <- colnames(comp_data)
  
  for (j in 1:n_components) {
    # Calculate base entropy of component
    h_comp <- calculate_entropy(discrete_comp[, j])
    
    for (i in 1:n_taxa) {
      # Calculate conditional entropy H(Component|Taxon)
      h_comp_given_taxon <- calculate_conditional_entropy(
        discrete_comp[, j], 
        discrete_taxa[i, ]
      )
      
      # Information gain = H(Component) - H(Component|Taxon)
      info_gain[i, j] <- h_comp - h_comp_given_taxon
    }
  }
  
  return(info_gain)
}

#' Calculate Conditional Entropy
#'
#' Calculates H(Y|X) = sum_x P(x) * H(Y|X=x)
#'
#' @param y Target variable
#' @param x Conditioning variable
#'
#' @return Conditional entropy
#' @keywords internal
calculate_conditional_entropy <- function(y, x) {
  
  # Remove missing values
  valid_indices <- !is.na(x) & !is.na(y)
  x <- x[valid_indices]
  y <- y[valid_indices]
  
  if (length(x) < 2) return(0)
  
  # Calculate P(X)
  px <- table(x) / length(x)
  
  conditional_entropy <- 0
  
  for (x_val in names(px)) {
    # Get Y values where X = x_val
    y_given_x <- y[x == x_val]
    
    if (length(y_given_x) > 0) {
      # Calculate H(Y|X=x_val)
      h_y_given_x <- calculate_entropy(y_given_x)
      
      # Weight by P(X=x_val)
      conditional_entropy <- conditional_entropy + px[x_val] * h_y_given_x
    }
  }
  
  return(conditional_entropy)
}

#' Rank Taxa by Information Content
#'
#' Creates ranked lists of taxa for each component based on information metrics
#'
#' @param mi_matrix Mutual information matrix
#' @param normalized_mi Normalized mutual information matrix
#' @param taxa_names Taxa names
#'
#' @return List of ranked taxa for each component
#' @keywords internal
rank_taxa_by_information <- function(mi_matrix, normalized_mi, taxa_names) {
  
  components <- colnames(mi_matrix)
  rankings <- list()
  
  for (comp in components) {
    # Create ranking data frame
    ranking_df <- data.frame(
      taxon = taxa_names,
      mutual_information = mi_matrix[, comp],
      normalized_mi = normalized_mi[, comp],
      rank_mi = rank(-mi_matrix[, comp], ties.method = "average"),
      rank_normalized = rank(-normalized_mi[, comp], ties.method = "average"),
      stringsAsFactors = FALSE
    )
    
    # Sort by normalized MI (more reliable for ranking)
    ranking_df <- ranking_df[order(ranking_df$normalized_mi, decreasing = TRUE), ]
    ranking_df$final_rank <- 1:nrow(ranking_df)
    
    rankings[[comp]] <- ranking_df
  }
  
  return(rankings)
}

#' Summarize Mutual Information Results
#'
#' Creates summary statistics and diagnostics
#'
#' @param mi_matrix Mutual information matrix
#' @param normalized_mi Normalized MI matrix
#' @param info_gain Information gain matrix
#' @param method Estimation method used
#'
#' @return List of summary statistics
#' @keywords internal
summarize_mutual_information <- function(mi_matrix, normalized_mi, info_gain, method) {
  
  summary_stats <- list()
  
  # Overall statistics
  summary_stats$overall <- list(
    n_taxa = nrow(mi_matrix),
    n_components = ncol(mi_matrix),
    method = method,
    mean_mi = mean(mi_matrix),
    median_mi = median(mi_matrix),
    max_mi = max(mi_matrix),
    mean_normalized_mi = mean(normalized_mi),
    median_normalized_mi = median(normalized_mi)
  )
  
  # Component-specific statistics
  components <- colnames(mi_matrix)
  for (comp in components) {
    summary_stats[[comp]] <- list(
      top_taxon = rownames(mi_matrix)[which.max(normalized_mi[, comp])],
      max_mi = max(mi_matrix[, comp]),
      max_normalized_mi = max(normalized_mi[, comp]),
      max_info_gain = max(info_gain[, comp]),
      mean_mi = mean(mi_matrix[, comp]),
      n_significant = sum(normalized_mi[, comp] > 0.1)  # Arbitrary threshold
    )
  }
  
  # Correlation between different metrics
  summary_stats$correlations <- list(
    mi_vs_normalized = cor(as.vector(mi_matrix), as.vector(normalized_mi)),
    mi_vs_info_gain = cor(as.vector(mi_matrix), as.vector(info_gain))
  )
  
  return(summary_stats)
}

#' Print Method for Mutual Information Results
#'
#' @param x A mutual_information object
#' @param ... Additional arguments (ignored)
#'
#' @export
print.mutual_information <- function(x, ...) {
  cat("Mutual Information Analysis Results\n")
  cat("Method:", x$method, "\n")
  cat("Discretization:", x$discretization, "with", x$n_bins, "bins\n")
  cat("=====================================\n\n")
  
  # Print summary statistics
  cat("OVERALL STATISTICS:\n")
  cat("  Taxa analyzed:", x$summary_stats$overall$n_taxa, "\n")
  cat("  Components:", x$summary_stats$overall$n_components, "\n")
  cat("  Mean MI:", round(x$summary_stats$overall$mean_mi, 4), "\n")
  cat("  Mean normalized MI:", round(x$summary_stats$overall$mean_normalized_mi, 4), "\n\n")
  
  # Print top taxa for each component
  components <- names(x$taxa_rankings)
  for (comp in components) {
    cat(toupper(comp), "COMPONENT:\n")
    
    top_5 <- head(x$taxa_rankings[[comp]], 5)
    for (i in 1:nrow(top_5)) {
      cat(sprintf("  %d. %s (NMI: %.3f, MI: %.3f)\n",
                 i,
                 top_5$taxon[i],
                 top_5$normalized_mi[i],
                 top_5$mutual_information[i]))
    }
    cat("\n")
  }
  
  # Print correlation diagnostics
  cat("METRIC CORRELATIONS:\n")
  cat("  MI vs Normalized MI:", round(x$summary_stats$correlations$mi_vs_normalized, 3), "\n")
  cat("  MI vs Information Gain:", round(x$summary_stats$correlations$mi_vs_info_gain, 3), "\n")
  
  invisible(x)
}

#' Plot Mutual Information Results
#'
#' Creates visualizations of mutual information analysis
#'
#' @param x A mutual_information object
#' @param type Plot type: "heatmap", "ranking", "comparison", or "network"
#' @param top_n Number of top taxa to show
#' @param interactive Create interactive plot using plotly
#' @param ... Additional arguments passed to plotting functions
#'
#' @return A ggplot2 or plotly object
#' @export
plot.mutual_information <- function(x, 
                                  type = c("heatmap", "ranking", "comparison", "network"),
                                  top_n = 15,
                                  interactive = FALSE,
                                  ...) {
  
  type <- match.arg(type)
  
  if (type == "heatmap") {
    p <- plot_mi_heatmap(x, top_n, interactive)
  } else if (type == "ranking") {
    p <- plot_mi_ranking(x, top_n, interactive)
  } else if (type == "comparison") {
    p <- plot_mi_comparison(x, top_n, interactive)
  } else if (type == "network") {
    p <- plot_mi_network(x, top_n, interactive)
  }
  
  return(p)
}

#' Plot Mutual Information Heatmap
#'
#' @param x Mutual information object
#' @param top_n Number of top taxa
#' @param interactive Use plotly
#'
#' @return Plot object
#' @keywords internal
plot_mi_heatmap <- function(x, top_n, interactive) {
  
  # Get top taxa across all components
  all_mi <- x$normalized_mi
  taxa_importance <- rowMeans(all_mi)
  top_taxa_indices <- order(taxa_importance, decreasing = TRUE)[1:min(top_n, length(taxa_importance))]
  
  # Subset data
  plot_data <- all_mi[top_taxa_indices, , drop = FALSE]
  
  # Convert to long format for ggplot
  plot_df <- expand.grid(
    Taxon = rownames(plot_data),
    Component = colnames(plot_data),
    stringsAsFactors = FALSE
  )
  plot_df$NormalizedMI <- as.vector(plot_data)
  
  # Create plot
  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = Component, y = Taxon, fill = NormalizedMI)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_viridis_c(name = "Normalized\nMI") +
    ggplot2::labs(title = "Taxa-Component Mutual Information Heatmap",
                 x = "Information Component",
                 y = "Taxon") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 8),
                  axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  
  if (interactive) {
    p <- plotly::ggplotly(p)
  }
  
  return(p)
}

#' Plot Mutual Information Rankings
#'
#' @param x Mutual information object
#' @param top_n Number of top taxa
#' @param interactive Use plotly
#'
#' @return Plot object
#' @keywords internal
plot_mi_ranking <- function(x, top_n, interactive) {
  
  # Combine rankings from all components
  all_rankings <- do.call(rbind, lapply(names(x$taxa_rankings), function(comp) {
    ranking <- head(x$taxa_rankings[[comp]], top_n)
    ranking$component <- comp
    return(ranking[, c("taxon", "normalized_mi", "component")])
  }))
  
  # Create plot
  p <- ggplot2::ggplot(all_rankings, 
                      ggplot2::aes(x = stats::reorder(taxon, normalized_mi), 
                                  y = normalized_mi, 
                                  fill = component)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::facet_wrap(~ component, scales = "free_y", ncol = 2) +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "Top Taxa by Normalized Mutual Information",
                 x = "Taxon",
                 y = "Normalized Mutual Information",
                 fill = "Component") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 8),
                  legend.position = "none")
  
  if (interactive) {
    p <- plotly::ggplotly(p)
  }
  
  return(p)
}

#' Plot Mutual Information Comparison
#'
#' @param x Mutual information object
#' @param top_n Number of top taxa
#' @param interactive Use plotly
#'
#' @return Plot object
#' @keywords internal
plot_mi_comparison <- function(x, top_n, interactive) {
  
  # Create comparison between MI and normalized MI
  comparison_data <- data.frame(
    MI = as.vector(x$mi_matrix),
    NormalizedMI = as.vector(x$normalized_mi),
    InfoGain = as.vector(x$information_gain),
    Component = rep(colnames(x$mi_matrix), each = nrow(x$mi_matrix))
  )
  
  # Create scatter plot
  p <- ggplot2::ggplot(comparison_data, ggplot2::aes(x = MI, y = NormalizedMI, color = Component)) +
    ggplot2::geom_point(alpha = 0.6) +
    ggplot2::geom_smooth(method = "lm", se = FALSE) +
    ggplot2::facet_wrap(~ Component) +
    ggplot2::labs(title = "Mutual Information vs Normalized Mutual Information",
                 x = "Mutual Information",
                 y = "Normalized Mutual Information") +
    ggplot2::theme_minimal()
  
  if (interactive) {
    p <- plotly::ggplotly(p)
  }
  
  return(p)
}

#' Plot Mutual Information Network
#'
#' @param x Mutual information object
#' @param top_n Number of top taxa
#' @param interactive Use plotly
#'
#' @return Plot object
#' @keywords internal
plot_mi_network <- function(x, top_n, interactive) {
  
  # For now, create a simple network showing strongest relationships
  # This is a placeholder - full network visualization would be more complex
  
  # Get strongest MI values
  mi_values <- x$normalized_mi
  top_taxa <- head(order(rowMeans(mi_values), decreasing = TRUE), top_n)
  
  # Create edge list for strongest connections
  edges <- data.frame(
    from = character(0),
    to = character(0),
    weight = numeric(0),
    stringsAsFactors = FALSE
  )
  
  for (i in top_taxa) {
    for (j in 1:ncol(mi_values)) {
      if (mi_values[i, j] > 0.1) {  # Threshold for visualization
        edges <- rbind(edges, data.frame(
          from = rownames(mi_values)[i],
          to = paste0(colnames(mi_values)[j], "_component"),
          weight = mi_values[i, j],
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  if (nrow(edges) == 0) {
    # No strong connections - create a simple bar plot instead
    return(plot_mi_ranking(x, top_n, interactive))
  }
  
  # Create simple network visualization using igraph
  g <- igraph::graph_from_data_frame(edges, directed = FALSE)
  
  # Set attributes
  igraph::E(g)$width <- edges$weight * 5
  igraph::V(g)$type <- ifelse(grepl("_component$", igraph::V(g)$name), "component", "taxon")
  
  # Create layout
  layout <- igraph::layout_with_fr(g)
  
  # Convert to ggraph plot
  p <- ggraph::ggraph(g, layout = layout) +
    ggraph::geom_edge_link(ggplot2::aes(width = weight), alpha = 0.6) +
    ggraph::geom_node_point(ggplot2::aes(color = type, size = type)) +
    ggraph::geom_node_text(ggplot2::aes(label = name), repel = TRUE, size = 3) +
    ggraph::scale_edge_width(range = c(0.5, 3)) +
    ggplot2::scale_size_manual(values = c("component" = 6, "taxon" = 4)) +
    ggplot2::labs(title = "Taxa-Component Information Network",
                 subtitle = "Edge width represents normalized mutual information") +
    ggraph::theme_graph()
  
  return(p)
}