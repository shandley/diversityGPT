#' Beta Diversity Decomposition Framework (Theoretical/Experimental)
#'
#' @description
#' Experimental implementation of beta diversity decomposition into
#' R (Richness), E (Evenness), P (Phylogenetic), and S (Spatial) components.
#' This extends the diversityGPT framework to between-sample diversity.
#'
#' @note This is experimental code for research purposes
#' @author diversityGPT Development Team

#' Decompose Beta Diversity Between Two Samples
#'
#' @param sample_i Numeric vector of abundances for sample i
#' @param sample_j Numeric vector of abundances for sample j
#' @param tree Optional phylogenetic tree (phylo object)
#' @param coords_i Optional spatial coordinates for sample i (numeric vector)
#' @param coords_j Optional spatial coordinates for sample j (numeric vector)
#' @param method Decomposition method: "information", "variance", or "geometric"
#'
#' @return List containing R_beta, E_beta, P_beta, S_beta components
#' @export
#' @examples
#' \dontrun{
#' # Create example data
#' sample1 <- c(10, 20, 0, 5, 0, 15)
#' sample2 <- c(0, 25, 10, 5, 5, 0)
#' 
#' # Basic decomposition (no phylogeny or spatial)
#' beta_components <- decompose_beta_diversity(sample1, sample2)
#' 
#' # With phylogeny
#' tree <- ape::rtree(n = 6)
#' beta_components <- decompose_beta_diversity(sample1, sample2, tree = tree)
#' }
decompose_beta_diversity <- function(sample_i, sample_j, 
                                   tree = NULL,
                                   coords_i = NULL, 
                                   coords_j = NULL,
                                   method = "information",
                                   spatial_max_dist = NULL,
                                   spatial_decay_rate = NULL) {
  
  # Ensure samples are same length
  if (length(sample_i) != length(sample_j)) {
    stop("Samples must have the same number of taxa")
  }
  
  # Initialize components
  components <- list()
  
  # 1. R_beta: Richness Turnover Component
  components$R_beta <- calculate_R_beta(sample_i, sample_j, method)
  
  # 2. E_beta: Evenness Turnover Component  
  components$E_beta <- calculate_E_beta(sample_i, sample_j, method)
  
  # 3. P_beta: Phylogenetic Turnover Component
  if (!is.null(tree)) {
    components$P_beta <- calculate_P_beta(sample_i, sample_j, tree, method)
  } else {
    components$P_beta <- 0
  }
  
  # 4. S_beta: Spatial Turnover Component
  if (!is.null(coords_i) && !is.null(coords_j)) {
    components$S_beta <- calculate_S_beta(coords_i, coords_j, 
                                          sample_i, sample_j, method,
                                          max_dist = spatial_max_dist,
                                          decay_rate = spatial_decay_rate)
  } else {
    components$S_beta <- 0
  }
  
  # Normalize components to [0,1]
  components <- normalize_beta_components(components)
  
  # Add summary statistics
  components$total_beta <- sum(unlist(components))
  components$dominant_component <- names(which.max(unlist(components[1:4])))
  
  class(components) <- c("beta_decomposition", "list")
  
  return(components)
}

#' Calculate R_beta (Richness Turnover)
#' @keywords internal
calculate_R_beta <- function(sample_i, sample_j, method = "information") {
  
  # Get presence/absence
  present_i <- sample_i > 0
  present_j <- sample_j > 0
  
  if (method == "information") {
    # Information-theoretic approach
    # Joint entropy - average of individual entropies
    
    # Species in each sample
    n_i <- sum(present_i)
    n_j <- sum(present_j)
    n_both <- sum(present_i & present_j)
    n_either <- sum(present_i | present_j)
    
    # Probabilities
    p_i <- n_i / length(sample_i)
    p_j <- n_j / length(sample_j)
    p_both <- n_both / length(sample_i)
    p_either <- n_either / length(sample_i)
    
    # Shannon entropy components with small pseudocount to avoid log(0)
    # Standard practice in information theory (see Cover & Thomas 2006)
    pseudocount <- 1e-10
    H_i <- -p_i * log(p_i + pseudocount)
    H_j <- -p_j * log(p_j + pseudocount)
    H_union <- -p_either * log(p_either + pseudocount)
    
    # Richness turnover as excess joint entropy
    # Using average entropy (0.5 weight) following Jost (2007)
    avg_weight <- 0.5
    R_beta <- H_union - avg_weight * (H_i + H_j)
    
  } else if (method == "variance") {
    # Variance partitioning approach
    # Based on Legendre & De Cáceres 2013
    
    a <- sum(present_i & present_j)  # Shared species
    b <- sum(present_i & !present_j) # Unique to i
    c <- sum(!present_i & present_j) # Unique to j
    
    # Sørensen-based turnover
    R_beta <- (b + c) / (2 * a + b + c)
    
  } else if (method == "geometric") {
    # Geometric approach
    # Based on Jaccard distance
    
    intersection <- sum(present_i & present_j)
    union <- sum(present_i | present_j)
    
    R_beta <- 1 - (intersection / union)
  }
  
  return(R_beta)
}

#' Calculate E_beta (Evenness Turnover)
#' @keywords internal
calculate_E_beta <- function(sample_i, sample_j, method = "information") {
  
  # Remove zeros and normalize
  nonzero_i <- sample_i[sample_i > 0]
  nonzero_j <- sample_j[sample_j > 0]
  
  if (length(nonzero_i) == 0 || length(nonzero_j) == 0) {
    return(1)  # Maximum dissimilarity if one sample is empty
  }
  
  # Normalize to relative abundances
  p_i <- nonzero_i / sum(nonzero_i)
  p_j <- nonzero_j / sum(nonzero_j)
  
  if (method == "information") {
    # Kullback-Leibler divergence approach
    
    # Need same length - use union of species
    all_species <- union(which(sample_i > 0), which(sample_j > 0))
    
    # Full probability vectors (with zeros)
    p_full_i <- numeric(length(all_species))
    p_full_j <- numeric(length(all_species))
    
    p_full_i[sample_i[all_species] > 0] <- p_i
    p_full_j[sample_j[all_species] > 0] <- p_j
    
    # Add pseudocount to avoid log(0)
    p_full_i <- (p_full_i + 1e-10) / sum(p_full_i + 1e-10)
    p_full_j <- (p_full_j + 1e-10) / sum(p_full_j + 1e-10)
    
    # Average distribution
    p_avg <- 0.5 * (p_full_i + p_full_j)
    
    # Symmetric KL divergence (Jensen-Shannon divergence)
    kl_i <- sum(p_full_i * log(p_full_i / p_avg))
    kl_j <- sum(p_full_j * log(p_full_j / p_avg))
    
    E_beta <- 0.5 * (kl_i + kl_j)
    
  } else if (method == "variance") {
    # Variance in log-ratios
    
    # Get shared species only
    shared <- which(sample_i > 0 & sample_j > 0)
    
    if (length(shared) < 2) {
      return(1)  # Maximum dissimilarity if few shared species
    }
    
    # Log-ratios of abundances
    log_ratios <- log((sample_i[shared] + 1) / (sample_j[shared] + 1))
    
    # Variance of log-ratios indicates evenness difference
    E_beta <- var(log_ratios) / (1 + var(log_ratios))  # Normalize to [0,1]
    
  } else if (method == "geometric") {
    # Bray-Curtis based approach
    
    # Get union of species
    all_species <- union(which(sample_i > 0), which(sample_j > 0))
    
    # Calculate Bray-Curtis
    sum_min <- sum(pmin(sample_i[all_species], sample_j[all_species]))
    sum_total <- sum(sample_i[all_species]) + sum(sample_j[all_species])
    
    E_beta <- 1 - (2 * sum_min / sum_total)
  }
  
  return(E_beta)
}

#' Calculate P_beta (Phylogenetic Turnover)
#' @keywords internal
calculate_P_beta <- function(sample_i, sample_j, tree, method = "information") {
  
  if (!inherits(tree, "phylo")) {
    stop("tree must be a phylo object")
  }
  
  # Ensure tree tips match sample order
  if (length(tree$tip.label) != length(sample_i)) {
    stop("Number of tree tips must match number of taxa")
  }
  
  # Get presence/absence
  present_i <- which(sample_i > 0)
  present_j <- which(sample_j > 0)
  
  if (method == "information") {
    # Phylogenetic entropy approach
    
    # Get subtrees
    if (length(present_i) > 1) {
      subtree_i <- ape::keep.tip(tree, present_i)
      phylo_entropy_i <- sum(subtree_i$edge.length)
    } else {
      phylo_entropy_i <- 0
    }
    
    if (length(present_j) > 1) {
      subtree_j <- ape::keep.tip(tree, present_j)
      phylo_entropy_j <- sum(subtree_j$edge.length)
    } else {
      phylo_entropy_j <- 0
    }
    
    # Union tree
    present_both <- union(present_i, present_j)
    if (length(present_both) > 1) {
      subtree_both <- ape::keep.tip(tree, present_both)
      phylo_entropy_both <- sum(subtree_both$edge.length)
    } else {
      phylo_entropy_both <- 0
    }
    
    # Phylogenetic turnover as excess phylogenetic diversity
    total_branch_length <- sum(tree$edge.length)
    P_beta <- (phylo_entropy_both - 0.5 * (phylo_entropy_i + phylo_entropy_j)) / 
              total_branch_length
    
  } else if (method == "variance") {
    # UniFrac-based approach
    
    # Simple unweighted UniFrac
    # This is a simplified version - full UniFrac is more complex
    
    # Get presence/absence matrix for these two samples
    comm_matrix <- matrix(0, nrow = 2, ncol = length(sample_i))
    comm_matrix[1, ] <- sample_i > 0
    comm_matrix[2, ] <- sample_j > 0
    
    # Calculate pairwise phylogenetic distance
    if (requireNamespace("picante", quietly = TRUE)) {
      phylo_dist <- picante::unifrac(comm_matrix, tree)
      P_beta <- phylo_dist[1, 2]
    } else {
      # Fallback to simple phylogenetic Jaccard
      dist_matrix <- ape::cophenetic.phylo(tree)
      mean_dist_i <- mean(dist_matrix[present_i, present_i])
      mean_dist_j <- mean(dist_matrix[present_j, present_j])
      mean_dist_ij <- mean(dist_matrix[present_i, present_j])
      
      P_beta <- mean_dist_ij / (mean_dist_i + mean_dist_j + mean_dist_ij)
    }
    
  } else if (method == "geometric") {
    # Phylogenetic Sørensen approach
    
    # Get branch lengths unique to each sample
    if (length(present_i) > 1 && length(present_j) > 1) {
      # This is simplified - full implementation would track actual branches
      faith_i <- sum(ape::keep.tip(tree, present_i)$edge.length)
      faith_j <- sum(ape::keep.tip(tree, present_j)$edge.length)
      faith_both <- sum(ape::keep.tip(tree, union(present_i, present_j))$edge.length)
      
      P_beta <- (faith_both - min(faith_i, faith_j)) / faith_both
    } else {
      P_beta <- 1  # Maximum dissimilarity if one sample has <2 species
    }
  }
  
  return(max(0, min(1, P_beta)))  # Ensure [0,1]
}

#' Calculate S_beta (Spatial Turnover)
#' @param coords_i Spatial coordinates for sample i
#' @param coords_j Spatial coordinates for sample j
#' @param sample_i Abundance vector for sample i
#' @param sample_j Abundance vector for sample j
#' @param method Calculation method
#' @param max_dist Optional maximum distance for normalization
#' @param decay_rate Optional decay rate parameter
#' @keywords internal
calculate_S_beta <- function(coords_i, coords_j, sample_i, sample_j, 
                            method = "information",
                            max_dist = NULL,
                            decay_rate = NULL) {
  
  # Calculate spatial distance
  spatial_dist <- sqrt(sum((coords_i - coords_j)^2))
  
  # If max_dist not provided, we cannot properly normalize
  # This should be calculated from the full spatial extent
  if (is.null(max_dist) && method == "geometric") {
    cli::cli_warn("Maximum spatial distance not provided. S_beta calculation may be unreliable.")
    cli::cli_alert_info("Provide max_dist parameter or calculate from full dataset spatial extent.")
    return(NA_real_)
  }
  
  if (method == "information") {
    # Information decay with distance
    
    # Use provided decay rate or default
    # Decay rate should ideally be estimated from variogram analysis
    if (is.null(decay_rate)) {
      decay_rate <- 1.0  # Default unit decay rate
      cli::cli_alert_info("Using default decay rate of 1.0. Consider estimating from variogram.")
    }
    
    # Spatial information as distance-decay
    S_beta <- 1 - exp(-spatial_dist / decay_rate)
    
  } else if (method == "variance") {
    # Variance partitioning approach
    
    # Calculate regular beta diversity (Bray-Curtis)
    all_species <- union(which(sample_i > 0), which(sample_j > 0))
    if (length(all_species) == 0) {
      return(NA_real_)  # No species in either sample
    }
    
    sum_min <- sum(pmin(sample_i[all_species], sample_j[all_species]))
    sum_total <- sum(sample_i[all_species]) + sum(sample_j[all_species])
    
    if (sum_total == 0) {
      return(NA_real_)  # Empty samples
    }
    
    bray_curtis <- 1 - (2 * sum_min / sum_total)
    
    # Distance-decay model with configurable rate
    if (is.null(decay_rate)) {
      decay_rate <- 1.0
    }
    expected_beta <- 1 - exp(-spatial_dist / decay_rate)
    
    # Spatial component is the predictable part
    S_beta <- min(bray_curtis, expected_beta)
    
  } else if (method == "geometric") {
    # Geometric decay approach
    
    # Normalize distance to [0,1] based on maximum distance
    S_beta <- min(1, spatial_dist / max_dist)
  }
  
  return(S_beta)
}

#' Normalize beta components to [0,1] scale
#' @keywords internal
normalize_beta_components <- function(components) {
  
  # Extract numeric components
  values <- unlist(components[c("R_beta", "E_beta", "P_beta", "S_beta")])
  
  # Ensure non-negative
  values[values < 0] <- 0
  
  # If all zeros, return as-is
  if (sum(values) == 0) {
    return(components)
  }
  
  # Normalize so max component = 1
  max_val <- max(values)
  if (max_val > 0) {
    components$R_beta <- components$R_beta / max_val
    components$E_beta <- components$E_beta / max_val
    components$P_beta <- components$P_beta / max_val
    components$S_beta <- components$S_beta / max_val
  }
  
  return(components)
}

#' Decompose Beta Diversity for Multiple Samples
#'
#' @param community_matrix Samples x taxa abundance matrix
#' @param tree Optional phylogenetic tree
#' @param coords Optional spatial coordinates (n x 2 matrix)
#' @param method Decomposition method
#' @param summary_method How to summarize pairwise comparisons: 
#'   "centroid", "average", or "pcoa"
#'
#' @return Beta diversity decomposition results
#' @export
decompose_beta_diversity_matrix <- function(community_matrix,
                                          tree = NULL,
                                          coords = NULL, 
                                          method = "information",
                                          summary_method = "centroid",
                                          spatial_max_dist = NULL,
                                          spatial_decay_rate = NULL) {
  
  n_samples <- nrow(community_matrix)
  
  # Calculate spatial extent if coordinates provided but max_dist not specified
  if (!is.null(coords) && is.null(spatial_max_dist)) {
    # Calculate maximum pairwise distance
    dist_mat <- as.matrix(dist(coords))
    spatial_max_dist <- max(dist_mat)
    cli::cli_alert_info("Calculated maximum spatial distance: {round(spatial_max_dist, 2)}")
  }
  
  # Initialize storage for pairwise components
  R_beta_mat <- matrix(0, n_samples, n_samples)
  E_beta_mat <- matrix(0, n_samples, n_samples)
  P_beta_mat <- matrix(0, n_samples, n_samples)
  S_beta_mat <- matrix(0, n_samples, n_samples)
  
  # Calculate pairwise beta components
  cli::cli_alert_info("Computing {n_samples * (n_samples-1) / 2} pairwise decompositions...")
  
  pb <- progress::progress_bar$new(total = n_samples * (n_samples-1) / 2)
  
  for (i in 1:(n_samples-1)) {
    for (j in (i+1):n_samples) {
      
      # Get coordinates if available
      coords_i <- if (!is.null(coords)) coords[i, ] else NULL
      coords_j <- if (!is.null(coords)) coords[j, ] else NULL
      
      # Decompose beta diversity
      beta_ij <- decompose_beta_diversity(
        community_matrix[i, ],
        community_matrix[j, ],
        tree = tree,
        coords_i = coords_i,
        coords_j = coords_j,
        method = method,
        spatial_max_dist = spatial_max_dist,
        spatial_decay_rate = spatial_decay_rate
      )
      
      # Store in matrices
      R_beta_mat[i, j] <- R_beta_mat[j, i] <- beta_ij$R_beta
      E_beta_mat[i, j] <- E_beta_mat[j, i] <- beta_ij$E_beta  
      P_beta_mat[i, j] <- P_beta_mat[j, i] <- beta_ij$P_beta
      S_beta_mat[i, j] <- S_beta_mat[j, i] <- beta_ij$S_beta
      
      pb$tick()
    }
  }
  
  # Create results object
  results <- list(
    R_beta = R_beta_mat,
    E_beta = E_beta_mat,
    P_beta = P_beta_mat,
    S_beta = S_beta_mat,
    n_samples = n_samples,
    method = method
  )
  
  # Add sample-level summaries based on method
  if (summary_method == "centroid") {
    # Distance to centroid in each component
    results$sample_R_beta <- apply(R_beta_mat, 1, mean)
    results$sample_E_beta <- apply(E_beta_mat, 1, mean)
    results$sample_P_beta <- apply(P_beta_mat, 1, mean)
    results$sample_S_beta <- apply(S_beta_mat, 1, mean)
    
  } else if (summary_method == "pcoa") {
    # PCoA in R-E-P-S space
    # Combine components into single distance matrix
    combined_dist <- (R_beta_mat + E_beta_mat + P_beta_mat + S_beta_mat) / 4
    
    # Perform PCoA
    pcoa_results <- stats::cmdscale(combined_dist, k = min(n_samples - 1, 10))
    results$pcoa_scores <- pcoa_results
    
    # Calculate component contributions to each axis
    results$axis_contributions <- calculate_axis_contributions(
      list(R_beta_mat, E_beta_mat, P_beta_mat, S_beta_mat),
      pcoa_results
    )
  }
  
  class(results) <- c("beta_decomposition_matrix", "list")
  
  return(results)
}

#' Calculate which components contribute to PCoA axes
#' @keywords internal
calculate_axis_contributions <- function(component_matrices, pcoa_scores) {
  
  n_axes <- ncol(pcoa_scores)
  n_components <- length(component_matrices)
  component_names <- c("R_beta", "E_beta", "P_beta", "S_beta")
  
  contributions <- matrix(0, n_components, n_axes)
  rownames(contributions) <- component_names[1:n_components]
  colnames(contributions) <- paste0("PCo", 1:n_axes)
  
  # For each axis, correlate with each component matrix
  for (axis in 1:n_axes) {
    axis_distances <- as.matrix(dist(pcoa_scores[, axis]))
    
    for (comp in 1:n_components) {
      # Mantel-like correlation
      cor_val <- cor(as.vector(component_matrices[[comp]]), 
                     as.vector(axis_distances))
      contributions[comp, axis] <- abs(cor_val)
    }
    
    # Normalize so contributions sum to 1
    contributions[, axis] <- contributions[, axis] / sum(contributions[, axis])
  }
  
  return(contributions)
}

#' Plot Beta Decomposition Results
#'
#' @param x Beta decomposition object
#' @param type Plot type: "heatmap", "pcoa", "components"
#' @param ... Additional plotting arguments
#'
#' @export
plot.beta_decomposition_matrix <- function(x, type = "heatmap", ...) {
  
  if (type == "heatmap") {
    # Create 2x2 heatmap grid
    par(mfrow = c(2, 2))
    
    image(x$R_beta, main = "R_beta: Richness Turnover", 
          col = heat.colors(20), xlab = "Sample", ylab = "Sample")
    
    image(x$E_beta, main = "E_beta: Evenness Turnover",
          col = heat.colors(20), xlab = "Sample", ylab = "Sample")
    
    image(x$P_beta, main = "P_beta: Phylogenetic Turnover", 
          col = heat.colors(20), xlab = "Sample", ylab = "Sample")
    
    image(x$S_beta, main = "S_beta: Spatial Turnover",
          col = heat.colors(20), xlab = "Sample", ylab = "Sample")
    
    par(mfrow = c(1, 1))
    
  } else if (type == "pcoa" && !is.null(x$pcoa_scores)) {
    # Color points by dominant component
    dominant_comp <- apply(
      cbind(x$sample_R_beta, x$sample_E_beta, x$sample_P_beta, x$sample_S_beta),
      1, which.max
    )
    
    comp_colors <- c("red", "blue", "green", "purple")
    
    plot(x$pcoa_scores[, 1:2],
         col = comp_colors[dominant_comp],
         pch = 19,
         xlab = paste0("PCo1 (", round(x$axis_contributions[, 1] * 100), "%)"),
         ylab = paste0("PCo2 (", round(x$axis_contributions[, 2] * 100), "%)"),
         main = "Beta Diversity in R-E-P-S Space")
    
    legend("topright", 
           legend = c("R-dominated", "E-dominated", "P-dominated", "S-dominated"),
           col = comp_colors, pch = 19)
    
  } else if (type == "components") {
    # Stacked bar chart of components
    if (!is.null(x$sample_R_beta)) {
      components <- cbind(x$sample_R_beta, x$sample_E_beta, 
                         x$sample_P_beta, x$sample_S_beta)
      
      # Normalize to percentages
      components_pct <- t(apply(components, 1, function(x) x / sum(x) * 100))
      
      barplot(t(components_pct), 
              col = c("red", "blue", "green", "purple"),
              legend = c("R_beta", "E_beta", "P_beta", "S_beta"),
              ylab = "Percentage Contribution",
              xlab = "Sample",
              main = "Beta Diversity Component Contributions")
    }
  }
}

#' Print method for beta decomposition
#' @export
print.beta_decomposition <- function(x, ...) {
  cat("Beta Diversity Decomposition\n")
  cat("===========================\n")
  cat(sprintf("R_beta (Richness): %.3f\n", x$R_beta))
  cat(sprintf("E_beta (Evenness): %.3f\n", x$E_beta))
  cat(sprintf("P_beta (Phylogenetic): %.3f\n", x$P_beta))
  cat(sprintf("S_beta (Spatial): %.3f\n", x$S_beta))
  cat(sprintf("\nTotal Beta: %.3f\n", x$total_beta))
  cat(sprintf("Dominant Component: %s\n", x$dominant_component))
}

#' Validate Beta Decomposition Against Traditional Metrics
#'
#' @param community_matrix Community abundance matrix
#' @param tree Optional phylogenetic tree
#' @param coords Optional spatial coordinates
#'
#' @return Validation results comparing decomposition to traditional metrics
#' @export
validate_beta_decomposition <- function(community_matrix, tree = NULL, coords = NULL) {
  
  cli::cli_alert_info("Validating beta decomposition framework...")
  
  # Calculate traditional beta diversity metrics
  trad_metrics <- list(
    jaccard = vegan::vegdist(community_matrix, method = "jaccard"),
    bray_curtis = vegan::vegdist(community_matrix, method = "bray"),
    sorensen = vegan::vegdist(community_matrix, method = "bray", binary = TRUE)
  )
  
  # Add phylogenetic metrics if tree provided
  if (!is.null(tree) && requireNamespace("picante", quietly = TRUE)) {
    trad_metrics$unifrac <- picante::unifrac(community_matrix, tree)
  }
  
  # Decompose using our framework
  decomp_results <- decompose_beta_diversity_matrix(
    community_matrix, tree, coords, 
    method = "information"
  )
  
  # Test reconstructions
  validation_results <- list()
  
  # Test 1: Can Jaccard be approximated by R_beta?
  validation_results$jaccard_vs_R <- cor(
    as.vector(trad_metrics$jaccard),
    as.vector(decomp_results$R_beta)
  )
  
  # Test 2: Can Bray-Curtis be approximated by R_beta + E_beta?
  combined_RE <- (decomp_results$R_beta + decomp_results$E_beta) / 2
  validation_results$bray_vs_RE <- cor(
    as.vector(trad_metrics$bray_curtis),
    as.vector(combined_RE)
  )
  
  # Test 3: Component orthogonality
  components_vec <- list(
    as.vector(decomp_results$R_beta),
    as.vector(decomp_results$E_beta),
    as.vector(decomp_results$P_beta),
    as.vector(decomp_results$S_beta)
  )
  
  orthogonality_matrix <- matrix(0, 4, 4)
  for (i in 1:4) {
    for (j in 1:4) {
      orthogonality_matrix[i, j] <- cor(components_vec[[i]], components_vec[[j]])
    }
  }
  
  validation_results$orthogonality <- orthogonality_matrix
  validation_results$mean_off_diagonal_cor <- mean(abs(
    orthogonality_matrix[upper.tri(orthogonality_matrix)]
  ))
  
  # Test 4: Variance explained
  total_var <- var(as.vector(trad_metrics$bray_curtis))
  explained_var <- var(as.vector(combined_RE))
  validation_results$variance_explained <- explained_var / total_var
  
  class(validation_results) <- c("beta_validation", "list")
  
  return(validation_results)
}

#' Example Ecological Application: Test Assembly Mechanisms
#'
#' @param community_matrix Community matrix
#' @param env_data Environmental data
#' @param tree Phylogenetic tree
#' @param coords Spatial coordinates
#'
#' @export
test_beta_assembly_mechanisms <- function(community_matrix, env_data, 
                                         tree = NULL, coords = NULL) {
  
  # Decompose beta diversity
  beta_decomp <- decompose_beta_diversity_matrix(
    community_matrix, tree, coords
  )
  
  # Test which components correlate with environmental gradients
  results <- list()
  
  # For each environmental variable
  for (env_var in names(env_data)) {
    env_dist <- dist(env_data[[env_var]])
    
    # Correlate with each beta component
    results[[env_var]] <- list(
      R_correlation = cor(as.vector(env_dist), as.vector(beta_decomp$R_beta)),
      E_correlation = cor(as.vector(env_dist), as.vector(beta_decomp$E_beta)),
      P_correlation = cor(as.vector(env_dist), as.vector(beta_decomp$P_beta)),
      S_correlation = cor(as.vector(env_dist), as.vector(beta_decomp$S_beta))
    )
  }
  
  # Identify dominant assembly mechanism
  # Environmental filtering -> R_beta responds to environment
  # Competitive exclusion -> E_beta responds to environment  
  # Dispersal limitation -> S_beta dominates
  
  return(results)
}