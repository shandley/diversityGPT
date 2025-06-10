#' Taxa Indicator Analysis for Information Components
#'
#' @description Functions to identify which taxa are indicators for the R, E, P, and S information components
#' @name taxa_indicators
#' @keywords taxa analysis
NULL

#' Identify Taxa Indicators for Information Components
#'
#' Identifies which taxa serve as indicators for each information component (R, E, P, S)
#' in diversity patterns.
#'
#' @param physeq A phyloseq object
#' @param components Universal information components (output from extract_universal_information)
#' @param top_n Number of top indicator taxa to identify per component (default: 10)
#' @param method Method for indicator identification: "contribution", "variance", or "correlation"
#' @param groups Optional grouping variable from sample_data
#' @param normalize Normalize contributions to sum to 1 (default: TRUE)
#' @param verbose Print progress messages
#'
#' @return A taxa_indicators object containing:
#'   \item{richness_indicators}{Taxa indicating richness patterns}
#'   \item{evenness_indicators}{Taxa indicating evenness patterns}
#'   \item{phylogenetic_indicators}{Taxa indicating phylogenetic patterns}
#'   \item{spatial_indicators}{Taxa indicating spatial patterns}
#'   \item{summary}{Summary statistics for each component}
#'   \item{method}{Method used for identification}
#'
#' @export
#' @examples
#' # Create demo data
#' demo_data <- create_demo_phyloseq()
#' 
#' # Extract universal information
#' info <- extract_universal_information(demo_data)
#' 
#' # Identify taxa indicators
#' indicators <- identify_taxa_indicators(demo_data, info)
#' 
#' # View top richness indicators
#' print(indicators$richness_indicators)
identify_taxa_indicators <- function(physeq, 
                                 components = NULL,
                                 top_n = 10,
                                 method = c("contribution", "variance", "correlation"),
                                 groups = NULL,
                                 normalize = TRUE,
                                 verbose = TRUE) {
  
  method <- match.arg(method)
  
  # Extract components if not provided
  if (is.null(components)) {
    if (verbose) message("Extracting universal information components...")
    components <- extract_universal_information(physeq)
  }
  
  # Get OTU table
  otu_mat <- as.matrix(phyloseq::otu_table(physeq))
  if (!phyloseq::taxa_are_rows(physeq)) {
    otu_mat <- t(otu_mat)
  }
  
  # Initialize results
  results <- list(
    richness_indicators = NULL,
    evenness_indicators = NULL,
    phylogenetic_indicators = NULL,
    spatial_indicators = NULL,
    summary = list(),
    method = method,
    call = match.call()
  )
  
  if (verbose) message("Identifying richness indicators...")
  results$richness_indicators <- identify_richness_indicators(
    otu_mat, components, top_n, method, groups, normalize
  )
  
  if (verbose) message("Identifying evenness indicators...")
  results$evenness_indicators <- identify_evenness_indicators(
    otu_mat, components, top_n, method, groups, normalize
  )
  
  if (verbose) message("Identifying phylogenetic indicators...")
  results$phylogenetic_indicators <- identify_phylogenetic_indicators(
    physeq, components, top_n, method, groups, normalize
  )
  
  if (verbose) message("Identifying spatial indicators...")
  results$spatial_indicators <- identify_spatial_indicators(
    physeq, components, top_n, method, groups, normalize
  )
  
  # Calculate summary statistics
  results$summary <- summarize_taxa_indicators(results)
  
  class(results) <- c("taxa_indicators", "list")
  return(results)
}

#' Identify Richness Indicators
#'
#' Identifies taxa that serve as indicators for richness patterns
#'
#' @param otu_mat OTU matrix (taxa as rows)
#' @param components Universal information components
#' @param top_n Number of top indicators to return
#' @param method Indicator identification method
#' @param groups Optional grouping variable
#' @param normalize Normalize contributions
#'
#' @return Data frame of richness indicator taxa
#' @keywords internal
identify_richness_indicators <- function(otu_mat, components, top_n, 
                                    method, groups, normalize) {
  
  n_taxa <- nrow(otu_mat)
  n_samples <- ncol(otu_mat)
  
  if (method == "contribution") {
    # Calculate each taxon's contribution to total richness
    # Richness contribution = presence frequency × abundance when present
    presence_freq <- rowSums(otu_mat > 0) / n_samples
    mean_abundance_when_present <- apply(otu_mat, 1, function(x) {
      present <- x > 0
      if (sum(present) > 0) mean(x[present]) else 0
    })
    
    contributions <- presence_freq * log1p(mean_abundance_when_present)
    
  } else if (method == "variance") {
    # Taxa with high variance in presence/absence drive richness changes
    presence_mat <- otu_mat > 0
    contributions <- apply(presence_mat, 1, var)
    
  } else if (method == "correlation") {
    # Correlate each taxon's presence with total richness
    sample_richness <- colSums(otu_mat > 0)
    contributions <- apply(otu_mat > 0, 1, function(x) {
      cor(x, sample_richness, method = "spearman")
    })
    contributions[is.na(contributions)] <- 0
  }
  
  # Normalize if requested
  if (normalize && sum(abs(contributions)) > 0) {
    contributions <- abs(contributions) / sum(abs(contributions))
  }
  
  # Get top contributors
  top_indices <- order(contributions, decreasing = TRUE)[1:min(top_n, n_taxa)]
  
  # Create results data frame
  results <- data.frame(
    taxon = rownames(otu_mat)[top_indices],
    contribution = contributions[top_indices],
    rank = 1:length(top_indices),
    component = "richness",
    stringsAsFactors = FALSE
  )
  
  # Add additional metrics
  results$presence_frequency <- rowSums(otu_mat[top_indices, , drop = FALSE] > 0) / n_samples
  results$mean_abundance <- rowMeans(otu_mat[top_indices, , drop = FALSE])
  
  return(results)
}

#' Identify Evenness Indicators
#'
#' Identifies taxa that serve as indicators for evenness patterns
#'
#' @param otu_mat OTU matrix (taxa as rows)
#' @param components Universal information components
#' @param top_n Number of top indicators to return
#' @param method Indicator identification method
#' @param groups Optional grouping variable
#' @param normalize Normalize contributions
#'
#' @return Data frame of evenness indicator taxa
#' @keywords internal
identify_evenness_indicators <- function(otu_mat, components, top_n,
                                    method, groups, normalize) {
  
  n_taxa <- nrow(otu_mat)
  n_samples <- ncol(otu_mat)
  
  if (method == "contribution") {
    # Taxa that dominate (high relative abundance) reduce evenness
    # Calculate dominance score for each taxon
    rel_abundance <- sweep(otu_mat, 2, colSums(otu_mat), "/")
    dominance_scores <- apply(rel_abundance, 1, function(x) {
      # High mean and high variance = strong evenness indicator
      mean(x) * sd(x)
    })
    contributions <- dominance_scores
    
  } else if (method == "variance") {
    # Taxa with high variance in relative abundance affect evenness
    rel_abundance <- sweep(otu_mat, 2, colSums(otu_mat), "/")
    contributions <- apply(rel_abundance, 1, var)
    
  } else if (method == "correlation") {
    # Correlate each taxon's relative abundance with Shannon evenness
    rel_abundance <- sweep(otu_mat, 2, colSums(otu_mat), "/")
    shannon_evenness <- vegan::diversity(t(otu_mat), index = "shannon") / 
                      log(colSums(otu_mat > 0))
    
    contributions <- apply(rel_abundance, 1, function(x) {
      # Negative correlation means taxon reduces evenness when dominant
      -cor(x, shannon_evenness, method = "spearman", use = "complete.obs")
    })
    contributions[is.na(contributions)] <- 0
  }
  
  # Normalize if requested
  if (normalize && sum(abs(contributions)) > 0) {
    contributions <- abs(contributions) / sum(abs(contributions))
  }
  
  # Get top contributors
  top_indices <- order(contributions, decreasing = TRUE)[1:min(top_n, n_taxa)]
  
  # Create results data frame
  results <- data.frame(
    taxon = rownames(otu_mat)[top_indices],
    contribution = contributions[top_indices],
    rank = 1:length(top_indices),
    component = "evenness",
    stringsAsFactors = FALSE
  )
  
  # Add additional metrics
  rel_abundance <- sweep(otu_mat, 2, colSums(otu_mat), "/")
  results$mean_relative_abundance <- rowMeans(rel_abundance[top_indices, , drop = FALSE])
  results$cv_relative_abundance <- apply(rel_abundance[top_indices, , drop = FALSE], 1, 
                                        function(x) sd(x) / mean(x))
  
  return(results)
}

#' Identify Phylogenetic Indicators
#'
#' Identifies taxa that serve as indicators for phylogenetic diversity patterns
#'
#' @param physeq A phyloseq object
#' @param components Universal information components
#' @param top_n Number of top indicators to return
#' @param method Indicator identification method
#' @param groups Optional grouping variable
#' @param normalize Normalize contributions
#'
#' @return Data frame of phylogenetic indicator taxa
#' @keywords internal
identify_phylogenetic_indicators <- function(physeq, components, top_n,
                                        method, groups, normalize) {
  
  otu_mat <- as.matrix(phyloseq::otu_table(physeq))
  if (!phyloseq::taxa_are_rows(physeq)) {
    otu_mat <- t(otu_mat)
  }
  
  n_taxa <- nrow(otu_mat)
  
  # Check if phylogenetic tree exists
  if (is.null(phyloseq::phy_tree(physeq, errorIfNULL = FALSE))) {
    # No tree - use taxonomic distance as proxy
    contributions <- calculate_taxonomic_uniqueness(physeq)
  } else {
    tree <- phyloseq::phy_tree(physeq)
    
    if (method == "contribution") {
      # Calculate phylogenetic uniqueness (branch length contribution)
      # Taxa on long branches contribute more to PD
      contributions <- calculate_phylogenetic_uniqueness(tree, rownames(otu_mat))
      
    } else if (method == "variance") {
      # Taxa with variable presence affect PD variability
      presence_mat <- otu_mat > 0
      presence_var <- apply(presence_mat, 1, var)
      
      # Weight by phylogenetic uniqueness
      uniqueness <- calculate_phylogenetic_uniqueness(tree, rownames(otu_mat))
      contributions <- presence_var * uniqueness
      
    } else if (method == "correlation") {
      # Correlate taxon presence with Faith's PD
      # Calculate PD for each sample
      sample_pd <- apply(otu_mat > 0, 2, function(x) {
        present_taxa <- rownames(otu_mat)[x]
        if (length(present_taxa) > 1) {
          subtree <- ape::keep.tip(tree, present_taxa)
          sum(subtree$edge.length)
        } else {
          0
        }
      })
      
      contributions <- apply(otu_mat > 0, 1, function(x) {
        cor(x, sample_pd, method = "spearman")
      })
      contributions[is.na(contributions)] <- 0
    }
  }
  
  # Normalize if requested
  if (normalize && sum(abs(contributions)) > 0) {
    contributions <- abs(contributions) / sum(abs(contributions))
  }
  
  # Get top contributors
  top_indices <- order(contributions, decreasing = TRUE)[1:min(top_n, n_taxa)]
  
  # Create results data frame
  results <- data.frame(
    taxon = rownames(otu_mat)[top_indices],
    contribution = contributions[top_indices],
    rank = 1:length(top_indices),
    component = "phylogenetic",
    stringsAsFactors = FALSE
  )
  
  # Add phylogenetic metrics if tree exists
  if (!is.null(phyloseq::phy_tree(physeq, errorIfNULL = FALSE))) {
    results$phylogenetic_uniqueness <- contributions[top_indices]
  }
  
  return(results)
}

#' Identify Spatial Indicators  
#'
#' Identifies taxa that serve as indicators for spatial patterns
#'
#' @param physeq A phyloseq object
#' @param components Universal information components
#' @param top_n Number of top indicators to return
#' @param method Indicator identification method
#' @param groups Optional grouping variable
#' @param normalize Normalize contributions
#'
#' @return Data frame of spatial indicator taxa
#' @keywords internal
identify_spatial_indicators <- function(physeq, components, top_n,
                                   method, groups, normalize) {
  
  otu_mat <- as.matrix(phyloseq::otu_table(physeq))
  if (!phyloseq::taxa_are_rows(physeq)) {
    otu_mat <- t(otu_mat)
  }
  
  n_taxa <- nrow(otu_mat)
  n_samples <- ncol(otu_mat)
  
  # Check for spatial/environmental gradients in sample data
  sample_data <- phyloseq::sample_data(physeq)
  
  if (method == "contribution") {
    # Taxa with patchy distributions indicate spatial patterns
    # Calculate spatial heterogeneity (Moran's I or dispersion index)
    contributions <- apply(otu_mat, 1, function(x) {
      # Dispersion index: variance/mean ratio
      # High values indicate spatial clustering
      if (mean(x) > 0) {
        var(x) / mean(x)
      } else {
        0
      }
    })
    
  } else if (method == "variance") {
    # Spatial variance in abundance
    if (!is.null(groups) && groups %in% names(sample_data)) {
      # Calculate among-group variance
      group_var <- sapply(1:n_taxa, function(i) {
        abundance <- otu_mat[i, ]
        group_factor <- sample_data[[groups]]
        if (length(unique(group_factor)) > 1) {
          summary(aov(abundance ~ group_factor))[[1]][1, "Mean Sq"]
        } else {
          var(abundance)
        }
      })
      contributions <- group_var
    } else {
      # Simple variance
      contributions <- apply(otu_mat, 1, var)
    }
    
  } else if (method == "correlation") {
    # Correlate with beta diversity
    # Taxa indicating beta diversity have patchy distributions
    dist_mat <- vegan::vegdist(t(otu_mat), method = "bray")
    
    contributions <- apply(otu_mat, 1, function(x) {
      # Create distance matrix for this taxon
      taxon_dist <- as.matrix(dist(x))
      # Correlation with overall community distance
      cor(as.vector(taxon_dist), as.vector(as.matrix(dist_mat)), 
          method = "spearman")
    })
    contributions[is.na(contributions)] <- 0
  }
  
  # Normalize if requested
  if (normalize && sum(abs(contributions)) > 0) {
    contributions <- abs(contributions) / sum(abs(contributions))
  }
  
  # Get top contributors
  top_indices <- order(contributions, decreasing = TRUE)[1:min(top_n, n_taxa)]
  
  # Create results data frame
  results <- data.frame(
    taxon = rownames(otu_mat)[top_indices],
    contribution = contributions[top_indices],
    rank = 1:length(top_indices),
    component = "spatial",
    stringsAsFactors = FALSE
  )
  
  # Add spatial metrics
  results$dispersion_index <- apply(otu_mat[top_indices, , drop = FALSE], 1, 
                                   function(x) {
                                     if (mean(x) > 0) var(x) / mean(x) else 0
                                   })
  results$occupancy <- rowSums(otu_mat[top_indices, , drop = FALSE] > 0) / n_samples
  
  return(results)
}

#' Calculate Phylogenetic Uniqueness
#'
#' Calculates how unique each taxon is based on phylogenetic position
#'
#' @param tree Phylogenetic tree
#' @param taxa Taxa names
#'
#' @return Numeric vector of uniqueness scores
#' @keywords internal
calculate_phylogenetic_uniqueness <- function(tree, taxa) {
  # Store original taxa order
  original_taxa <- taxa
  
  # Ensure taxa match tree tips
  taxa_in_tree <- intersect(taxa, tree$tip.label)
  
  if (length(taxa_in_tree) == 0) {
    # No taxa in tree - return uniform scores
    uniqueness <- rep(1, length(original_taxa))
    names(uniqueness) <- original_taxa
    return(uniqueness)
  }
  
  # Calculate uniqueness for taxa in tree
  uniqueness_in_tree <- numeric(length(taxa_in_tree))
  names(uniqueness_in_tree) <- taxa_in_tree
  
  if (length(taxa_in_tree) > 1) {
    # Get phylogenetic distance matrix
    dist_mat <- ape::cophenetic.phylo(tree)
    
    for (i in seq_along(taxa_in_tree)) {
      # Calculate mean pairwise phylogenetic distance
      other_taxa <- taxa_in_tree[-i]
      dists <- dist_mat[taxa_in_tree[i], other_taxa]
      uniqueness_in_tree[taxa_in_tree[i]] <- mean(dists)
    }
  } else {
    uniqueness_in_tree[taxa_in_tree] <- 1
  }
  
  # Create full result vector matching original taxa
  uniqueness <- numeric(length(original_taxa))
  names(uniqueness) <- original_taxa
  
  # Fill in values for taxa in tree
  uniqueness[taxa_in_tree] <- uniqueness_in_tree[taxa_in_tree]
  
  # Taxa not in tree get average uniqueness
  if (length(taxa_in_tree) < length(original_taxa)) {
    mean_uniqueness <- mean(uniqueness_in_tree)
    uniqueness[!original_taxa %in% taxa_in_tree] <- mean_uniqueness
  }
  
  return(uniqueness)
}

#' Calculate Taxonomic Uniqueness
#'
#' Calculates taxonomic uniqueness when no phylogenetic tree is available
#'
#' @param physeq A phyloseq object
#'
#' @return Numeric vector of uniqueness scores
#' @keywords internal
calculate_taxonomic_uniqueness <- function(physeq) {
  # Get OTU/taxa names
  otu_mat <- as.matrix(phyloseq::otu_table(physeq))
  if (!phyloseq::taxa_are_rows(physeq)) {
    otu_mat <- t(otu_mat)
  }
  taxa_names <- rownames(otu_mat)
  
  tax_table <- phyloseq::tax_table(physeq)
  
  if (is.null(tax_table)) {
    # No taxonomy - return uniform scores with names
    uniqueness <- rep(1, length(taxa_names))
    names(uniqueness) <- taxa_names
    return(uniqueness)
  }
  
  # Calculate uniqueness based on taxonomic rank sharing
  tax_mat <- as.matrix(tax_table)
  n_taxa <- nrow(tax_mat)
  uniqueness <- numeric(n_taxa)
  
  # Handle case where tax_mat might be character or factor
  if (ncol(tax_mat) > 0) {
    for (i in 1:n_taxa) {
      # Count how many taxa share each taxonomic level
      shared_counts <- sapply(1:ncol(tax_mat), function(rank) {
        # Convert to character for safe comparison
        this_taxon <- as.character(tax_mat[i, rank])
        all_taxa <- as.character(tax_mat[, rank])
        
        # Count matches, handling NAs
        if (is.na(this_taxon)) {
          sum(is.na(all_taxa))
        } else {
          sum(all_taxa == this_taxon, na.rm = TRUE)
        }
      })
      
      # Avoid division by zero
      shared_counts[shared_counts == 0] <- 1
      
      # Weight by rank (higher ranks = more weight)
      weights <- rev(seq_len(ncol(tax_mat)))
      uniqueness[i] <- sum(weights / shared_counts) / sum(weights)
    }
  } else {
    # No taxonomy columns - uniform uniqueness
    uniqueness <- rep(1, n_taxa)
  }
  
  names(uniqueness) <- rownames(tax_mat)
  return(uniqueness)
}

#' Summarize Taxa Indicators
#'
#' Creates summary statistics for taxa indicator analysis
#'
#' @param indicators Taxa indicators results
#'
#' @return List of summary statistics
#' @keywords internal
summarize_taxa_indicators <- function(indicators) {
  summary_stats <- list()
  
  # Summarize each component
  for (component in c("richness", "evenness", "phylogenetic", "spatial")) {
    comp_name <- paste0(component, "_indicators")
    if (!is.null(indicators[[comp_name]])) {
      indicator_df <- indicators[[comp_name]]
      
      summary_stats[[component]] <- list(
        n_indicators = nrow(indicator_df),
        total_contribution = sum(indicator_df$contribution),
        top_taxon = indicator_df$taxon[1],
        top_contribution = indicator_df$contribution[1],
        mean_contribution = mean(indicator_df$contribution),
        sd_contribution = sd(indicator_df$contribution)
      )
    }
  }
  
  # Overall statistics
  # Extract only common columns to avoid rbind errors
  common_cols <- c("taxon", "contribution", "rank", "component")
  
  indicator_list <- list()
  if (!is.null(indicators$richness_indicators)) {
    indicator_list$richness <- indicators$richness_indicators[, common_cols]
  }
  if (!is.null(indicators$evenness_indicators)) {
    indicator_list$evenness <- indicators$evenness_indicators[, common_cols]
  }
  if (!is.null(indicators$phylogenetic_indicators)) {
    indicator_list$phylogenetic <- indicators$phylogenetic_indicators[, common_cols]
  }
  if (!is.null(indicators$spatial_indicators)) {
    indicator_list$spatial <- indicators$spatial_indicators[, common_cols]
  }
  
  all_indicators <- do.call(rbind, indicator_list)
  
  if (!is.null(all_indicators)) {
    # Find taxa that indicate multiple components
    taxa_counts <- table(all_indicators$taxon)
    multi_indicators <- names(taxa_counts)[taxa_counts > 1]
    
    summary_stats$overall <- list(
      total_unique_indicators = length(unique(all_indicators$taxon)),
      multi_component_indicators = multi_indicators,
      n_multi_component = length(multi_indicators),
      method = indicators$method
    )
  }
  
  return(summary_stats)
}

#' Print Method for Taxa Indicators
#'
#' @param x A taxa_indicators object
#' @param ... Additional arguments (ignored)
#'
#' @export
print.taxa_indicators <- function(x, ...) {
  cat("Taxa Indicator Analysis Results\n")
  cat("Method:", x$method, "\n")
  cat("===========================\n\n")
  
  # Print top indicators for each component
  components <- c("richness", "evenness", "phylogenetic", "spatial")
  
  for (comp in components) {
    comp_name <- paste0(comp, "_indicators")
    if (!is.null(x[[comp_name]]) && nrow(x[[comp_name]]) > 0) {
      cat(toupper(comp), "INDICATORS:\n")
      
      # Show top 5
      top_5 <- head(x[[comp_name]], 5)
      for (i in 1:nrow(top_5)) {
        cat(sprintf("  %d. %s (contribution: %.3f)\n",
                   top_5$rank[i],
                   top_5$taxon[i],
                   top_5$contribution[i]))
      }
      cat("\n")
    }
  }
  
  # Print summary
  if (!is.null(x$summary$overall)) {
    cat("SUMMARY:\n")
    cat("  Total unique indicator taxa:", x$summary$overall$total_unique_indicators, "\n")
    cat("  Multi-component indicators:", x$summary$overall$n_multi_component, "\n")
    
    if (x$summary$overall$n_multi_component > 0) {
      cat("  Taxa indicating multiple components:\n")
      for (taxon in head(x$summary$overall$multi_component_indicators, 5)) {
        cat("    -", taxon, "\n")
      }
    }
  }
  
  invisible(x)
}

#' Plot Taxa Indicators
#'
#' Creates visualizations of taxa indicator analysis results
#'
#' @param x A taxa_indicators object
#' @param type Plot type: "bar", "network", "heatmap", or "contribution"
#' @param top_n Number of top taxa to show per component
#' @param interactive Create interactive plot using plotly
#' @param ... Additional arguments passed to plotting functions
#'
#' @return A ggplot2 or plotly object
#' @export
plot.taxa_indicators <- function(x, 
                             type = c("bar", "network", "heatmap", "contribution"),
                             top_n = 10,
                             interactive = FALSE,
                             ...) {
  
  type <- match.arg(type)
  
  if (type == "bar") {
    p <- plot_indicator_bars(x, top_n, interactive)
  } else if (type == "network") {
    p <- plot_indicator_network(x, top_n, interactive)
  } else if (type == "heatmap") {
    p <- plot_indicator_heatmap(x, top_n, interactive)
  } else if (type == "contribution") {
    p <- plot_indicator_contributions(x, top_n, interactive)
  }
  
  return(p)
}

#' Plot Indicator Bars
#'
#' @param indicators Taxa indicators object
#' @param top_n Number of top taxa
#' @param interactive Use plotly
#'
#' @return Plot object
#' @keywords internal
plot_indicator_bars <- function(indicators, top_n, interactive) {
  # Combine all indicator data - only use common columns
  common_cols <- c("taxon", "contribution", "rank", "component")
  
  indicator_list <- list()
  if (!is.null(indicators$richness_indicators) && nrow(indicators$richness_indicators) > 0) {
    indicator_list$richness <- head(indicators$richness_indicators[, common_cols], top_n)
  }
  if (!is.null(indicators$evenness_indicators) && nrow(indicators$evenness_indicators) > 0) {
    indicator_list$evenness <- head(indicators$evenness_indicators[, common_cols], top_n)
  }
  if (!is.null(indicators$phylogenetic_indicators) && nrow(indicators$phylogenetic_indicators) > 0) {
    indicator_list$phylogenetic <- head(indicators$phylogenetic_indicators[, common_cols], top_n)
  }
  if (!is.null(indicators$spatial_indicators) && nrow(indicators$spatial_indicators) > 0) {
    indicator_list$spatial <- head(indicators$spatial_indicators[, common_cols], top_n)
  }
  
  all_indicators <- do.call(rbind, indicator_list)
  
  if (is.null(all_indicators) || nrow(all_indicators) == 0) {
    stop("No indicator data to plot")
  }
  
  # Create bar plot
  p <- ggplot2::ggplot(all_indicators, 
                      ggplot2::aes(x = stats::reorder(taxon, contribution),
                                  y = contribution,
                                  fill = component)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::facet_wrap(~ component, scales = "free_y", ncol = 2) +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "Top Taxa Indicators for Information Components",
                 x = "Taxon",
                 y = "Contribution",
                 fill = "Component") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 8),
                  legend.position = "none")
  
  if (interactive) {
    p <- plotly::ggplotly(p)
  }
  
  return(p)
}