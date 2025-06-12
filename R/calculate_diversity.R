#' Calculate multiple diversity metrics
#'
#' @description
#' Calculates a comprehensive suite of alpha diversity metrics from a phyloseq object.
#' This is the core function that computes all requested diversity measures in a
#' consistent format, preparing them for consensus analysis and interpretation.
#'
#' @param physeq A phyloseq object containing the microbiome data
#' @param metrics Character vector of diversity metrics to calculate. 
#'   Options include: "shannon", "simpson", "invsimpson", "chao1", "observed", 
#'   "faith_pd" (requires phylogenetic tree)
#' @param groups Optional character string specifying the sample variable to use for grouping
#'
#' @return A data frame with samples as rows and diversity metrics as columns,
#'   with an additional group column if specified
#'
#' @export
#' @examples
#' \dontrun{
#' library(phyloseq)
#' data(GlobalPatterns)
#' 
#' # Calculate basic diversity metrics
#' div_results <- calculate_diversity(GlobalPatterns)
#' head(div_results)
#' 
#' # Calculate specific metrics with grouping
#' div_results <- calculate_diversity(
#'   GlobalPatterns,
#'   metrics = c("shannon", "simpson", "chao1"),
#'   groups = "SampleType"
#' )
#' }
calculate_diversity <- function(physeq, 
                              metrics = c("shannon", "simpson", "chao1", "observed"),
                              groups = NULL) {
  
  # Input validation
  if (!inherits(physeq, "phyloseq")) {
    cli::cli_abort("Input must be a phyloseq object")
  }
  
  # Check available metrics
  available_metrics <- c("shannon", "simpson", "invsimpson", "chao1", "observed", "faith_pd")
  invalid_metrics <- setdiff(metrics, available_metrics)
  
  if (length(invalid_metrics) > 0) {
    cli::cli_warn("Unknown metrics will be ignored: {invalid_metrics}")
    metrics <- intersect(metrics, available_metrics)
  }
  
  # Check if tree is needed
  if ("faith_pd" %in% metrics && is.null(phyloseq::phy_tree(physeq, errorIfNULL = FALSE))) {
    cli::cli_warn("Phylogenetic tree not found. Removing faith_pd from metrics.")
    metrics <- setdiff(metrics, "faith_pd")
  }
  
  # Initialize results data frame
  results <- data.frame(
    sample = phyloseq::sample_names(physeq),
    stringsAsFactors = FALSE
  )
  
  # Calculate each metric
  cli::cli_progress_bar("Calculating diversity metrics", total = length(metrics))
  
  for (metric in metrics) {
    cli::cli_progress_update()
    
    if (metric %in% c("shannon", "simpson", "invsimpson")) {
      # Use vegan for these metrics
      # Get OTU table in correct orientation (samples as rows)
      otu_mat <- as.matrix(phyloseq::otu_table(physeq))
      if (phyloseq::taxa_are_rows(physeq)) {
        otu_mat <- t(otu_mat)
      }
      values <- vegan::diversity(otu_mat, index = metric)
      
    } else if (metric == "chao1") {
      # Use vegan's estimateR for Chao1
      otu_mat <- as.matrix(phyloseq::otu_table(physeq))
      if (phyloseq::taxa_are_rows(physeq)) {
        otu_mat <- t(otu_mat)
      }
      values <- vegan::estimateR(otu_mat)[1, ]
      
    } else if (metric == "observed") {
      # Count observed OTUs
      values <- phyloseq::estimate_richness(physeq, measures = "Observed")[, 1]
      
    } else if (metric == "faith_pd") {
      # Calculate Faith's phylogenetic diversity
      otu_mat <- as.matrix(phyloseq::otu_table(physeq))
      if (phyloseq::taxa_are_rows(physeq)) {
        otu_mat <- t(otu_mat)
      }
      
      # Check if picante is available
      if (requireNamespace("picante", quietly = TRUE)) {
        # Use picante::pd for proper Faith's PD calculation
        tree <- phyloseq::phy_tree(physeq)
        pd_result <- picante::pd(otu_mat, tree, include.root = FALSE)
        values <- pd_result$PD
      } else {
        # If picante not available, use dependency check function
        check_function_dependencies("faith_pd")
        values <- rep(NA, nrow(otu_mat))
      }
    }
    
    results[[metric]] <- values
  }
  
  cli::cli_progress_done()
  
  # Add group information if requested
  if (!is.null(groups)) {
    if (groups %in% phyloseq::sample_variables(physeq)) {
      sample_data <- phyloseq::sample_data(physeq)
      # Use the original column name, not lowercase
      results[[groups]] <- as.character(sample_data[[groups]])
    } else {
      cli::cli_warn("Group variable '{groups}' not found in sample data")
    }
  }
  
  # Add metadata as attributes
  attr(results, "metrics") <- metrics
  attr(results, "n_samples") <- phyloseq::nsamples(physeq)
  attr(results, "n_taxa") <- phyloseq::ntaxa(physeq)
  class(results) <- c("diversity_results", "data.frame")
  
  return(results)
}

#' Print method for diversity results
#'
#' @param x A diversity_results object
#' @param ... Additional arguments (unused)
#' @export
print.diversity_results <- function(x, ...) {
  cli::cli_h1("Diversity Analysis Results")
  cli::cli_text("Samples: {attr(x, 'n_samples')}")
  cli::cli_text("Taxa: {attr(x, 'n_taxa')}")
  cli::cli_text("Metrics: {paste(attr(x, 'metrics'), collapse = ', ')}")
  cli::cli_text("")
  
  # Print first few rows
  print(head(as.data.frame(x)), ...)
  
  if (nrow(x) > 6) {
    cli::cli_text("... with {nrow(x) - 6} more rows")
  }
}