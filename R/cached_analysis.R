#' Cached Universal Analysis Functions
#'
#' High-performance cached versions of expensive universal analysis operations
#' with intelligent caching and progress tracking
#'

#' Cached universal information extraction
#'
#' Extract universal information components with intelligent caching
#'
#' @param physeq phyloseq object
#' @param groups Optional grouping variable
#' @param include_phylogenetic Include phylogenetic diversity (default: auto-detect)
#' @param use_cache Whether to use caching (default: TRUE)
#' @param force_recompute Force recomputation even if cached (default: FALSE)
#' @param show_progress Show progress bar (default: TRUE)
#' @param cache_metadata Optional metadata for cached results
#'
#' @return universal_information object
#' @export
#' @examples
#' \dontrun{
#' # Cached universal analysis
#' universal_info <- cached_extract_universal_information(phyloseq_obj)
#' 
#' # Force recomputation
#' universal_info <- cached_extract_universal_information(
#'   phyloseq_obj, 
#'   force_recompute = TRUE
#' )
#' }
cached_extract_universal_information <- function(physeq,
                                                groups = NULL,
                                                include_phylogenetic = NULL,
                                                use_cache = TRUE,
                                                force_recompute = FALSE,
                                                show_progress = TRUE,
                                                cache_metadata = NULL) {
  
  # Load required functions
  if (!exists("extract_universal_information")) {
    source("R/universal_information.R")
  }
  
  # Load cache and progress systems
  load_cache_system()
  load_progress_system()
  
  # Auto-detect phylogenetic capability
  if (is.null(include_phylogenetic)) {
    include_phylogenetic <- !is.null(phy_tree(physeq, errorIfNULL = FALSE))
  }
  
  # Generate cache key based on data characteristics
  cache_key <- generate_universal_cache_key(physeq, groups, include_phylogenetic)
  
  # Check cache first
  if (use_cache && !force_recompute && exists("cache_get")) {
    cached_result <- cache_get(cache_key)
    if (!is.null(cached_result)) {
      if (show_progress) {
        message("Loaded universal information from cache")
      }
      return(cached_result)
    }
  }
  
  # Compute universal information with progress tracking
  if (show_progress) {
    pb <- create_progress_tracker(5, "Computing Universal Information")
    
    tryCatch({
      # Step 1: Data preparation
      update_progress(pb, 1, "Preparing data")
      n_samples <- nsamples(physeq)
      n_taxa <- ntaxa(physeq)
      
      # Step 2: Diversity calculation
      update_progress(pb, 2, "Calculating diversity metrics")
      result <- extract_universal_information(
        physeq,
        groups = groups,
        include_phylogenetic = include_phylogenetic
      )
      
      # Step 3: Information decomposition
      update_progress(pb, 3, "Decomposing information components")
      # (This step is handled internally by extract_universal_information)
      
      # Step 4: Quality assessment
      update_progress(pb, 4, "Assessing transformation quality")
      # (This step is handled internally by extract_universal_information)
      
      # Step 5: Finalization
      update_progress(pb, 5, "Finalizing results")
      
      finish_progress(pb, paste0("Complete! R² = ", 
                               round(result$deconvolution_quality$mean_r_squared, 3)))
      
    }, error = function(e) {
      finish_progress(pb, paste("Error:", e$message))
      stop(e)
    })
    
  } else {
    # Compute without progress tracking
    result <- extract_universal_information(
      physeq,
      groups = groups,
      include_phylogenetic = include_phylogenetic
    )
  }
  
  # Store in cache
  if (use_cache && exists("cache_store")) {
    metadata <- list(
      dataset_type = "universal_information",
      n_samples = nsamples(physeq),
      n_taxa = ntaxa(physeq),
      has_phylogeny = include_phylogenetic,
      mean_r_squared = result$deconvolution_quality$mean_r_squared,
      computed_at = Sys.time()
    )
    
    if (!is.null(cache_metadata)) {
      metadata <- c(metadata, cache_metadata)
    }
    
    cache_store(cache_key, result, type = "transformation", metadata = metadata)
    
    if (show_progress) {
      message("Universal information cached for future use")
    }
  }
  
  return(result)
}

#' Cached diversity suite analysis
#'
#' Complete diversity analysis with caching and progress tracking
#'
#' @param physeq phyloseq object
#' @param groups Optional grouping variable
#' @param metrics Diversity metrics to calculate
#' @param include_universal Include universal transformation (default: TRUE)
#' @param include_consensus Include consensus analysis (default: TRUE)
#' @param use_cache Whether to use caching (default: TRUE)
#' @param force_recompute Force recomputation (default: FALSE)
#' @param show_progress Show progress bar (default: TRUE)
#'
#' @return Complete diversity analysis results
#' @export
cached_diversity_suite <- function(physeq,
                                  groups = NULL,
                                  metrics = c("shannon", "simpson", "observed", "chao1"),
                                  include_universal = TRUE,
                                  include_consensus = TRUE,
                                  use_cache = TRUE,
                                  force_recompute = FALSE,
                                  show_progress = TRUE) {
  
  # Load required functions
  load_diversity_functions()
  load_cache_system()
  load_progress_system()
  
  # Generate cache key for complete analysis
  cache_key <- generate_cache_key(
    physeq_hash = generate_phyloseq_hash(physeq),
    groups = groups,
    metrics = metrics,
    include_universal = include_universal,
    include_consensus = include_consensus,
    prefix = "diversity_suite"
  )
  
  # Check cache first
  if (use_cache && !force_recompute && exists("cache_get")) {
    cached_result <- cache_get(cache_key)
    if (!is.null(cached_result)) {
      if (show_progress) {
        message("Loaded diversity suite results from cache")
      }
      return(cached_result)
    }
  }
  
  # Compute diversity suite with progress
  n_steps <- 3 + as.numeric(include_universal) + as.numeric(include_consensus)
  
  if (show_progress) {
    pb <- create_progress_tracker(n_steps, "Computing Diversity Suite")
  }
  
  results <- list()
  step <- 0
  
  tryCatch({
    # Step 1: Basic diversity calculation
    step <- step + 1
    if (show_progress) update_progress(pb, step, "Calculating diversity metrics")
    
    if (!exists("calculate_diversity")) {
      source("R/calculate_diversity.R")
    }
    results$diversity_metrics <- calculate_diversity(physeq, metrics = metrics)
    
    # Step 2: Data summary
    step <- step + 1
    if (show_progress) update_progress(pb, step, "Summarizing data")
    
    results$data_summary <- list(
      n_samples = nsamples(physeq),
      n_taxa = ntaxa(physeq),
      has_tree = !is.null(phy_tree(physeq, errorIfNULL = FALSE)),
      groups = groups,
      metrics_computed = metrics
    )
    
    # Step 3: Universal transformation (if requested)
    if (include_universal) {
      step <- step + 1
      if (show_progress) update_progress(pb, step, "Computing universal transformations")
      
      results$universal_info <- cached_extract_universal_information(
        physeq,
        groups = groups,
        use_cache = use_cache,
        force_recompute = force_recompute,
        show_progress = FALSE  # Avoid nested progress bars
      )
    }
    
    # Step 4: Consensus analysis (if requested)
    if (include_consensus) {
      step <- step + 1
      if (show_progress) update_progress(pb, step, "Computing consensus diversity")
      
      if (!exists("consensus_diversity")) {
        source("R/consensus_diversity.R")
      }
      results$consensus <- consensus_diversity(
        results$diversity_metrics,
        method = "adaptive",
        groups = groups
      )
    }
    
    # Step 5: Finalization
    step <- step + 1
    if (show_progress) update_progress(pb, step, "Finalizing results")
    
    # Add analysis metadata
    results$analysis_info <- list(
      completed_at = Sys.time(),
      include_universal = include_universal,
      include_consensus = include_consensus,
      cache_used = use_cache
    )
    
    class(results) <- "diversity_suite_results"
    
    if (show_progress) {
      finish_message <- "Complete!"
      if (include_universal) {
        finish_message <- paste0(finish_message, " R² = ", 
                               round(results$universal_info$deconvolution_quality$mean_r_squared, 3))
      }
      finish_progress(pb, finish_message)
    }
    
  }, error = function(e) {
    if (show_progress) finish_progress(pb, paste("Error:", e$message))
    stop(e)
  })
  
  # Store in cache
  if (use_cache && exists("cache_store")) {
    metadata <- list(
      analysis_type = "diversity_suite",
      n_samples = results$data_summary$n_samples,
      n_taxa = results$data_summary$n_taxa,
      metrics = metrics,
      include_universal = include_universal,
      include_consensus = include_consensus,
      computed_at = Sys.time()
    )
    
    cache_store(cache_key, results, type = "transformation", metadata = metadata)
    
    if (show_progress) {
      message("Diversity suite results cached for future use")
    }
  }
  
  return(results)
}

#' Generate cache key for universal information
#' @keywords internal
generate_universal_cache_key <- function(physeq, groups, include_phylogenetic) {
  
  # Create a hash of the phyloseq object structure
  physeq_hash <- generate_phyloseq_hash(physeq)
  
  # Generate cache key
  cache_key <- generate_cache_key(
    physeq_hash = physeq_hash,
    groups = groups,
    include_phylogenetic = include_phylogenetic,
    prefix = "universal_info"
  )
  
  return(cache_key)
}

#' Generate hash for phyloseq object
#' @keywords internal
generate_phyloseq_hash <- function(physeq) {
  
  # Create summary characteristics for hashing
  characteristics <- list(
    n_samples = nsamples(physeq),
    n_taxa = ntaxa(physeq),
    sample_names_hash = if (nsamples(physeq) > 0) {
      digest::digest(sample_names(physeq), algo = "md5")
    } else { "no_samples" },
    taxa_names_hash = if (ntaxa(physeq) > 0) {
      digest::digest(taxa_names(physeq), algo = "md5")
    } else { "no_taxa" },
    otu_table_hash = if (ntaxa(physeq) > 0 && nsamples(physeq) > 0) {
      # Use a subset for large datasets
      otu_mat <- as(otu_table(physeq), "matrix")
      if (length(otu_mat) > 10000) {
        # Sample subset for very large matrices
        sample_indices <- sample(length(otu_mat), min(1000, length(otu_mat)))
        otu_subset <- otu_mat[sample_indices]
      } else {
        otu_subset <- otu_mat
      }
      digest::digest(otu_subset, algo = "md5")
    } else { "no_data" },
    has_tree = !is.null(phy_tree(physeq, errorIfNULL = FALSE))
  )
  
  # Generate combined hash
  if (requireNamespace("digest", quietly = TRUE)) {
    return(digest::digest(characteristics, algo = "md5"))
  } else {
    # Fallback without digest
    return(paste(unlist(characteristics), collapse = "_"))
  }
}

#' Load cache system helper
#' @keywords internal
load_cache_system <- function() {
  if (!exists("cache_is_initialized")) {
    cache_file <- system.file("R", "cache_system.R", package = "diversityGPT")
    if (cache_file == "" || !file.exists(cache_file)) {
      cache_file <- "R/cache_system.R"
    }
    if (file.exists(cache_file)) {
      source(cache_file)
      if (!cache_is_initialized()) {
        init_cache_system()
      }
    }
  }
}

#' Load progress system helper
#' @keywords internal
load_progress_system <- function() {
  if (!exists("create_progress_tracker")) {
    progress_file <- system.file("R", "progress_tracking.R", package = "diversityGPT")
    if (progress_file == "" || !file.exists(progress_file)) {
      progress_file <- "R/progress_tracking.R"
    }
    if (file.exists(progress_file)) {
      source(progress_file)
    }
  }
}

#' Load diversity functions helper
#' @keywords internal
load_diversity_functions <- function() {
  required_files <- c(
    "universal_information.R",
    "calculate_diversity.R",
    "consensus_diversity.R"
  )
  
  for (file in required_files) {
    if (!exists(gsub("\\.R$", "", file))) {
      func_file <- system.file("R", file, package = "diversityGPT")
      if (func_file == "" || !file.exists(func_file)) {
        func_file <- file.path("R", file)
      }
      if (file.exists(func_file)) {
        source(func_file)
      }
    }
  }
}