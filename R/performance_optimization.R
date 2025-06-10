#' Performance Optimization for diversityGPT
#'
#' Functions to improve performance for large datasets and computationally
#' intensive operations using parallel processing and efficient algorithms
#'

#' Parallel extraction of universal information
#'
#' Use parallel processing to speed up universal information extraction
#' for large phyloseq objects
#'
#' @param physeq phyloseq object
#' @param groups Optional grouping variable
#' @param n_cores Number of cores to use (NULL for auto-detect)
#' @param chunk_size Number of samples per chunk
#' @param progress Show progress bar
#' @param ... Additional arguments passed to extract_universal_information
#'
#' @return Universal information object
#' @export
#' @examples
#' \dontrun{
#' # Use parallel processing for large dataset
#' universal_info <- parallel_extract_universal(
#'   large_phyloseq,
#'   n_cores = 4,
#'   progress = TRUE
#' )
#' }
parallel_extract_universal <- function(physeq, 
                                     groups = NULL,
                                     n_cores = NULL,
                                     chunk_size = 100,
                                     progress = TRUE,
                                     ...) {
  
  # Check if parallel package is available
  if (!requireNamespace("parallel", quietly = TRUE)) {
    message("'parallel' package not available. Using single-threaded processing.")
    return(extract_universal_information(physeq, groups = groups, ...))
  }
  
  # Determine number of cores
  if (is.null(n_cores)) {
    n_cores <- max(1, parallel::detectCores() - 1)
  }
  n_cores <- min(n_cores, parallel::detectCores())
  
  # Get sample size
  n_samples <- phyloseq::nsamples(physeq)
  
  # If dataset is small, use single-threaded
  if (n_samples < chunk_size * 2) {
    message("Dataset too small for parallel processing. Using single-threaded.")
    return(extract_universal_information(physeq, groups = groups, ...))
  }
  
  message(paste("Using", n_cores, "cores for parallel processing"))
  
  # Create chunks
  chunks <- split(
    phyloseq::sample_names(physeq),
    ceiling(seq_along(phyloseq::sample_names(physeq)) / chunk_size)
  )
  
  # Create progress tracker if requested
  if (progress) {
    pb <- create_progress_tracker(length(chunks), "Processing chunks")
  }
  
  # Process chunks in parallel
  if (.Platform$OS.type == "unix") {
    # Use mclapply on Unix-like systems
    chunk_results <- parallel::mclapply(
      seq_along(chunks),
      function(i) {
        chunk_samples <- chunks[[i]]
        chunk_physeq <- phyloseq::prune_samples(chunk_samples, physeq)
        
        # Extract information for chunk
        result <- extract_universal_information(
          chunk_physeq,
          groups = groups,
          ...
        )
        
        if (progress) {
          update_progress(pb, i)
        }
        
        return(result)
      },
      mc.cores = n_cores
    )
  } else {
    # Use parLapply on Windows
    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl))
    
    # Export necessary objects
    parallel::clusterExport(cl, c("physeq", "groups", "chunks"), 
                           envir = environment())
    parallel::clusterEvalQ(cl, library(diversityGPT))
    
    chunk_results <- parallel::parLapply(
      cl,
      seq_along(chunks),
      function(i) {
        chunk_samples <- chunks[[i]]
        chunk_physeq <- phyloseq::prune_samples(chunk_samples, physeq)
        
        result <- extract_universal_information(
          chunk_physeq,
          groups = groups
        )
        
        return(result)
      }
    )
  }
  
  if (progress) {
    finish_progress(pb, "Merging results")
  }
  
  # Merge chunk results
  merged_result <- merge_universal_results(chunk_results)
  
  return(merged_result)
}

#' Merge universal information results from chunks
#' @keywords internal
merge_universal_results <- function(chunk_results) {
  
  # Extract components from each chunk
  all_components <- do.call(rbind, lapply(chunk_results, function(x) {
    x$information_components
  }))
  
  # Average transformation matrices
  all_matrices <- lapply(chunk_results, function(x) x$transformation_matrix)
  avg_matrix <- Reduce("+", all_matrices) / length(all_matrices)
  
  # Combine quality metrics
  all_r_squared <- unlist(lapply(chunk_results, function(x) {
    x$deconvolution_quality$metric_r_squared
  }))
  
  # Build merged result
  merged <- list(
    information_components = all_components,
    transformation_matrix = avg_matrix,
    deconvolution_quality = list(
      metric_r_squared = tapply(all_r_squared, names(all_r_squared), mean),
      mean_r_squared = mean(all_r_squared, na.rm = TRUE),
      components_present = chunk_results[[1]]$deconvolution_quality$components_present,
      overall_quality = ifelse(
        mean(all_r_squared, na.rm = TRUE) > 0.8,
        "Excellent",
        ifelse(mean(all_r_squared, na.rm = TRUE) > 0.6, "Good", "Moderate")
      )
    ),
    metadata = chunk_results[[1]]$metadata
  )
  
  class(merged) <- "universal_information"
  return(merged)
}

#' Optimized diversity calculation
#'
#' Calculate diversity metrics with optimizations for large datasets
#'
#' @param physeq phyloseq object
#' @param metrics Vector of metric names
#' @param use_sparse Use sparse matrix operations when applicable
#' @param batch_size Process samples in batches
#' @param ... Additional arguments
#'
#' @return Data frame of diversity metrics
#' @export
#' @examples
#' \dontrun{
#' # Optimized calculation for large dataset
#' div_results <- optimized_calculate_diversity(
#'   large_phyloseq,
#'   metrics = c("shannon", "simpson"),
#'   use_sparse = TRUE,
#'   batch_size = 500
#' )
#' }
optimized_calculate_diversity <- function(physeq,
                                        metrics = c("shannon", "simpson", "observed"),
                                        use_sparse = TRUE,
                                        batch_size = 1000,
                                        ...) {
  
  n_samples <- phyloseq::nsamples(physeq)
  
  # For small datasets, use regular calculation
  if (n_samples <= batch_size) {
    return(calculate_diversity(physeq, metrics = metrics, ...))
  }
  
  # Check if we can use sparse matrices
  otu_data <- phyloseq::otu_table(physeq)
  sparsity <- sum(otu_data == 0) / length(otu_data)
  
  if (use_sparse && sparsity > 0.7 && requireNamespace("Matrix", quietly = TRUE)) {
    message("Using sparse matrix optimizations (", round(sparsity * 100), "% sparse)")
    
    # Convert to sparse matrix
    if (phyloseq::taxa_are_rows(physeq)) {
      sparse_otu <- Matrix::Matrix(as.matrix(otu_data), sparse = TRUE)
    } else {
      sparse_otu <- Matrix::Matrix(t(as.matrix(otu_data)), sparse = TRUE)
    }
    
    # Use optimized calculations for sparse data
    return(calculate_diversity_sparse(sparse_otu, metrics, physeq))
  }
  
  # Process in batches
  message("Processing ", n_samples, " samples in batches of ", batch_size)
  
  batches <- split(
    phyloseq::sample_names(physeq),
    ceiling(seq_along(phyloseq::sample_names(physeq)) / batch_size)
  )
  
  pb <- create_progress_tracker(length(batches), "Processing batches")
  
  batch_results <- lapply(seq_along(batches), function(i) {
    batch_samples <- batches[[i]]
    batch_physeq <- phyloseq::prune_samples(batch_samples, physeq)
    
    result <- calculate_diversity(batch_physeq, metrics = metrics, ...)
    
    update_progress(pb, i)
    
    return(result)
  })
  
  finish_progress(pb)
  
  # Combine results
  combined_results <- do.call(rbind, batch_results)
  
  return(combined_results)
}

#' Calculate diversity metrics from sparse matrix
#' @keywords internal
calculate_diversity_sparse <- function(sparse_otu, metrics, physeq) {
  
  # Initialize results
  n_samples <- ncol(sparse_otu)
  results <- data.frame(
    sample = phyloseq::sample_names(physeq),
    stringsAsFactors = FALSE
  )
  
  # Efficient calculations for sparse matrices
  if ("observed" %in% metrics) {
    results$observed <- Matrix::colSums(sparse_otu > 0)
  }
  
  if ("shannon" %in% metrics) {
    # Shannon for sparse matrix
    results$shannon <- apply(sparse_otu, 2, function(x) {
      x <- x[x > 0]  # Only non-zero values
      if (length(x) == 0) return(0)
      p <- x / sum(x)
      -sum(p * log(p))
    })
  }
  
  if ("simpson" %in% metrics) {
    # Simpson for sparse matrix
    results$simpson <- apply(sparse_otu, 2, function(x) {
      x <- x[x > 0]
      if (length(x) == 0) return(0)
      p <- x / sum(x)
      1 - sum(p^2)
    })
  }
  
  # Add more metrics as needed...
  
  return(results)
}

#' Memory-efficient transformation
#'
#' Transform diversity metrics with memory optimization for large matrices
#'
#' @param source_metrics Named vector or matrix of source metrics
#' @param target_metrics Character vector of target metric names
#' @param transformation_matrix Transformation matrix
#' @param chunk_size Process in chunks to save memory
#' @param ... Additional arguments
#'
#' @return Transformation results
#' @export
memory_efficient_transform <- function(source_metrics,
                                     target_metrics,
                                     transformation_matrix,
                                     chunk_size = 10000,
                                     ...) {
  
  # If source_metrics is small, use regular transformation
  if (is.vector(source_metrics) || nrow(source_metrics) < chunk_size) {
    return(universal_diversity_transform(
      source_metrics,
      target_metrics,
      transformation_matrix,
      ...
    ))
  }
  
  # Process large matrices in chunks
  n_samples <- nrow(source_metrics)
  chunks <- split(1:n_samples, ceiling((1:n_samples) / chunk_size))
  
  pb <- create_progress_tracker(length(chunks), "Transforming metrics")
  
  chunk_results <- lapply(seq_along(chunks), function(i) {
    chunk_indices <- chunks[[i]]
    chunk_metrics <- source_metrics[chunk_indices, , drop = FALSE]
    
    result <- universal_diversity_transform(
      chunk_metrics,
      target_metrics,
      transformation_matrix
    )
    
    update_progress(pb, i)
    
    return(result)
  })
  
  finish_progress(pb)
  
  # Combine results
  combined_predictions <- do.call(rbind, lapply(chunk_results, function(x) {
    x$predictions
  }))
  
  # Average quality metrics
  avg_quality <- do.call(rbind, lapply(chunk_results, function(x) {
    x$quality
  }))
  avg_quality <- aggregate(. ~ metric, data = avg_quality, FUN = mean)
  
  return(list(
    predictions = combined_predictions,
    quality = avg_quality,
    method = "memory_efficient"
  ))
}

#' Precompute and save universal information
#'
#' Precompute universal information for large datasets and save to disk
#'
#' @param physeq phyloseq object
#' @param output_file Path to save precomputed results
#' @param compress Compression level (0-9)
#' @param ... Additional arguments
#'
#' @return Path to saved file
#' @export
#' @examples
#' \dontrun{
#' # Precompute for later use
#' precompute_universal_info(
#'   large_phyloseq,
#'   "universal_info_large.rds",
#'   compress = 9
#' )
#' }
precompute_universal_info <- function(physeq,
                                    output_file,
                                    compress = 9,
                                    ...) {
  
  message("Precomputing universal information...")
  
  # Check available memory
  mem_available <- as.numeric(system("awk '/MemAvailable/ {print $2}' /proc/meminfo", 
                                    intern = TRUE, 
                                    ignore.stderr = TRUE))
  
  if (!is.na(mem_available)) {
    mem_gb <- mem_available / 1024 / 1024
    if (mem_gb < 4) {
      warning("Low memory available (", round(mem_gb, 1), " GB). Consider using smaller chunks.")
    }
  }
  
  # Use parallel processing if dataset is large
  if (phyloseq::nsamples(physeq) > 1000) {
    universal_info <- parallel_extract_universal(physeq, progress = TRUE, ...)
  } else {
    universal_info <- extract_universal_information(physeq, ...)
  }
  
  # Add metadata
  universal_info$precomputed <- list(
    date = Sys.time(),
    version = packageVersion("diversityGPT"),
    n_samples = phyloseq::nsamples(physeq),
    n_taxa = phyloseq::ntaxa(physeq)
  )
  
  # Save with compression
  message("Saving precomputed results...")
  saveRDS(universal_info, file = output_file, compress = compress)
  
  # Report file size
  file_size <- file.info(output_file)$size / 1024 / 1024
  message("Saved to ", output_file, " (", round(file_size, 1), " MB)")
  
  return(output_file)
}

#' Load precomputed universal information
#'
#' @param file_path Path to precomputed RDS file
#' @param verify Check integrity of loaded data
#'
#' @return Universal information object
#' @export
load_precomputed_universal <- function(file_path, verify = TRUE) {
  
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }
  
  message("Loading precomputed universal information...")
  universal_info <- readRDS(file_path)
  
  if (verify) {
    # Verify structure
    required_elements <- c("information_components", "transformation_matrix", 
                          "deconvolution_quality")
    
    missing <- setdiff(required_elements, names(universal_info))
    if (length(missing) > 0) {
      warning("Missing elements in precomputed data: ", 
              paste(missing, collapse = ", "))
    }
    
    # Check version compatibility
    if (!is.null(universal_info$precomputed$version)) {
      current_version <- packageVersion("diversityGPT")
      saved_version <- universal_info$precomputed$version
      
      if (saved_version < current_version) {
        message("Note: Data was computed with older version (",
                saved_version, " vs current ", current_version, ")")
      }
    }
  }
  
  # Report info
  if (!is.null(universal_info$precomputed)) {
    message("Precomputed on: ", universal_info$precomputed$date)
    message("Samples: ", universal_info$precomputed$n_samples,
            ", Taxa: ", universal_info$precomputed$n_taxa)
  }
  
  return(universal_info)
}

#' Enable performance monitoring
#'
#' Set options to monitor and report performance metrics
#'
#' @param enable Enable performance monitoring
#' @param verbose Print performance reports
#'
#' @export
#' @examples
#' \dontrun{
#' # Enable performance monitoring
#' enable_performance_monitoring(TRUE)
#' 
#' # Run analysis
#' results <- extract_universal_information(physeq)
#' 
#' # Disable monitoring
#' enable_performance_monitoring(FALSE)
#' }
enable_performance_monitoring <- function(enable = TRUE, verbose = TRUE) {
  
  options(
    diversityGPT.monitor_performance = enable,
    diversityGPT.performance_verbose = verbose
  )
  
  if (enable && verbose) {
    message("Performance monitoring enabled")
  }
  
  invisible(enable)
}

#' Get performance report
#'
#' Get summary of performance metrics from recent operations
#'
#' @return Performance report
#' @export
get_performance_report <- function() {
  
  perf_data <- getOption("diversityGPT.performance_data", list())
  
  if (length(perf_data) == 0) {
    message("No performance data available. Enable monitoring first.")
    return(invisible(NULL))
  }
  
  # Summarize performance data
  report <- list(
    n_operations = length(perf_data),
    total_time = sum(sapply(perf_data, function(x) x$elapsed)),
    operations = perf_data
  )
  
  class(report) <- "performance_report"
  
  return(report)
}

#' Print performance report
#' @export
print.performance_report <- function(x, ...) {
  cat("diversityGPT Performance Report\n")
  cat("==============================\n")
  cat("Total operations:", x$n_operations, "\n")
  cat("Total time:", round(x$total_time, 2), "seconds\n")
  cat("\nOperation breakdown:\n")
  
  for (op in x$operations) {
    cat("  -", op$operation, ":", round(op$elapsed, 3), "sec")
    if (!is.null(op$n_items)) {
      cat(" (", op$n_items, " items, ",
          round(op$n_items / op$elapsed, 1), " items/sec)")
    }
    cat("\n")
  }
}