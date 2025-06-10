#' Batch Processing for diversityGPT
#'
#' Functions for processing multiple datasets efficiently using
#' parallel processing, caching, and automated workflow management.

#' Batch Process Multiple Phyloseq Objects
#'
#' Efficiently processes multiple phyloseq objects through the complete
#' diversityGPT analysis pipeline with parallel processing and result caching.
#'
#' @param phyloseq_list List of phyloseq objects to process
#' @param dataset_names Optional names for datasets (default: Dataset1, Dataset2, ...)
#' @param analysis_steps Vector of analysis steps to perform: 
#'   "universal", "mechanisms", "hypotheses", "literature", "validation"
#' @param output_dir Directory to save results (default: "diversityGPT_batch_results")
#' @param parallel_cores Number of cores for parallel processing (NULL for auto-detect)
#' @param cache_results Whether to cache intermediate results
#' @param generate_reports Whether to generate individual reports for each dataset
#' @param generate_summary Whether to generate a summary report across all datasets
#' @param report_format Format for reports: "html", "pdf", or "both"
#' @param resume_failed Whether to resume processing of failed datasets
#' @param save_progress Whether to save progress after each dataset
#' @param study_context Optional study context information for each dataset
#' @param llm_provider LLM provider for ecological analysis: "anthropic", "openai", "none"
#'
#' @return A list containing:
#'   \item{results}{List of analysis results for each dataset}
#'   \item{summary_statistics}{Summary statistics across all datasets}
#'   \item{processing_log}{Detailed processing log with timing and errors}
#'   \item{failed_datasets}{List of datasets that failed processing}
#'   \item{batch_metadata}{Metadata about the batch processing run}
#'
#' @examples
#' \dontrun{
#' # Create multiple test datasets
#' data(GlobalPatterns)
#' dataset1 <- create_demo_subset(GlobalPatterns, n_samples = 20, seed = 123)
#' dataset2 <- create_demo_subset(GlobalPatterns, n_samples = 25, seed = 456)
#' dataset3 <- create_demo_subset(GlobalPatterns, n_samples = 15, seed = 789)
#' 
#' # Batch process with full analysis
#' batch_results <- batch_process_phyloseq(
#'   phyloseq_list = list(dataset1, dataset2, dataset3),
#'   dataset_names = c("Marine", "Soil", "Gut"),
#'   analysis_steps = c("universal", "mechanisms", "hypotheses"),
#'   parallel_cores = 2,
#'   generate_reports = TRUE,
#'   generate_summary = TRUE
#' )
#' 
#' print(batch_results)
#' 
#' # Resume failed datasets
#' resumed_results <- batch_process_phyloseq(
#'   phyloseq_list = list(dataset1, dataset2, dataset3),
#'   dataset_names = c("Marine", "Soil", "Gut"),
#'   output_dir = "diversityGPT_batch_results",
#'   resume_failed = TRUE
#' )
#' }
#'
#' @export
batch_process_phyloseq <- function(phyloseq_list,
                                 dataset_names = NULL,
                                 analysis_steps = c("universal", "mechanisms", "hypotheses"),
                                 output_dir = "diversityGPT_batch_results",
                                 parallel_cores = NULL,
                                 cache_results = TRUE,
                                 generate_reports = FALSE,
                                 generate_summary = TRUE,
                                 report_format = "html",
                                 resume_failed = FALSE,
                                 save_progress = TRUE,
                                 study_context = NULL,
                                 llm_provider = "none") {
  
  # Validate inputs
  if (!is.list(phyloseq_list) || length(phyloseq_list) == 0) {
    stop("phyloseq_list must be a non-empty list")
  }
  
  n_datasets <- length(phyloseq_list)
  
  # Generate dataset names if not provided
  if (is.null(dataset_names)) {
    dataset_names <- paste0("Dataset", 1:n_datasets)
  } else if (length(dataset_names) != n_datasets) {
    stop("dataset_names must have same length as phyloseq_list")
  }
  
  # Validate analysis steps
  valid_steps <- c("universal", "mechanisms", "hypotheses", "literature", "validation")
  invalid_steps <- setdiff(analysis_steps, valid_steps)
  if (length(invalid_steps) > 0) {
    stop("Invalid analysis steps: ", paste(invalid_steps, collapse = ", "))
  }
  
  # Create output directory
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Set up parallel processing
  if (is.null(parallel_cores)) {
    parallel_cores <- max(1, parallel::detectCores() - 1)
  }
  parallel_cores <- min(parallel_cores, n_datasets, parallel::detectCores())
  
  cat("Batch Processing Configuration\n")
  cat("=============================\n")
  cat("Datasets:", n_datasets, "\n")
  cat("Analysis steps:", paste(analysis_steps, collapse = ", "), "\n")
  cat("Parallel cores:", parallel_cores, "\n")
  cat("Output directory:", output_dir, "\n")
  cat("Cache results:", cache_results, "\n")
  cat("Generate reports:", generate_reports, "\n\n")
  
  # Initialize batch results
  batch_results <- list(
    results = list(),
    summary_statistics = list(),
    processing_log = data.frame(
      dataset = character(),
      status = character(),
      start_time = character(),
      end_time = character(),
      elapsed_seconds = numeric(),
      error_message = character(),
      stringsAsFactors = FALSE
    ),
    failed_datasets = character(),
    batch_metadata = list(
      n_datasets = n_datasets,
      dataset_names = dataset_names,
      analysis_steps = analysis_steps,
      parallel_cores = parallel_cores,
      output_dir = output_dir,
      start_time = Sys.time(),
      end_time = NULL,
      package_version = packageVersion("diversityGPT")
    )
  )
  
  # Check for existing results if resuming
  existing_results <- list()
  if (resume_failed) {
    cat("Checking for existing results...\n")
    existing_results <- .load_existing_batch_results(output_dir, dataset_names)
    cat("Found", length(existing_results), "existing results\n\n")
  }
  
  # Process datasets
  if (n_datasets == 1 || parallel_cores == 1) {
    # Sequential processing
    cat("Processing datasets sequentially...\n")
    for (i in 1:n_datasets) {
      result <- .process_single_dataset(
        phyloseq_list[[i]], dataset_names[i], analysis_steps, 
        output_dir, cache_results, generate_reports, report_format,
        study_context, llm_provider, existing_results
      )
      
      batch_results$results[[dataset_names[i]]] <- result$analysis_result
      batch_results$processing_log <- rbind(batch_results$processing_log, result$log_entry)
      
      if (result$log_entry$status == "failed") {
        batch_results$failed_datasets <- c(batch_results$failed_datasets, dataset_names[i])
      }
      
      # Save progress if requested
      if (save_progress) {
        .save_batch_progress(batch_results, output_dir)
      }
    }
  } else {
    # Parallel processing
    cat("Processing datasets in parallel...\n")
    
    if (.Platform$OS.type == "unix") {
      # Use mclapply on Unix-like systems
      parallel_results <- parallel::mclapply(
        1:n_datasets,
        function(i) {
          .process_single_dataset(
            phyloseq_list[[i]], dataset_names[i], analysis_steps,
            output_dir, cache_results, generate_reports, report_format,
            study_context, llm_provider, existing_results
          )
        },
        mc.cores = parallel_cores
      )
    } else {
      # Use parLapply on Windows
      cl <- parallel::makeCluster(parallel_cores)
      on.exit(parallel::stopCluster(cl))
      
      # Export necessary objects
      parallel::clusterExport(cl, c("phyloseq_list", "dataset_names", "analysis_steps",
                                   "output_dir", "cache_results", "generate_reports", 
                                   "report_format", "study_context", "llm_provider",
                                   "existing_results"), envir = environment())
      parallel::clusterEvalQ(cl, library(diversityGPT))
      
      parallel_results <- parallel::parLapply(
        cl,
        1:n_datasets,
        function(i) {
          .process_single_dataset(
            phyloseq_list[[i]], dataset_names[i], analysis_steps,
            output_dir, cache_results, generate_reports, report_format,
            study_context, llm_provider, existing_results
          )
        }
      )
    }
    
    # Collect parallel results
    for (i in 1:n_datasets) {
      result <- parallel_results[[i]]
      batch_results$results[[dataset_names[i]]] <- result$analysis_result
      batch_results$processing_log <- rbind(batch_results$processing_log, result$log_entry)
      
      if (result$log_entry$status == "failed") {
        batch_results$failed_datasets <- c(batch_results$failed_datasets, dataset_names[i])
      }
    }
  }
  
  # Update batch metadata
  batch_results$batch_metadata$end_time <- Sys.time()
  batch_results$batch_metadata$total_elapsed <- as.numeric(
    difftime(batch_results$batch_metadata$end_time, 
             batch_results$batch_metadata$start_time, units = "secs")
  )
  
  # Generate summary statistics
  cat("Generating summary statistics...\n")
  batch_results$summary_statistics <- .generate_batch_summary_statistics(batch_results)
  
  # Generate summary report
  if (generate_summary) {
    cat("Generating summary report...\n")
    summary_report_path <- .generate_batch_summary_report(batch_results, output_dir, report_format)
    batch_results$summary_report_path <- summary_report_path
  }
  
  # Save final results
  final_results_path <- file.path(output_dir, "batch_results.rds")
  saveRDS(batch_results, final_results_path)
  
  # Add class for S3 methods
  class(batch_results) <- c("batch_processing", "list")
  
  cat("\nBatch Processing Complete!\n")
  cat("Successful:", sum(batch_results$processing_log$status == "success"), "/", n_datasets, "\n")
  cat("Failed:", length(batch_results$failed_datasets), "\n")
  cat("Total time:", round(batch_results$batch_metadata$total_elapsed, 1), "seconds\n")
  cat("Results saved to:", output_dir, "\n")
  
  return(batch_results)
}

# Internal function: Process a single dataset
.process_single_dataset <- function(phyloseq_obj, dataset_name, analysis_steps, 
                                   output_dir, cache_results, generate_reports, 
                                   report_format, study_context, llm_provider,
                                   existing_results) {
  
  start_time <- Sys.time()
  
  # Check if results already exist
  if (dataset_name %in% names(existing_results)) {
    cat("Skipping", dataset_name, "- results already exist\n")
    return(list(
      analysis_result = existing_results[[dataset_name]],
      log_entry = data.frame(
        dataset = dataset_name,
        status = "skipped",
        start_time = as.character(start_time),
        end_time = as.character(Sys.time()),
        elapsed_seconds = 0,
        error_message = "Results already exist",
        stringsAsFactors = FALSE
      )
    ))
  }
  
  cat("Processing:", dataset_name, "\n")
  
  # Initialize results for this dataset
  dataset_results <- list(
    dataset_name = dataset_name,
    universal_info = NULL,
    assembly_mechanisms = NULL,
    hypotheses = NULL,
    literature = NULL,
    validation = NULL
  )
  
  error_message <- ""
  
  tryCatch({
    # Step 1: Universal information extraction
    if ("universal" %in% analysis_steps) {
      cache_file <- if (cache_results) file.path(output_dir, paste0(dataset_name, "_universal.rds")) else NULL
      
      if (!is.null(cache_file) && file.exists(cache_file)) {
        dataset_results$universal_info <- readRDS(cache_file)
      } else {
        dataset_results$universal_info <- extract_universal_information(phyloseq_obj)
        if (!is.null(cache_file)) {
          saveRDS(dataset_results$universal_info, cache_file)
        }
      }
    }
    
    # Step 2: Assembly mechanisms
    if ("mechanisms" %in% analysis_steps && !is.null(dataset_results$universal_info)) {
      cache_file <- if (cache_results) file.path(output_dir, paste0(dataset_name, "_mechanisms.rds")) else NULL
      
      if (!is.null(cache_file) && file.exists(cache_file)) {
        dataset_results$assembly_mechanisms <- readRDS(cache_file)
      } else {
        dataset_results$assembly_mechanisms <- detect_assembly_mechanisms(dataset_results$universal_info)
        if (!is.null(cache_file)) {
          saveRDS(dataset_results$assembly_mechanisms, cache_file)
        }
      }
    }
    
    # Step 3: Ecological hypotheses
    if ("hypotheses" %in% analysis_steps && !is.null(dataset_results$universal_info)) {
      cache_file <- if (cache_results) file.path(output_dir, paste0(dataset_name, "_hypotheses.rds")) else NULL
      
      if (!is.null(cache_file) && file.exists(cache_file)) {
        dataset_results$hypotheses <- readRDS(cache_file)
      } else {
        context <- if (!is.null(study_context)) study_context[[dataset_name]] else NULL
        dataset_results$hypotheses <- generate_ecological_hypotheses(
          dataset_results$universal_info,
          dataset_results$assembly_mechanisms,
          study_context = context,
          llm_provider = llm_provider
        )
        if (!is.null(cache_file)) {
          saveRDS(dataset_results$hypotheses, cache_file)
        }
      }
    }
    
    # Step 4: Literature integration
    if ("literature" %in% analysis_steps && !is.null(dataset_results$universal_info)) {
      cache_file <- if (cache_results) file.path(output_dir, paste0(dataset_name, "_literature.rds")) else NULL
      
      if (!is.null(cache_file) && file.exists(cache_file)) {
        dataset_results$literature <- readRDS(cache_file)
      } else {
        context <- if (!is.null(study_context)) study_context[[dataset_name]] else NULL
        dataset_results$literature <- search_literature(
          dataset_results$universal_info,
          dataset_results$assembly_mechanisms,
          dataset_results$hypotheses,
          study_context = context,
          max_papers = 5,  # Limit for batch processing
          llm_synthesis = FALSE  # Disable for batch processing speed
        )
        if (!is.null(cache_file)) {
          saveRDS(dataset_results$literature, cache_file)
        }
      }
    }
    
    # Step 5: Statistical validation
    if ("validation" %in% analysis_steps && !is.null(dataset_results$universal_info)) {
      cache_file <- if (cache_results) file.path(output_dir, paste0(dataset_name, "_validation.rds")) else NULL
      
      if (!is.null(cache_file) && file.exists(cache_file)) {
        dataset_results$validation <- readRDS(cache_file)
      } else {
        dataset_results$validation <- validate_universal_analysis(
          dataset_results$universal_info,
          phyloseq_obj,
          bootstrap_n = 100  # Reduced for batch processing
        )
        if (!is.null(cache_file)) {
          saveRDS(dataset_results$validation, cache_file)
        }
      }
    }
    
    # Generate individual report if requested
    if (generate_reports && !is.null(dataset_results$universal_info)) {
      report_file <- file.path(output_dir, paste0(dataset_name, "_report"))
      context <- if (!is.null(study_context)) study_context[[dataset_name]] else NULL
      
      generate_diversity_report(
        universal_info = dataset_results$universal_info,
        phyloseq_obj = phyloseq_obj,
        assembly_mechanisms = dataset_results$assembly_mechanisms,
        hypotheses = dataset_results$hypotheses,
        literature = dataset_results$literature,
        validation = dataset_results$validation,
        output_file = report_file,
        output_format = report_format,
        template = "summary",  # Use summary template for batch processing
        include_code = FALSE,
        title = paste("Analysis Report:", dataset_name),
        author = "diversityGPT Batch Processing",
        study_context = context
      )
    }
    
    status <- "success"
    
  }, error = function(e) {
    error_message <<- e$message
    status <<- "failed"
    warning("Dataset ", dataset_name, " failed: ", e$message)
  })
  
  end_time <- Sys.time()
  elapsed_seconds <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Create log entry
  log_entry <- data.frame(
    dataset = dataset_name,
    status = status,
    start_time = as.character(start_time),
    end_time = as.character(end_time),
    elapsed_seconds = elapsed_seconds,
    error_message = error_message,
    stringsAsFactors = FALSE
  )
  
  return(list(
    analysis_result = dataset_results,
    log_entry = log_entry
  ))
}

# Internal function: Load existing batch results
.load_existing_batch_results <- function(output_dir, dataset_names) {
  
  existing_results <- list()
  
  # Check for individual cached files
  for (dataset_name in dataset_names) {
    universal_file <- file.path(output_dir, paste0(dataset_name, "_universal.rds"))
    
    if (file.exists(universal_file)) {
      # Try to load all components for this dataset
      dataset_result <- list(
        dataset_name = dataset_name,
        universal_info = NULL,
        assembly_mechanisms = NULL,
        hypotheses = NULL,
        literature = NULL,
        validation = NULL
      )
      
      # Load universal info
      dataset_result$universal_info <- readRDS(universal_file)
      
      # Load other components if they exist
      mechanisms_file <- file.path(output_dir, paste0(dataset_name, "_mechanisms.rds"))
      if (file.exists(mechanisms_file)) {
        dataset_result$assembly_mechanisms <- readRDS(mechanisms_file)
      }
      
      hypotheses_file <- file.path(output_dir, paste0(dataset_name, "_hypotheses.rds"))
      if (file.exists(hypotheses_file)) {
        dataset_result$hypotheses <- readRDS(hypotheses_file)
      }
      
      literature_file <- file.path(output_dir, paste0(dataset_name, "_literature.rds"))
      if (file.exists(literature_file)) {
        dataset_result$literature <- readRDS(literature_file)
      }
      
      validation_file <- file.path(output_dir, paste0(dataset_name, "_validation.rds"))
      if (file.exists(validation_file)) {
        dataset_result$validation <- readRDS(validation_file)
      }
      
      existing_results[[dataset_name]] <- dataset_result
    }
  }
  
  return(existing_results)
}

# Internal function: Save batch progress
.save_batch_progress <- function(batch_results, output_dir) {
  
  progress_file <- file.path(output_dir, "batch_progress.rds")
  saveRDS(batch_results, progress_file)
}

# Internal function: Generate batch summary statistics
.generate_batch_summary_statistics <- function(batch_results) {
  
  successful_results <- batch_results$results[!names(batch_results$results) %in% batch_results$failed_datasets]
  
  if (length(successful_results) == 0) {
    return(list(
      n_successful = 0,
      n_failed = length(batch_results$failed_datasets),
      summary = "No successful analyses to summarize"
    ))
  }
  
  # Dataset characteristics
  n_samples <- sapply(successful_results, function(x) {
    if (!is.null(x$universal_info)) {
      nrow(x$universal_info$information_components)
    } else {
      NA
    }
  })
  
  # Transformation quality
  transformation_quality <- sapply(successful_results, function(x) {
    if (!is.null(x$universal_info$deconvolution_quality)) {
      x$universal_info$deconvolution_quality$mean_r_squared
    } else {
      NA
    }
  })
  
  # Assembly mechanisms
  primary_mechanisms <- sapply(successful_results, function(x) {
    if (!is.null(x$assembly_mechanisms) && nrow(x$assembly_mechanisms$mechanisms) > 0) {
      x$assembly_mechanisms$mechanisms$mechanism[1]
    } else {
      NA
    }
  })
  
  # Processing times
  processing_times <- batch_results$processing_log$elapsed_seconds[
    batch_results$processing_log$status == "success"
  ]
  
  # Component statistics
  component_stats <- .calculate_cross_dataset_component_stats(successful_results)
  
  summary_stats <- list(
    n_successful = length(successful_results),
    n_failed = length(batch_results$failed_datasets),
    dataset_summary = list(
      sample_sizes = list(
        mean = mean(n_samples, na.rm = TRUE),
        median = median(n_samples, na.rm = TRUE),
        range = range(n_samples, na.rm = TRUE)
      ),
      transformation_quality = list(
        mean = mean(transformation_quality, na.rm = TRUE),
        median = median(transformation_quality, na.rm = TRUE),
        range = range(transformation_quality, na.rm = TRUE)
      )
    ),
    processing_summary = list(
      total_time_seconds = sum(processing_times),
      mean_time_per_dataset = mean(processing_times),
      median_time_per_dataset = median(processing_times),
      fastest_dataset = min(processing_times),
      slowest_dataset = max(processing_times)
    ),
    assembly_mechanisms = list(
      mechanism_frequency = table(primary_mechanisms, useNA = "ifany"),
      most_common_mechanism = names(sort(table(primary_mechanisms), decreasing = TRUE))[1]
    ),
    component_statistics = component_stats
  )
  
  return(summary_stats)
}

# Internal function: Calculate cross-dataset component statistics
.calculate_cross_dataset_component_stats <- function(successful_results) {
  
  # Extract all components
  all_components <- lapply(successful_results, function(x) {
    if (!is.null(x$universal_info)) {
      x$universal_info$information_components
    } else {
      NULL
    }
  })
  
  # Remove NULL entries
  all_components <- all_components[!sapply(all_components, is.null)]
  
  if (length(all_components) == 0) {
    return(list(message = "No component data available"))
  }
  
  # Find common components
  all_component_names <- unique(unlist(lapply(all_components, names)))
  common_components <- character()
  
  for (comp_name in all_component_names) {
    present_in_all <- all(sapply(all_components, function(x) comp_name %in% names(x)))
    if (present_in_all) {
      common_components <- c(common_components, comp_name)
    }
  }
  
  # Calculate statistics for common components
  component_stats <- list()
  
  for (comp_name in common_components) {
    # Collect all values for this component
    all_values <- unlist(lapply(all_components, function(x) x[[comp_name]]))
    
    # Calculate summary statistics
    component_stats[[comp_name]] <- list(
      mean = mean(all_values, na.rm = TRUE),
      sd = sd(all_values, na.rm = TRUE),
      median = median(all_values, na.rm = TRUE),
      range = range(all_values, na.rm = TRUE),
      n_values = length(all_values),
      n_datasets = length(all_components)
    )
  }
  
  return(list(
    common_components = common_components,
    component_statistics = component_stats,
    n_datasets_with_components = length(all_components)
  ))
}

# Internal function: Generate batch summary report
.generate_batch_summary_report <- function(batch_results, output_dir, format) {
  
  # Create comprehensive summary report
  summary_stats <- batch_results$summary_statistics
  
  html_content <- sprintf('
<!DOCTYPE html>
<html>
<head>
    <title>Batch Processing Summary Report</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 40px; }
        .header { background-color: #f0f8ff; padding: 20px; border-radius: 10px; }
        .section { margin: 20px 0; padding: 15px; background-color: #f9f9f9; border-radius: 5px; }
        .success { color: green; font-weight: bold; }
        .failed { color: red; font-weight: bold; }
        table { border-collapse: collapse; width: 100%%; margin: 10px 0; }
        th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
        th { background-color: #f2f2f2; }
        .metric { display: inline-block; margin: 10px; padding: 10px; background-color: white; border-radius: 5px; border: 1px solid #ddd; }
    </style>
</head>
<body>
    <div class="header">
        <h1>diversityGPT Batch Processing Summary</h1>
        <p><strong>Generated:</strong> %s</p>
        <p><strong>Total Datasets:</strong> %d | <span class="success">Successful: %d</span> | <span class="failed">Failed: %d</span></p>
        <p><strong>Total Processing Time:</strong> %.1f seconds</p>
    </div>
    
    <div class="section">
        <h2>Dataset Summary</h2>
        <div class="metric">
            <strong>Sample Sizes</strong><br>
            Mean: %.1f<br>
            Range: %d - %d
        </div>
        <div class="metric">
            <strong>Transformation Quality</strong><br>
            Mean: %.3f<br>
            Range: %.3f - %.3f
        </div>
        <div class="metric">
            <strong>Processing Speed</strong><br>
            Mean: %.1f sec/dataset<br>
            Range: %.1f - %.1f sec
        </div>
    </div>
    
    <div class="section">
        <h2>Assembly Mechanisms</h2>
        <p><strong>Most Common:</strong> %s</p>
        <table>
            <tr><th>Mechanism</th><th>Frequency</th></tr>
            %s
        </table>
    </div>
    
    <div class="section">
        <h2>Processing Log</h2>
        <table>
            <tr><th>Dataset</th><th>Status</th><th>Time (sec)</th><th>Error</th></tr>
            %s
        </table>
    </div>
    
    <div class="section">
        <h2>Files Generated</h2>
        <p>Individual results and reports have been saved to: <code>%s</code></p>
        <p>Use <code>batch_results.rds</code> to load complete results in R</p>
    </div>
    
</body>
</html>',
    Sys.time(),
    batch_results$batch_metadata$n_datasets,
    summary_stats$n_successful,
    summary_stats$n_failed,
    summary_stats$processing_summary$total_time_seconds,
    summary_stats$dataset_summary$sample_sizes$mean,
    summary_stats$dataset_summary$sample_sizes$range[1],
    summary_stats$dataset_summary$sample_sizes$range[2],
    summary_stats$dataset_summary$transformation_quality$mean,
    summary_stats$dataset_summary$transformation_quality$range[1],
    summary_stats$dataset_summary$transformation_quality$range[2],
    summary_stats$processing_summary$mean_time_per_dataset,
    summary_stats$processing_summary$fastest_dataset,
    summary_stats$processing_summary$slowest_dataset,
    summary_stats$assembly_mechanisms$most_common_mechanism %||% "None",
    .format_mechanism_table(summary_stats$assembly_mechanisms$mechanism_frequency),
    .format_processing_log_table(batch_results$processing_log),
    output_dir
  )
  
  # Write report
  report_path <- file.path(output_dir, "batch_summary_report.html")
  writeLines(html_content, report_path)
  
  return(report_path)
}

# Internal function: Format mechanism frequency table
.format_mechanism_table <- function(mechanism_freq) {
  
  if (length(mechanism_freq) == 0) {
    return("<tr><td colspan='2'>No mechanisms detected</td></tr>")
  }
  
  rows <- character()
  for (i in 1:length(mechanism_freq)) {
    mechanism <- names(mechanism_freq)[i]
    frequency <- mechanism_freq[i]
    rows <- c(rows, sprintf("<tr><td>%s</td><td>%d</td></tr>", mechanism, frequency))
  }
  
  return(paste(rows, collapse = "\n"))
}

# Internal function: Format processing log table
.format_processing_log_table <- function(processing_log) {
  
  if (nrow(processing_log) == 0) {
    return("<tr><td colspan='4'>No processing log available</td></tr>")
  }
  
  rows <- character()
  for (i in 1:nrow(processing_log)) {
    log_entry <- processing_log[i, ]
    status_class <- ifelse(log_entry$status == "success", "success", "failed")
    error_text <- ifelse(log_entry$error_message == "", "-", substr(log_entry$error_message, 1, 50))
    
    row <- sprintf(
      "<tr><td>%s</td><td class='%s'>%s</td><td>%.1f</td><td>%s</td></tr>",
      log_entry$dataset,
      status_class,
      log_entry$status,
      log_entry$elapsed_seconds,
      error_text
    )
    rows <- c(rows, row)
  }
  
  return(paste(rows, collapse = "\n"))
}

#' Load Batch Processing Results
#'
#' Load previously saved batch processing results from disk
#'
#' @param results_dir Directory containing batch results
#' @param results_file Specific results file to load (default: "batch_results.rds")
#'
#' @return Batch processing results object
#' @export
load_batch_results <- function(results_dir, results_file = "batch_results.rds") {
  
  results_path <- file.path(results_dir, results_file)
  
  if (!file.exists(results_path)) {
    stop("Results file not found: ", results_path)
  }
  
  cat("Loading batch results from:", results_path, "\n")
  batch_results <- readRDS(results_path)
  
  if (!inherits(batch_results, "batch_processing")) {
    warning("Loaded object may not be valid batch processing results")
  }
  
  cat("Loaded results for", batch_results$batch_metadata$n_datasets, "datasets\n")
  cat("Successful:", batch_results$summary_statistics$n_successful, "\n")
  cat("Failed:", batch_results$summary_statistics$n_failed, "\n")
  
  return(batch_results)
}

#' Retry Failed Datasets
#'
#' Retry processing of datasets that failed in previous batch run
#'
#' @param batch_results Previous batch processing results
#' @param phyloseq_list Original list of phyloseq objects
#' @param ... Additional arguments passed to batch_process_phyloseq
#'
#' @return Updated batch processing results
#' @export
retry_failed_datasets <- function(batch_results, phyloseq_list, ...) {
  
  if (!inherits(batch_results, "batch_processing")) {
    stop("batch_results must be a batch_processing object")
  }
  
  failed_datasets <- batch_results$failed_datasets
  
  if (length(failed_datasets) == 0) {
    cat("No failed datasets to retry\n")
    return(batch_results)
  }
  
  cat("Retrying", length(failed_datasets), "failed datasets:", paste(failed_datasets, collapse = ", "), "\n")
  
  # Extract failed phyloseq objects
  dataset_names <- batch_results$batch_metadata$dataset_names
  failed_indices <- which(dataset_names %in% failed_datasets)
  failed_phyloseq_list <- phyloseq_list[failed_indices]
  
  # Retry processing
  retry_results <- batch_process_phyloseq(
    phyloseq_list = failed_phyloseq_list,
    dataset_names = failed_datasets,
    ...
  )
  
  # Merge results
  # This is a simplified merge - in practice, would need more sophisticated merging
  for (dataset_name in failed_datasets) {
    if (dataset_name %in% names(retry_results$results)) {
      batch_results$results[[dataset_name]] <- retry_results$results[[dataset_name]]
      
      # Update processing log
      retry_log <- retry_results$processing_log[retry_results$processing_log$dataset == dataset_name, ]
      if (nrow(retry_log) > 0 && retry_log$status == "success") {
        batch_results$failed_datasets <- setdiff(batch_results$failed_datasets, dataset_name)
      }
    }
  }
  
  # Regenerate summary statistics
  batch_results$summary_statistics <- .generate_batch_summary_statistics(batch_results)
  
  return(batch_results)
}

# S3 method for printing batch processing results
#' @export
print.batch_processing <- function(x, ...) {
  cat("diversityGPT Batch Processing Results\n")
  cat("====================================\n\n")
  
  cat("Processing Summary:\n")
  cat("Total datasets:", x$batch_metadata$n_datasets, "\n")
  cat("Successful:", x$summary_statistics$n_successful, "\n")
  cat("Failed:", x$summary_statistics$n_failed, "\n")
  cat("Success rate:", round(x$summary_statistics$n_successful / x$batch_metadata$n_datasets * 100, 1), "%\n\n")
  
  cat("Timing:\n")
  cat("Total time:", round(x$batch_metadata$total_elapsed, 1), "seconds\n")
  cat("Mean time per dataset:", round(x$summary_statistics$processing_summary$mean_time_per_dataset, 1), "seconds\n\n")
  
  if (x$summary_statistics$n_successful > 0) {
    cat("Dataset Characteristics:\n")
    cat("Mean sample size:", round(x$summary_statistics$dataset_summary$sample_sizes$mean, 1), "\n")
    cat("Mean transformation quality:", round(x$summary_statistics$dataset_summary$transformation_quality$mean, 3), "\n\n")
  }
  
  if (!is.null(x$summary_statistics$assembly_mechanisms$most_common_mechanism)) {
    cat("Most common assembly mechanism:", x$summary_statistics$assembly_mechanisms$most_common_mechanism, "\n")
  }
  
  if (length(x$failed_datasets) > 0) {
    cat("\nFailed datasets:", paste(x$failed_datasets, collapse = ", "), "\n")
  }
  
  cat("\nResults directory:", x$batch_metadata$output_dir, "\n")
  
  invisible(x)
}