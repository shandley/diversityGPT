#' diversityGPT API Framework
#'
#' Provides a clean, consistent API layer for programmatic access to all
#' diversityGPT functionality with standardized inputs, outputs, and error handling.

#' diversityGPT API Client
#'
#' Main entry point for programmatic access to diversityGPT functionality.
#' Provides a unified interface with consistent error handling and validation.
#'
#' @param operation The API operation to perform
#' @param ... Arguments passed to the specific operation
#' @param config Optional configuration list
#' @param async Whether to run operation asynchronously
#' @param timeout Maximum time for operation (seconds)
#' @param retry Number of retry attempts for failed operations
#' @param verbose Whether to print progress messages
#'
#' @return An api_response object with standardized structure
#'
#' @examples
#' \dontrun{
#' # Simple universal analysis
#' result <- diversityGPT_api(
#'   "analyze",
#'   phyloseq_obj = my_data,
#'   components = c("universal", "mechanisms")
#' )
#' 
#' # Batch processing with configuration
#' config <- create_api_config(
#'   cache = TRUE,
#'   parallel = TRUE,
#'   llm_provider = "anthropic"
#' )
#' 
#' result <- diversityGPT_api(
#'   "batch_analyze",
#'   dataset_list = my_datasets,
#'   config = config,
#'   async = TRUE
#' )
#' 
#' # Get status of async operation
#' status <- diversityGPT_api("status", job_id = result$job_id)
#' }
#'
#' @export
diversityGPT_api <- function(operation,
                            ...,
                            config = NULL,
                            async = FALSE,
                            timeout = 300,
                            retry = 3,
                            verbose = TRUE) {
  
  # Validate operation
  valid_operations <- c(
    "analyze", "transform", "validate", "batch_analyze",
    "meta_analyze", "generate_report", "search_literature",
    "detect_mechanisms", "generate_hypotheses", "status",
    "cancel", "list_jobs", "get_results", "clean_cache"
  )
  
  if (!operation %in% valid_operations) {
    return(.create_error_response(
      paste("Invalid operation:", operation),
      "INVALID_OPERATION",
      list(valid_operations = valid_operations)
    ))
  }
  
  # Load configuration
  config <- .merge_api_config(config)
  
  # Set up logging if verbose
  if (verbose) {
    .api_log("Starting operation:", operation)
  }
  
  # Handle async operations
  if (async && operation %in% c("analyze", "batch_analyze", "meta_analyze")) {
    return(.handle_async_operation(operation, list(...), config, timeout, verbose))
  }
  
  # Route to appropriate handler
  result <- tryCatch({
    switch(operation,
      analyze = .api_analyze(..., config = config, verbose = verbose),
      transform = .api_transform(..., config = config, verbose = verbose),
      validate = .api_validate(..., config = config, verbose = verbose),
      batch_analyze = .api_batch_analyze(..., config = config, verbose = verbose),
      meta_analyze = .api_meta_analyze(..., config = config, verbose = verbose),
      generate_report = .api_generate_report(..., config = config, verbose = verbose),
      search_literature = .api_search_literature(..., config = config, verbose = verbose),
      detect_mechanisms = .api_detect_mechanisms(..., config = config, verbose = verbose),
      generate_hypotheses = .api_generate_hypotheses(..., config = config, verbose = verbose),
      status = .api_job_status(..., verbose = verbose),
      cancel = .api_cancel_job(..., verbose = verbose),
      list_jobs = .api_list_jobs(..., verbose = verbose),
      get_results = .api_get_results(..., verbose = verbose),
      clean_cache = .api_clean_cache(..., config = config, verbose = verbose)
    )
  }, error = function(e) {
    # Retry logic
    if (retry > 0 && !operation %in% c("status", "cancel", "list_jobs")) {
      if (verbose) {
        .api_log("Operation failed, retrying...", paste("Attempts left:", retry))
      }
      Sys.sleep(1)  # Brief pause before retry
      return(diversityGPT_api(operation, ..., config = config, 
                             async = async, timeout = timeout, 
                             retry = retry - 1, verbose = verbose))
    }
    
    return(.create_error_response(
      e$message,
      "OPERATION_FAILED",
      list(operation = operation, call = sys.call())
    ))
  })
  
  # Ensure consistent response format
  if (!inherits(result, "api_response")) {
    result <- .wrap_api_response(result, operation)
  }
  
  if (verbose) {
    .api_log("Operation completed:", operation, 
             paste("Status:", result$status))
  }
  
  return(result)
}

#' Create API Configuration
#'
#' Creates a configuration object for diversityGPT API operations
#'
#' @param cache Whether to enable caching
#' @param cache_dir Directory for cache storage
#' @param parallel Whether to enable parallel processing
#' @param n_cores Number of cores for parallel processing
#' @param llm_provider LLM provider: "anthropic", "openai", "none"
#' @param llm_model Specific model to use
#' @param api_key API key for LLM provider
#' @param output_dir Directory for output files
#' @param temp_dir Directory for temporary files
#' @param log_file Path to log file
#' @param memory_limit Maximum memory usage (MB)
#'
#' @return Configuration list
#' @export
create_api_config <- function(cache = TRUE,
                             cache_dir = NULL,
                             parallel = TRUE,
                             n_cores = NULL,
                             llm_provider = "none",
                             llm_model = NULL,
                             api_key = NULL,
                             output_dir = "diversityGPT_output",
                             temp_dir = tempdir(),
                             log_file = NULL,
                             memory_limit = 4096) {
  
  config <- list(
    cache = list(
      enabled = cache,
      directory = cache_dir %||% file.path(temp_dir, "diversityGPT_cache")
    ),
    parallel = list(
      enabled = parallel,
      n_cores = n_cores %||% max(1, parallel::detectCores() - 1)
    ),
    llm = list(
      provider = llm_provider,
      model = llm_model,
      api_key = api_key %||% Sys.getenv(paste0(toupper(llm_provider), "_API_KEY"))
    ),
    paths = list(
      output = output_dir,
      temp = temp_dir,
      log = log_file
    ),
    resources = list(
      memory_limit = memory_limit
    ),
    metadata = list(
      created = Sys.time(),
      version = packageVersion("diversityGPT")
    )
  )
  
  class(config) <- c("api_config", "list")
  return(config)
}

# API operation handlers

.api_analyze <- function(phyloseq_obj,
                        components = c("universal", "mechanisms", "hypotheses"),
                        config = NULL,
                        verbose = TRUE) {
  
  # Validate inputs
  if (!inherits(phyloseq_obj, "phyloseq")) {
    return(.create_error_response(
      "Input must be a phyloseq object",
      "INVALID_INPUT",
      list(provided_class = class(phyloseq_obj))
    ))
  }
  
  valid_components <- c("universal", "mechanisms", "hypotheses", 
                       "literature", "validation")
  invalid_components <- setdiff(components, valid_components)
  
  if (length(invalid_components) > 0) {
    return(.create_error_response(
      paste("Invalid components:", paste(invalid_components, collapse = ", ")),
      "INVALID_COMPONENT",
      list(valid_components = valid_components)
    ))
  }
  
  # Initialize results
  results <- list()
  start_time <- Sys.time()
  
  # Universal information extraction
  if ("universal" %in% components) {
    if (verbose) .api_log("Extracting universal information...")
    
    results$universal <- tryCatch({
      if (config$cache$enabled) {
        cached_universal_analysis(phyloseq_obj, cache_dir = config$cache$directory)
      } else {
        extract_universal_information(phyloseq_obj)
      }
    }, error = function(e) {
      list(error = e$message)
    })
  }
  
  # Assembly mechanisms
  if ("mechanisms" %in% components && !is.null(results$universal)) {
    if (verbose) .api_log("Detecting assembly mechanisms...")
    
    results$mechanisms <- tryCatch({
      detect_assembly_mechanisms(results$universal)
    }, error = function(e) {
      list(error = e$message)
    })
  }
  
  # Ecological hypotheses
  if ("hypotheses" %in% components && !is.null(results$universal)) {
    if (verbose) .api_log("Generating ecological hypotheses...")
    
    results$hypotheses <- tryCatch({
      generate_ecological_hypotheses(
        results$universal,
        results$mechanisms,
        llm_provider = config$llm$provider
      )
    }, error = function(e) {
      list(error = e$message)
    })
  }
  
  # Literature search
  if ("literature" %in% components && !is.null(results$universal)) {
    if (verbose) .api_log("Searching scientific literature...")
    
    results$literature <- tryCatch({
      search_literature(
        results$universal,
        results$mechanisms,
        results$hypotheses,
        max_papers = 10,
        llm_synthesis = (config$llm$provider != "none")
      )
    }, error = function(e) {
      list(error = e$message)
    })
  }
  
  # Statistical validation
  if ("validation" %in% components && !is.null(results$universal)) {
    if (verbose) .api_log("Performing statistical validation...")
    
    results$validation <- tryCatch({
      validate_universal_analysis(
        results$universal,
        phyloseq_obj,
        bootstrap_n = 100  # Reduced for API speed
      )
    }, error = function(e) {
      list(error = e$message)
    })
  }
  
  # Create response
  end_time <- Sys.time()
  elapsed_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Check for errors
  errors <- lapply(results, function(x) {
    if (is.list(x) && "error" %in% names(x)) x$error else NULL
  })
  errors <- errors[!sapply(errors, is.null)]
  
  if (length(errors) > 0) {
    return(.create_api_response(
      status = "partial_success",
      data = results,
      metadata = list(
        operation = "analyze",
        components_requested = components,
        components_completed = setdiff(names(results), names(errors)),
        errors = errors,
        elapsed_time = elapsed_time
      ),
      warnings = paste("Some components failed:", paste(names(errors), collapse = ", "))
    ))
  }
  
  return(.create_api_response(
    status = "success",
    data = results,
    metadata = list(
      operation = "analyze",
      components = components,
      elapsed_time = elapsed_time,
      n_samples = phyloseq::nsamples(phyloseq_obj),
      n_taxa = phyloseq::ntaxa(phyloseq_obj)
    )
  ))
}

.api_transform <- function(source_metrics,
                          target_metrics,
                          transformation_matrix = NULL,
                          phyloseq_reference = NULL,
                          config = NULL,
                          verbose = TRUE) {
  
  # Validate inputs
  if (!is.data.frame(source_metrics) && !is.list(source_metrics)) {
    return(.create_error_response(
      "source_metrics must be a data frame or named list",
      "INVALID_INPUT"
    ))
  }
  
  if (!is.character(target_metrics)) {
    return(.create_error_response(
      "target_metrics must be a character vector",
      "INVALID_INPUT"
    ))
  }
  
  # Get transformation matrix if not provided
  if (is.null(transformation_matrix)) {
    if (is.null(phyloseq_reference)) {
      return(.create_error_response(
        "Either transformation_matrix or phyloseq_reference must be provided",
        "MISSING_PARAMETER"
      ))
    }
    
    if (verbose) .api_log("Extracting transformation matrix from reference...")
    
    universal_info <- extract_universal_information(phyloseq_reference)
    transformation_matrix <- universal_info$transformation_matrix
  }
  
  # Perform transformation
  if (verbose) .api_log("Performing metric transformation...")
  
  result <- tryCatch({
    universal_diversity_transform(
      source_metrics,
      target_metrics,
      transformation_matrix
    )
  }, error = function(e) {
    return(.create_error_response(
      e$message,
      "TRANSFORMATION_FAILED"
    ))
  })
  
  return(.create_api_response(
    status = "success",
    data = result,
    metadata = list(
      operation = "transform",
      source_metrics = names(source_metrics),
      target_metrics = target_metrics,
      n_transformations = length(target_metrics)
    )
  ))
}

.api_batch_analyze <- function(dataset_list,
                              dataset_names = NULL,
                              analysis_steps = c("universal", "mechanisms"),
                              config = NULL,
                              verbose = TRUE) {
  
  # Validate inputs
  if (!is.list(dataset_list)) {
    return(.create_error_response(
      "dataset_list must be a list",
      "INVALID_INPUT"
    ))
  }
  
  # Run batch processing
  if (verbose) .api_log("Starting batch analysis for", length(dataset_list), "datasets...")
  
  result <- tryCatch({
    batch_process_phyloseq(
      phyloseq_list = dataset_list,
      dataset_names = dataset_names,
      analysis_steps = analysis_steps,
      output_dir = config$paths$output,
      parallel_cores = if (config$parallel$enabled) config$parallel$n_cores else 1,
      cache_results = config$cache$enabled,
      llm_provider = config$llm$provider
    )
  }, error = function(e) {
    return(.create_error_response(
      e$message,
      "BATCH_PROCESSING_FAILED"
    ))
  })
  
  return(.create_api_response(
    status = "success",
    data = result,
    metadata = list(
      operation = "batch_analyze",
      n_datasets = length(dataset_list),
      analysis_steps = analysis_steps
    )
  ))
}

.api_meta_analyze <- function(study_list,
                             study_names = NULL,
                             meta_method = "random_effects",
                             config = NULL,
                             verbose = TRUE) {
  
  # Validate inputs
  if (!is.list(study_list)) {
    return(.create_error_response(
      "study_list must be a list",
      "INVALID_INPUT"
    ))
  }
  
  if (length(study_list) < 2) {
    return(.create_error_response(
      "At least 2 studies required for meta-analysis",
      "INSUFFICIENT_DATA"
    ))
  }
  
  # Run meta-analysis
  if (verbose) .api_log("Performing meta-analysis on", length(study_list), "studies...")
  
  result <- tryCatch({
    cross_study_meta_analysis(
      study_list = study_list,
      study_names = study_names,
      meta_analysis_method = meta_method,
      include_forest_plots = TRUE,
      include_heterogeneity = TRUE
    )
  }, error = function(e) {
    return(.create_error_response(
      e$message,
      "META_ANALYSIS_FAILED"
    ))
  })
  
  return(.create_api_response(
    status = "success",
    data = result,
    metadata = list(
      operation = "meta_analyze",
      n_studies = length(study_list),
      method = meta_method
    )
  ))
}

.api_generate_report <- function(analysis_results,
                                phyloseq_obj = NULL,
                                output_format = "html",
                                template = "research",
                                config = NULL,
                                verbose = TRUE) {
  
  # Validate inputs
  if (!is.list(analysis_results)) {
    return(.create_error_response(
      "analysis_results must be a list",
      "INVALID_INPUT"
    ))
  }
  
  # Generate report
  if (verbose) .api_log("Generating", output_format, "report...")
  
  output_file <- file.path(config$paths$output, 
                          paste0("diversityGPT_report_", 
                                format(Sys.time(), "%Y%m%d_%H%M%S")))
  
  result <- tryCatch({
    report_path <- generate_diversity_report(
      universal_info = analysis_results$universal,
      phyloseq_obj = phyloseq_obj,
      assembly_mechanisms = analysis_results$mechanisms,
      hypotheses = analysis_results$hypotheses,
      literature = analysis_results$literature,
      validation = analysis_results$validation,
      output_file = output_file,
      output_format = output_format,
      template = template
    )
    
    list(report_path = report_path)
  }, error = function(e) {
    return(.create_error_response(
      e$message,
      "REPORT_GENERATION_FAILED"
    ))
  })
  
  return(.create_api_response(
    status = "success",
    data = result,
    metadata = list(
      operation = "generate_report",
      format = output_format,
      template = template
    )
  ))
}

# Async operation handling

.handle_async_operation <- function(operation, args, config, timeout, verbose) {
  
  # Generate unique job ID
  job_id <- paste0("job_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", 
                   sample(1000:9999, 1))
  
  # Create job directory
  job_dir <- file.path(config$paths$temp, "diversityGPT_jobs", job_id)
  dir.create(job_dir, recursive = TRUE)
  
  # Save job info
  job_info <- list(
    job_id = job_id,
    operation = operation,
    args = args,
    config = config,
    status = "running",
    created = Sys.time(),
    timeout = timeout
  )
  
  saveRDS(job_info, file.path(job_dir, "job_info.rds"))
  
  # Launch background process
  if (verbose) .api_log("Launching async job:", job_id)
  
  # Use callr for background execution
  if (requireNamespace("callr", quietly = TRUE)) {
    process <- callr::r_bg(
      function(operation, args, config, job_dir) {
        library(diversityGPT)
        
        # Run operation
        result <- do.call(paste0(".api_", operation), 
                         c(args, list(config = config, verbose = FALSE)))
        
        # Save result
        saveRDS(result, file.path(job_dir, "result.rds"))
        
        # Update job info
        job_info <- readRDS(file.path(job_dir, "job_info.rds"))
        job_info$status <- "completed"
        job_info$completed = Sys.time()
        saveRDS(job_info, file.path(job_dir, "job_info.rds"))
      },
      args = list(operation = operation, args = args, 
                 config = config, job_dir = job_dir)
    )
    
    # Save process handle
    saveRDS(process, file.path(job_dir, "process.rds"))
  } else {
    warning("Package 'callr' not available for async operations")
    # Fallback to synchronous execution
    return(do.call(paste0(".api_", operation), 
                  c(args, list(config = config, verbose = verbose))))
  }
  
  return(.create_api_response(
    status = "accepted",
    data = list(job_id = job_id),
    metadata = list(
      operation = operation,
      async = TRUE,
      status_url = paste0("diversityGPT_api('status', job_id = '", job_id, "')")
    ),
    message = "Job submitted successfully. Use status operation to check progress."
  ))
}

.api_job_status <- function(job_id, verbose = TRUE) {
  
  # Find job directory
  job_dir <- file.path(tempdir(), "diversityGPT_jobs", job_id)
  
  if (!dir.exists(job_dir)) {
    return(.create_error_response(
      paste("Job not found:", job_id),
      "JOB_NOT_FOUND"
    ))
  }
  
  # Load job info
  job_info <- readRDS(file.path(job_dir, "job_info.rds"))
  
  # Check if process is still running
  if (job_info$status == "running" && file.exists(file.path(job_dir, "process.rds"))) {
    process <- readRDS(file.path(job_dir, "process.rds"))
    
    if (requireNamespace("callr", quietly = TRUE)) {
      if (process$is_alive()) {
        elapsed <- as.numeric(difftime(Sys.time(), job_info$created, units = "secs"))
        
        # Check timeout
        if (elapsed > job_info$timeout) {
          process$kill()
          job_info$status <- "timeout"
          job_info$completed <- Sys.time()
          saveRDS(job_info, file.path(job_dir, "job_info.rds"))
        }
      } else {
        # Process finished
        if (file.exists(file.path(job_dir, "result.rds"))) {
          job_info$status <- "completed"
        } else {
          job_info$status <- "failed"
        }
        job_info$completed <- Sys.time()
        saveRDS(job_info, file.path(job_dir, "job_info.rds"))
      }
    }
  }
  
  # Prepare response data
  response_data <- list(
    job_id = job_id,
    status = job_info$status,
    operation = job_info$operation,
    created = job_info$created
  )
  
  if (job_info$status %in% c("completed", "failed", "timeout")) {
    response_data$completed <- job_info$completed
    response_data$elapsed_time <- as.numeric(
      difftime(job_info$completed, job_info$created, units = "secs")
    )
  }
  
  return(.create_api_response(
    status = "success",
    data = response_data,
    metadata = list(
      operation = "status"
    )
  ))
}

.api_get_results <- function(job_id, clean = TRUE, verbose = TRUE) {
  
  # Find job directory
  job_dir <- file.path(tempdir(), "diversityGPT_jobs", job_id)
  
  if (!dir.exists(job_dir)) {
    return(.create_error_response(
      paste("Job not found:", job_id),
      "JOB_NOT_FOUND"
    ))
  }
  
  # Check if results exist
  result_file <- file.path(job_dir, "result.rds")
  
  if (!file.exists(result_file)) {
    # Check job status
    job_info <- readRDS(file.path(job_dir, "job_info.rds"))
    
    if (job_info$status == "running") {
      return(.create_error_response(
        "Job is still running",
        "JOB_IN_PROGRESS",
        list(job_id = job_id, status = "running")
      ))
    } else {
      return(.create_error_response(
        paste("No results available. Job status:", job_info$status),
        "NO_RESULTS",
        list(job_id = job_id, status = job_info$status)
      ))
    }
  }
  
  # Load results
  results <- readRDS(result_file)
  
  # Clean up if requested
  if (clean) {
    unlink(job_dir, recursive = TRUE)
    if (verbose) .api_log("Job directory cleaned:", job_id)
  }
  
  return(results)
}

# Utility functions

.create_api_response <- function(status = "success",
                                data = NULL,
                                metadata = NULL,
                                message = NULL,
                                warnings = NULL) {
  
  response <- list(
    status = status,
    data = data,
    metadata = c(
      list(
        timestamp = Sys.time(),
        version = packageVersion("diversityGPT")
      ),
      metadata
    )
  )
  
  if (!is.null(message)) response$message <- message
  if (!is.null(warnings)) response$warnings <- warnings
  
  class(response) <- c("api_response", "list")
  return(response)
}

.create_error_response <- function(message, code = "ERROR", details = NULL) {
  
  response <- list(
    status = "error",
    error = list(
      message = message,
      code = code,
      details = details
    ),
    metadata = list(
      timestamp = Sys.time(),
      version = packageVersion("diversityGPT")
    )
  )
  
  class(response) <- c("api_response", "list")
  return(response)
}

.wrap_api_response <- function(result, operation) {
  
  if (inherits(result, "api_response")) {
    return(result)
  }
  
  return(.create_api_response(
    status = "success",
    data = result,
    metadata = list(operation = operation)
  ))
}

.merge_api_config <- function(config) {
  
  default_config <- create_api_config()
  
  if (is.null(config)) {
    return(default_config)
  }
  
  # Merge configurations
  for (section in names(default_config)) {
    if (section %in% names(config)) {
      if (is.list(default_config[[section]])) {
        for (key in names(default_config[[section]])) {
          if (key %in% names(config[[section]])) {
            default_config[[section]][[key]] <- config[[section]][[key]]
          }
        }
      } else {
        default_config[[section]] <- config[[section]]
      }
    }
  }
  
  return(default_config)
}

.api_log <- function(...) {
  cat("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ..., "\n", sep = "")
}

.api_clean_cache <- function(age_days = 7, config = NULL, verbose = TRUE) {
  
  cache_dir <- config$cache$directory
  
  if (!dir.exists(cache_dir)) {
    return(.create_api_response(
      status = "success",
      data = list(files_removed = 0),
      metadata = list(operation = "clean_cache"),
      message = "No cache directory found"
    ))
  }
  
  # Find old cache files
  cache_files <- list.files(cache_dir, full.names = TRUE, recursive = TRUE)
  file_info <- file.info(cache_files)
  
  cutoff_time <- Sys.time() - (age_days * 24 * 60 * 60)
  old_files <- cache_files[file_info$mtime < cutoff_time]
  
  if (length(old_files) > 0) {
    if (verbose) .api_log("Removing", length(old_files), "old cache files...")
    unlink(old_files)
  }
  
  return(.create_api_response(
    status = "success",
    data = list(
      files_removed = length(old_files),
      space_freed_mb = sum(file_info$size[file_info$mtime < cutoff_time]) / 1024 / 1024
    ),
    metadata = list(
      operation = "clean_cache",
      age_days = age_days
    )
  ))
}

.api_list_jobs <- function(status_filter = NULL, verbose = TRUE) {
  
  jobs_dir <- file.path(tempdir(), "diversityGPT_jobs")
  
  if (!dir.exists(jobs_dir)) {
    return(.create_api_response(
      status = "success",
      data = list(jobs = list()),
      metadata = list(operation = "list_jobs"),
      message = "No jobs found"
    ))
  }
  
  # Find all job directories
  job_dirs <- list.dirs(jobs_dir, full.names = TRUE, recursive = FALSE)
  
  jobs <- lapply(job_dirs, function(job_dir) {
    job_info_file <- file.path(job_dir, "job_info.rds")
    if (file.exists(job_info_file)) {
      job_info <- readRDS(job_info_file)
      
      # Filter by status if requested
      if (!is.null(status_filter) && job_info$status != status_filter) {
        return(NULL)
      }
      
      return(list(
        job_id = job_info$job_id,
        operation = job_info$operation,
        status = job_info$status,
        created = job_info$created,
        completed = job_info$completed %||% NA
      ))
    }
    return(NULL)
  })
  
  # Remove NULL entries
  jobs <- jobs[!sapply(jobs, is.null)]
  
  return(.create_api_response(
    status = "success",
    data = list(jobs = jobs),
    metadata = list(
      operation = "list_jobs",
      n_jobs = length(jobs),
      status_filter = status_filter
    )
  ))
}

.api_cancel_job <- function(job_id, verbose = TRUE) {
  
  # Find job directory
  job_dir <- file.path(tempdir(), "diversityGPT_jobs", job_id)
  
  if (!dir.exists(job_dir)) {
    return(.create_error_response(
      paste("Job not found:", job_id),
      "JOB_NOT_FOUND"
    ))
  }
  
  # Load job info
  job_info <- readRDS(file.path(job_dir, "job_info.rds"))
  
  if (job_info$status != "running") {
    return(.create_error_response(
      paste("Cannot cancel job with status:", job_info$status),
      "INVALID_JOB_STATE",
      list(job_id = job_id, status = job_info$status)
    ))
  }
  
  # Kill process if running
  if (file.exists(file.path(job_dir, "process.rds"))) {
    process <- readRDS(file.path(job_dir, "process.rds"))
    
    if (requireNamespace("callr", quietly = TRUE) && process$is_alive()) {
      process$kill()
      if (verbose) .api_log("Process killed for job:", job_id)
    }
  }
  
  # Update job status
  job_info$status <- "cancelled"
  job_info$completed <- Sys.time()
  saveRDS(job_info, file.path(job_dir, "job_info.rds"))
  
  return(.create_api_response(
    status = "success",
    data = list(
      job_id = job_id,
      status = "cancelled"
    ),
    metadata = list(operation = "cancel"),
    message = paste("Job", job_id, "cancelled successfully")
  ))
}

.api_search_literature <- function(universal_info = NULL,
                                  query = NULL,
                                  databases = c("biorxiv"),
                                  max_papers = 10,
                                  config = NULL,
                                  verbose = TRUE) {
  
  # Build search query
  if (is.null(query) && is.null(universal_info)) {
    return(.create_error_response(
      "Either universal_info or query must be provided",
      "MISSING_PARAMETER"
    ))
  }
  
  if (verbose) .api_log("Searching literature...")
  
  result <- tryCatch({
    search_literature(
      universal_info = universal_info,
      assembly_mechanisms = NULL,
      hypotheses = NULL,
      custom_query = query,
      databases = databases,
      max_papers = max_papers,
      llm_synthesis = (config$llm$provider != "none")
    )
  }, error = function(e) {
    return(.create_error_response(
      e$message,
      "LITERATURE_SEARCH_FAILED"
    ))
  })
  
  return(.create_api_response(
    status = "success",
    data = result,
    metadata = list(
      operation = "search_literature",
      databases = databases,
      max_papers = max_papers
    )
  ))
}

.api_detect_mechanisms <- function(universal_info,
                                  environmental_data = NULL,
                                  config = NULL,
                                  verbose = TRUE) {
  
  # Validate inputs
  if (!inherits(universal_info, "universal_information")) {
    return(.create_error_response(
      "universal_info must be a universal_information object",
      "INVALID_INPUT"
    ))
  }
  
  if (verbose) .api_log("Detecting assembly mechanisms...")
  
  result <- tryCatch({
    detect_assembly_mechanisms(
      universal_info,
      environmental_data = environmental_data
    )
  }, error = function(e) {
    return(.create_error_response(
      e$message,
      "MECHANISM_DETECTION_FAILED"
    ))
  })
  
  return(.create_api_response(
    status = "success",
    data = result,
    metadata = list(
      operation = "detect_mechanisms"
    )
  ))
}

.api_generate_hypotheses <- function(universal_info,
                                   assembly_mechanisms = NULL,
                                   study_context = NULL,
                                   config = NULL,
                                   verbose = TRUE) {
  
  # Validate inputs
  if (!inherits(universal_info, "universal_information")) {
    return(.create_error_response(
      "universal_info must be a universal_information object",
      "INVALID_INPUT"
    ))
  }
  
  if (verbose) .api_log("Generating ecological hypotheses...")
  
  result <- tryCatch({
    generate_ecological_hypotheses(
      universal_info,
      assembly_mechanisms = assembly_mechanisms,
      study_context = study_context,
      llm_provider = config$llm$provider
    )
  }, error = function(e) {
    return(.create_error_response(
      e$message,
      "HYPOTHESIS_GENERATION_FAILED"
    ))
  })
  
  return(.create_api_response(
    status = "success",
    data = result,
    metadata = list(
      operation = "generate_hypotheses"
    )
  ))
}

.api_validate <- function(universal_info,
                         phyloseq_obj,
                         validation_type = "full",
                         config = NULL,
                         verbose = TRUE) {
  
  # Validate inputs
  if (!inherits(universal_info, "universal_information")) {
    return(.create_error_response(
      "universal_info must be a universal_information object",
      "INVALID_INPUT"
    ))
  }
  
  if (!inherits(phyloseq_obj, "phyloseq")) {
    return(.create_error_response(
      "phyloseq_obj must be a phyloseq object",
      "INVALID_INPUT"
    ))
  }
  
  if (verbose) .api_log("Running", validation_type, "validation...")
  
  result <- tryCatch({
    if (validation_type == "full") {
      validate_universal_analysis(
        universal_info,
        phyloseq_obj,
        bootstrap_n = 100,
        cv_folds = 5
      )
    } else if (validation_type == "quick") {
      validate_universal_analysis(
        universal_info,
        phyloseq_obj,
        bootstrap_n = 10,
        cv_folds = 2,
        validate_transformations = FALSE
      )
    } else {
      return(.create_error_response(
        paste("Unknown validation type:", validation_type),
        "INVALID_PARAMETER"
      ))
    }
  }, error = function(e) {
    return(.create_error_response(
      e$message,
      "VALIDATION_FAILED"
    ))
  })
  
  return(.create_api_response(
    status = "success",
    data = result,
    metadata = list(
      operation = "validate",
      validation_type = validation_type
    )
  ))
}

# S3 methods for api_response class

#' @export
print.api_response <- function(x, ...) {
  cat("diversityGPT API Response\n")
  cat("========================\n")
  cat("Status:", x$status, "\n")
  
  if (x$status == "error") {
    cat("Error Code:", x$error$code, "\n")
    cat("Error Message:", x$error$message, "\n")
    if (!is.null(x$error$details)) {
      cat("Details:\n")
      print(x$error$details)
    }
  } else {
    if (!is.null(x$message)) {
      cat("Message:", x$message, "\n")
    }
    
    if (!is.null(x$warnings)) {
      cat("Warnings:", x$warnings, "\n")
    }
    
    cat("\nMetadata:\n")
    cat("- Operation:", x$metadata$operation %||% "unknown", "\n")
    cat("- Timestamp:", as.character(x$metadata$timestamp), "\n")
    cat("- Version:", as.character(x$metadata$version), "\n")
    
    if (!is.null(x$data)) {
      cat("\nData Summary:\n")
      if (is.list(x$data)) {
        cat("- Type: list with", length(x$data), "elements\n")
        cat("- Elements:", paste(names(x$data), collapse = ", "), "\n")
      } else {
        cat("- Type:", class(x$data)[1], "\n")
      }
    }
  }
  
  invisible(x)
}

#' @export
summary.api_response <- function(object, ...) {
  summary_list <- list(
    status = object$status,
    operation = object$metadata$operation %||% "unknown",
    timestamp = object$metadata$timestamp
  )
  
  if (object$status == "error") {
    summary_list$error_code <- object$error$code
    summary_list$error_message <- object$error$message
  } else {
    summary_list$data_type <- class(object$data)[1]
    if (is.list(object$data)) {
      summary_list$data_elements <- names(object$data)
    }
  }
  
  class(summary_list) <- c("api_response_summary", "list")
  return(summary_list)
}

#' Extract Data from API Response
#'
#' @param x API response object
#' @param component Optional component to extract
#' @param ... Additional arguments
#'
#' @return Extracted data
#' @export
get_api_data <- function(x, component = NULL, ...) {
  if (!inherits(x, "api_response")) {
    stop("Input must be an api_response object")
  }
  
  if (x$status == "error") {
    stop("Cannot extract data from error response: ", x$error$message)
  }
  
  if (is.null(component)) {
    return(x$data)
  } else {
    if (!component %in% names(x$data)) {
      stop("Component '", component, "' not found in response data")
    }
    return(x$data[[component]])
  }
}

#' Check API Response Status
#'
#' @param x API response object
#'
#' @return Logical indicating success
#' @export
is_api_success <- function(x) {
  inherits(x, "api_response") && x$status %in% c("success", "partial_success")
}

#' Get API Error Details
#'
#' @param x API response object
#'
#' @return Error details or NULL
#' @export
get_api_error <- function(x) {
  if (!inherits(x, "api_response") || x$status != "error") {
    return(NULL)
  }
  return(x$error)
}