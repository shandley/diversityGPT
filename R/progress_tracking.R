#' Progress Tracking System for diversityGPT
#'
#' Functions to track progress of long-running operations with
#' intelligent time estimation and user feedback
#'

#' Create a progress tracker
#'
#' @param total Total number of steps
#' @param title Title for the progress bar
#' @param show_time Whether to show time estimates (default: TRUE)
#' @param show_percent Whether to show percentage (default: TRUE)
#' @param update_interval Minimum interval between updates in seconds (default: 0.1)
#'
#' @return Progress tracker object
#' @export
#' @examples
#' \dontrun{
#' # Create progress tracker
#' pb <- create_progress_tracker(100, "Processing data")
#' 
#' for (i in 1:100) {
#'   # Do work
#'   Sys.sleep(0.1)
#'   
#'   # Update progress
#'   update_progress(pb, i)
#' }
#' 
#' finish_progress(pb)
#' }
create_progress_tracker <- function(total, 
                                   title = "Progress",
                                   show_time = TRUE,
                                   show_percent = TRUE,
                                   update_interval = 0.1) {
  
  # Check if we're in a Shiny environment
  in_shiny <- exists(".shinyMethods", envir = .GlobalEnv) || 
              !is.null(getOption("shiny.inprogress"))
  
  # Use appropriate progress system
  if (in_shiny) {
    tracker <- create_shiny_progress(total, title, show_time, show_percent)
  } else {
    tracker <- create_console_progress(total, title, show_time, show_percent, update_interval)
  }
  
  return(tracker)
}

#' Create console-based progress tracker
#' @keywords internal
create_console_progress <- function(total, title, show_time, show_percent, update_interval) {
  
  tracker <- list(
    type = "console",
    total = total,
    current = 0,
    title = title,
    show_time = show_time,
    show_percent = show_percent,
    update_interval = update_interval,
    start_time = Sys.time(),
    last_update = 0,
    last_message = "",
    finished = FALSE
  )
  
  class(tracker) <- "progress_tracker"
  
  # Print initial message
  cat(paste0(title, "...\n"))
  
  return(tracker)
}

#' Create Shiny-based progress tracker
#' @keywords internal
create_shiny_progress <- function(total, title, show_time, show_percent) {
  
  # Create Shiny progress object
  if (requireNamespace("shiny", quietly = TRUE)) {
    shiny_progress <- shiny::Progress$new()
    shiny_progress$set(message = title, value = 0)
  } else {
    # Fallback to console if Shiny not available
    return(create_console_progress(total, title, show_time, show_percent, 0.1))
  }
  
  tracker <- list(
    type = "shiny",
    total = total,
    current = 0,
    title = title,
    show_time = show_time,
    show_percent = show_percent,
    start_time = Sys.time(),
    shiny_progress = shiny_progress,
    finished = FALSE
  )
  
  class(tracker) <- "progress_tracker"
  
  return(tracker)
}

#' Update progress tracker
#'
#' @param tracker Progress tracker object
#' @param current Current step number
#' @param message Optional message to display
#'
#' @export
update_progress <- function(tracker, current, message = NULL) {
  
  if (!inherits(tracker, "progress_tracker") || tracker$finished) {
    return(invisible(tracker))
  }
  
  tracker$current <- current
  
  if (tracker$type == "console") {
    update_console_progress(tracker, message)
  } else if (tracker$type == "shiny") {
    update_shiny_progress(tracker, message)
  }
  
  return(invisible(tracker))
}

#' Update console progress
#' @keywords internal
update_console_progress <- function(tracker, message) {
  
  current_time <- as.numeric(Sys.time())
  
  # Check if enough time has passed to update
  if (current_time - tracker$last_update < tracker$update_interval && 
      tracker$current < tracker$total) {
    return(invisible(tracker))
  }
  
  tracker$last_update <- current_time
  
  # Calculate progress
  percent <- round((tracker$current / tracker$total) * 100)
  elapsed <- as.numeric(difftime(Sys.time(), tracker$start_time, units = "secs"))
  
  # Build progress bar
  bar_width <- 40
  filled <- round((tracker$current / tracker$total) * bar_width)
  bar <- paste0(
    "[",
    paste(rep("=", filled), collapse = ""),
    paste(rep(" ", bar_width - filled), collapse = ""),
    "]"
  )
  
  # Build message components
  components <- c()
  
  if (tracker$show_percent) {
    components <- c(components, paste0(percent, "%"))
  }
  
  components <- c(components, paste0(tracker$current, "/", tracker$total))
  
  if (tracker$show_time && tracker$current > 0) {
    # Estimate remaining time
    rate <- tracker$current / elapsed
    remaining_items <- tracker$total - tracker$current
    eta_seconds <- remaining_items / rate
    
    if (eta_seconds < 60) {
      eta_str <- paste0(round(eta_seconds), "s")
    } else if (eta_seconds < 3600) {
      eta_str <- paste0(round(eta_seconds / 60), "m")
    } else {
      eta_str <- paste0(round(eta_seconds / 3600, 1), "h")
    }
    
    components <- c(components, paste0("ETA: ", eta_str))
  }
  
  if (!is.null(message)) {
    components <- c(components, message)
  }
  
  # Build final message
  progress_msg <- paste0("\r", bar, " ", paste(components, collapse = " | "))
  
  # Clear previous line and print new progress
  cat(paste0("\r", paste(rep(" ", nchar(tracker$last_message)), collapse = "")))
  cat(progress_msg)
  
  tracker$last_message <- progress_msg
  
  # Add newline when complete
  if (tracker$current >= tracker$total) {
    cat("\n")
  }
  
  return(invisible(tracker))
}

#' Update Shiny progress
#' @keywords internal
update_shiny_progress <- function(tracker, message) {
  
  if (is.null(tracker$shiny_progress)) {
    return(invisible(tracker))
  }
  
  # Calculate progress value
  value <- tracker$current / tracker$total
  
  # Build detail message
  details <- c()
  
  if (tracker$show_percent) {
    details <- c(details, paste0(round(value * 100), "%"))
  }
  
  details <- c(details, paste0(tracker$current, "/", tracker$total))
  
  if (tracker$show_time && tracker$current > 0) {
    elapsed <- as.numeric(difftime(Sys.time(), tracker$start_time, units = "secs"))
    rate <- tracker$current / elapsed
    remaining_items <- tracker$total - tracker$current
    eta_seconds <- remaining_items / rate
    
    if (eta_seconds < 60) {
      eta_str <- paste0(round(eta_seconds), "s remaining")
    } else if (eta_seconds < 3600) {
      eta_str <- paste0(round(eta_seconds / 60), "m remaining")
    } else {
      eta_str <- paste0(round(eta_seconds / 3600, 1), "h remaining")
    }
    
    details <- c(details, eta_str)
  }
  
  detail_msg <- paste(details, collapse = " | ")
  
  if (!is.null(message)) {
    detail_msg <- paste0(detail_msg, " | ", message)
  }
  
  # Update Shiny progress
  tracker$shiny_progress$set(
    value = value,
    detail = detail_msg
  )
  
  return(invisible(tracker))
}

#' Finish progress tracker
#'
#' @param tracker Progress tracker object
#' @param message Optional completion message
#'
#' @export
finish_progress <- function(tracker, message = "Complete!") {
  
  if (!inherits(tracker, "progress_tracker") || tracker$finished) {
    return(invisible(tracker))
  }
  
  tracker$finished <- TRUE
  
  if (tracker$type == "console") {
    # Update to 100% completion
    update_console_progress(tracker, message)
    
    # Print summary
    elapsed <- as.numeric(difftime(Sys.time(), tracker$start_time, units = "secs"))
    if (elapsed < 60) {
      time_str <- paste0(round(elapsed, 1), " seconds")
    } else if (elapsed < 3600) {
      time_str <- paste0(round(elapsed / 60, 1), " minutes")
    } else {
      time_str <- paste0(round(elapsed / 3600, 1), " hours")
    }
    
    cat(paste0("Completed in ", time_str, "\n"))
    
  } else if (tracker$type == "shiny") {
    # Close Shiny progress
    if (!is.null(tracker$shiny_progress)) {
      tracker$shiny_progress$close()
      tracker$shiny_progress <- NULL
    }
  }
  
  return(invisible(tracker))
}

#' Progress wrapper for functions
#'
#' Wraps a function to automatically track progress for vectorized operations
#'
#' @param items Vector of items to process
#' @param fun Function to apply to each item
#' @param title Progress bar title
#' @param ... Additional arguments passed to fun
#'
#' @return List of results from applying fun to each item
#' @export
#' @examples
#' \dontrun{
#' # Process list with progress tracking
#' results <- with_progress(1:100, function(x) {
#'   Sys.sleep(0.1)  # Simulate work
#'   x^2
#' }, title = "Computing squares")
#' }
with_progress <- function(items, fun, title = "Processing", ...) {
  
  n_items <- length(items)
  
  if (n_items == 0) {
    return(list())
  }
  
  # Create progress tracker
  pb <- create_progress_tracker(n_items, title)
  
  # Process items
  results <- vector("list", n_items)
  
  tryCatch({
    for (i in seq_along(items)) {
      results[[i]] <- fun(items[[i]], ...)
      update_progress(pb, i)
    }
    
    finish_progress(pb)
    
  }, error = function(e) {
    finish_progress(pb, paste("Error:", e$message))
    stop(e)
  })
  
  return(results)
}

#' Cached computation with progress tracking
#'
#' Combines caching with progress tracking for expensive operations
#'
#' @param key Cache key
#' @param items Vector of items to process (for progress tracking)
#' @param expr Expression to evaluate if not cached
#' @param title Progress bar title
#' @param type Cache type
#' @param force Force re-computation
#' @param metadata Cache metadata
#'
#' @return Result of computation (cached or computed)
#' @export
#' @examples
#' \dontrun{
#' # Cached computation with progress
#' result <- cached_computation_with_progress(
#'   "expensive_analysis",
#'   items = 1:1000,
#'   expr = {
#'     # Expensive computation here
#'     with_progress(1:1000, expensive_function, "Computing")
#'   },
#'   title = "Analysis"
#' )
#' }
cached_computation_with_progress <- function(key, items, expr, 
                                           title = "Computing",
                                           type = "temp", 
                                           force = FALSE,
                                           metadata = NULL) {
  
  # Load cache system if needed
  if (!exists("cache_with_key")) {
    cache_file <- system.file("R", "cache_system.R", package = "diversityGPT")
    if (cache_file == "" || !file.exists(cache_file)) {
      cache_file <- "R/cache_system.R"
    }
    if (file.exists(cache_file)) {
      source(cache_file)
    }
  }
  
  # Check cache first
  if (exists("cache_with_key")) {
    return(cache_with_key(key, expr, type = type, force = force, metadata = metadata))
  } else {
    # Fallback without caching
    return(eval(expr, envir = parent.frame()))
  }
}

#' Progress-enabled dataset loading
#'
#' Load dataset with progress tracking for long operations
#'
#' @param dataset_id Dataset identifier
#' @param source Dataset source
#' @param show_progress Whether to show progress (default: TRUE)
#' @param ... Additional arguments
#'
#' @return Loaded dataset
#' @export
load_dataset_with_progress <- function(dataset_id, source = NULL, show_progress = TRUE, ...) {
  
  if (!show_progress) {
    # Load cache system and dataset loaders
    if (!exists("load_dataset")) {
      source("R/dataset_loaders.R")
    }
    return(load_dataset(dataset_id, source = source, ...))
  }
  
  # Create progress tracker
  pb <- create_progress_tracker(4, paste("Loading", dataset_id))
  
  tryCatch({
    # Step 1: Load registry
    update_progress(pb, 1, "Loading registry")
    if (!exists("get_dataset_info")) {
      source("R/dataset_registry.R")
    }
    
    # Step 2: Get dataset info
    update_progress(pb, 2, "Getting dataset info")
    dataset_info <- get_dataset_info(dataset_id, source)
    
    if (is.null(dataset_info)) {
      stop("Dataset not found: ", dataset_id)
    }
    
    # Step 3: Load dataset
    update_progress(pb, 3, "Loading data")
    if (!exists("load_dataset")) {
      source("R/dataset_loaders.R")
    }
    result <- load_dataset(dataset_id, source = source, ...)
    
    # Step 4: Complete
    update_progress(pb, 4, "Complete")
    finish_progress(pb)
    
    return(result)
    
  }, error = function(e) {
    finish_progress(pb, paste("Error:", e$message))
    stop(e)
  })
}