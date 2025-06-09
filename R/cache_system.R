#' Smart Caching System for diversityGPT
#'
#' Intelligent caching system for datasets, universal transformations,
#' and computed results to improve performance and user experience
#'
#' @import digest
#' @importFrom utils object.size

# Global cache configuration
.diversityGPT_cache <- new.env(parent = emptyenv())

#' Initialize cache system
#'
#' Sets up the caching directories and configuration
#'
#' @param cache_dir Base directory for cache storage
#' @param max_size_mb Maximum cache size in MB (default: 1000MB = 1GB)
#' @param max_age_days Maximum age of cached items in days (default: 30)
#' @param enable_memory_cache Enable in-memory caching (default: TRUE)
#' @param enable_disk_cache Enable disk-based caching (default: TRUE)
#'
#' @export
#' @examples
#' # Initialize with default settings
#' init_cache_system()
#' 
#' # Custom cache settings
#' init_cache_system(max_size_mb = 500, max_age_days = 7)
#'
init_cache_system <- function(cache_dir = NULL,
                             max_size_mb = 1000,
                             max_age_days = 30,
                             enable_memory_cache = TRUE,
                             enable_disk_cache = TRUE) {
  
  # Set default cache directory
  if (is.null(cache_dir)) {
    if (requireNamespace("rappdirs", quietly = TRUE)) {
      cache_dir <- file.path(rappdirs::user_cache_dir("diversityGPT"), "cache")
    } else {
      cache_dir <- file.path(tempdir(), "diversityGPT_cache")
    }
  }
  
  # Create cache directories
  datasets_dir <- file.path(cache_dir, "datasets")
  transformations_dir <- file.path(cache_dir, "transformations")
  temp_dir <- file.path(cache_dir, "temp")
  
  dir.create(datasets_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(transformations_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Store configuration
  .diversityGPT_cache$config <- list(
    cache_dir = cache_dir,
    datasets_dir = datasets_dir,
    transformations_dir = transformations_dir,
    temp_dir = temp_dir,
    max_size_mb = max_size_mb,
    max_age_days = max_age_days,
    enable_memory_cache = enable_memory_cache,
    enable_disk_cache = enable_disk_cache,
    initialized = Sys.time()
  )
  
  # Initialize memory cache
  if (enable_memory_cache) {
    .diversityGPT_cache$memory <- new.env(parent = emptyenv())
    .diversityGPT_cache$memory_stats <- list(
      hits = 0,
      misses = 0,
      size_mb = 0
    )
  }
  
  # Initialize disk cache index
  if (enable_disk_cache) {
    index_file <- file.path(cache_dir, "cache_index.rds")
    if (file.exists(index_file)) {
      .diversityGPT_cache$disk_index <- readRDS(index_file)
    } else {
      .diversityGPT_cache$disk_index <- list()
    }
    
    .diversityGPT_cache$disk_stats <- list(
      hits = 0,
      misses = 0,
      size_mb = 0
    )
  }
  
  message("Cache system initialized:")
  message("  Directory: ", cache_dir)
  message("  Max size: ", max_size_mb, " MB")
  message("  Max age: ", max_age_days, " days")
  message("  Memory cache: ", enable_memory_cache)
  message("  Disk cache: ", enable_disk_cache)
  
  # Clean up old cache entries
  invisible(cache_cleanup(verbose = FALSE))
}

#' Generate cache key for objects
#'
#' @param ... Objects to include in cache key generation
#' @param prefix Optional prefix for the cache key
#' @return Character string cache key
#' @keywords internal
generate_cache_key <- function(..., prefix = "") {
  # Combine all arguments into a list
  objects <- list(...)
  
  # Create a hash of the objects
  if (requireNamespace("digest", quietly = TRUE)) {
    hash <- digest::digest(objects, algo = "md5")
  } else {
    # Fallback method without digest package
    hash <- as.character(abs(hash.fun(objects)))
  }
  
  # Add prefix if provided
  if (nchar(prefix) > 0) {
    key <- paste0(prefix, "_", hash)
  } else {
    key <- hash
  }
  
  return(key)
}

#' Simple hash function fallback
#' @keywords internal
hash.fun <- function(x) {
  # Simple hash function for when digest is not available
  sum(utf8ToInt(paste(deparse(x), collapse = ""))) %% 2^31
}

#' Store object in cache
#'
#' @param key Cache key
#' @param object Object to cache
#' @param type Cache type: "dataset", "transformation", or "temp"
#' @param metadata Optional metadata for the cached object
#' @param force_disk Force storage to disk even if memory cache is enabled
#'
#' @return Logical indicating success
#' @export
#' @examples
#' \dontrun{
#' # Cache a dataset
#' cache_store("my_dataset", phyloseq_object, type = "dataset")
#' 
#' # Cache with metadata
#' cache_store("analysis_results", results, 
#'            type = "transformation",
#'            metadata = list(method = "shannon", date = Sys.time()))
#' }
cache_store <- function(key, object, type = "temp", metadata = NULL, force_disk = FALSE) {
  
  if (!cache_is_initialized()) {
    init_cache_system()
  }
  
  config <- .diversityGPT_cache$config
  
  # Calculate object size
  obj_size_bytes <- as.numeric(object.size(object))
  obj_size_mb <- obj_size_bytes / (1024^2)
  
  # Check if object is too large
  if (obj_size_mb > config$max_size_mb * 0.5) {
    warning("Object size (", round(obj_size_mb, 1), " MB) exceeds 50% of cache limit")
    return(FALSE)
  }
  
  # Determine storage location
  use_memory <- config$enable_memory_cache && !force_disk && obj_size_mb < 100
  use_disk <- config$enable_disk_cache
  
  stored <- FALSE
  
  # Store in memory cache
  if (use_memory) {
    .diversityGPT_cache$memory[[key]] <- list(
      object = object,
      metadata = metadata,
      created = Sys.time(),
      type = type,
      size_mb = obj_size_mb,
      access_count = 0,
      last_accessed = Sys.time()
    )
    
    # Update memory stats
    .diversityGPT_cache$memory_stats$size_mb <- 
      .diversityGPT_cache$memory_stats$size_mb + obj_size_mb
    
    stored <- TRUE
  }
  
  # Store on disk cache
  if (use_disk) {
    # Determine subdirectory
    subdir <- switch(type,
      "dataset" = config$datasets_dir,
      "transformation" = config$transformations_dir,
      "temp" = config$temp_dir,
      config$temp_dir
    )
    
    # Store object
    cache_file <- file.path(subdir, paste0(key, ".rds"))
    
    tryCatch({
      saveRDS(object, cache_file, compress = TRUE)
      
      # Update disk index
      .diversityGPT_cache$disk_index[[key]] <- list(
        file = cache_file,
        metadata = metadata,
        created = Sys.time(),
        type = type,
        size_mb = obj_size_mb,
        access_count = 0,
        last_accessed = Sys.time()
      )
      
      # Save index
      index_file <- file.path(config$cache_dir, "cache_index.rds")
      saveRDS(.diversityGPT_cache$disk_index, index_file)
      
      # Update disk stats
      .diversityGPT_cache$disk_stats$size_mb <- 
        .diversityGPT_cache$disk_stats$size_mb + obj_size_mb
      
      stored <- TRUE
      
    }, error = function(e) {
      warning("Failed to store object in disk cache: ", e$message)
    })
  }
  
  return(stored)
}

#' Retrieve object from cache
#'
#' @param key Cache key
#' @param default Default value if not found in cache
#'
#' @return Cached object or default value
#' @export
#' @examples
#' \dontrun{
#' # Retrieve from cache
#' data <- cache_get("my_dataset")
#' 
#' # With default value
#' data <- cache_get("my_dataset", default = NULL)
#' }
cache_get <- function(key, default = NULL) {
  
  if (!cache_is_initialized()) {
    return(default)
  }
  
  config <- .diversityGPT_cache$config
  
  # Check memory cache first
  if (config$enable_memory_cache && exists(key, envir = .diversityGPT_cache$memory)) {
    entry <- .diversityGPT_cache$memory[[key]]
    
    # Update access statistics
    entry$access_count <- entry$access_count + 1
    entry$last_accessed <- Sys.time()
    .diversityGPT_cache$memory[[key]] <- entry
    .diversityGPT_cache$memory_stats$hits <- .diversityGPT_cache$memory_stats$hits + 1
    
    return(entry$object)
  }
  
  # Check disk cache
  if (config$enable_disk_cache && key %in% names(.diversityGPT_cache$disk_index)) {
    entry <- .diversityGPT_cache$disk_index[[key]]
    
    # Check if file still exists
    if (file.exists(entry$file)) {
      tryCatch({
        object <- readRDS(entry$file)
        
        # Update access statistics
        entry$access_count <- entry$access_count + 1
        entry$last_accessed <- Sys.time()
        .diversityGPT_cache$disk_index[[key]] <- entry
        .diversityGPT_cache$disk_stats$hits <- .diversityGPT_cache$disk_stats$hits + 1
        
        # Store in memory cache if enabled and object is small enough
        if (config$enable_memory_cache && entry$size_mb < 100) {
          cache_store(key, object, type = entry$type, metadata = entry$metadata)
        }
        
        return(object)
        
      }, error = function(e) {
        warning("Failed to read cached object: ", e$message)
        # Remove invalid entry
        cache_remove(key)
      })
    } else {
      # Remove invalid entry
      cache_remove(key)
    }
  }
  
  # Update miss statistics
  if (config$enable_memory_cache) {
    .diversityGPT_cache$memory_stats$misses <- .diversityGPT_cache$memory_stats$misses + 1
  }
  if (config$enable_disk_cache) {
    .diversityGPT_cache$disk_stats$misses <- .diversityGPT_cache$disk_stats$misses + 1
  }
  
  return(default)
}

#' Check if object exists in cache
#'
#' @param key Cache key
#' @return Logical indicating if key exists in cache
#' @export
cache_exists <- function(key) {
  
  if (!cache_is_initialized()) {
    return(FALSE)
  }
  
  config <- .diversityGPT_cache$config
  
  # Check memory cache
  if (config$enable_memory_cache && exists(key, envir = .diversityGPT_cache$memory)) {
    return(TRUE)
  }
  
  # Check disk cache
  if (config$enable_disk_cache && key %in% names(.diversityGPT_cache$disk_index)) {
    entry <- .diversityGPT_cache$disk_index[[key]]
    return(file.exists(entry$file))
  }
  
  return(FALSE)
}

#' Remove object from cache
#'
#' @param key Cache key
#' @return Logical indicating success
#' @export
cache_remove <- function(key) {
  
  if (!cache_is_initialized()) {
    return(FALSE)
  }
  
  config <- .diversityGPT_cache$config
  removed <- FALSE
  
  # Remove from memory cache
  if (config$enable_memory_cache && exists(key, envir = .diversityGPT_cache$memory)) {
    entry <- .diversityGPT_cache$memory[[key]]
    .diversityGPT_cache$memory_stats$size_mb <- 
      .diversityGPT_cache$memory_stats$size_mb - entry$size_mb
    
    rm(list = key, envir = .diversityGPT_cache$memory)
    removed <- TRUE
  }
  
  # Remove from disk cache
  if (config$enable_disk_cache && key %in% names(.diversityGPT_cache$disk_index)) {
    entry <- .diversityGPT_cache$disk_index[[key]]
    
    # Remove file
    if (file.exists(entry$file)) {
      unlink(entry$file)
    }
    
    # Update stats
    .diversityGPT_cache$disk_stats$size_mb <- 
      .diversityGPT_cache$disk_stats$size_mb - entry$size_mb
    
    # Remove from index
    .diversityGPT_cache$disk_index[[key]] <- NULL
    
    # Save updated index
    index_file <- file.path(config$cache_dir, "cache_index.rds")
    saveRDS(.diversityGPT_cache$disk_index, index_file)
    
    removed <- TRUE
  }
  
  return(removed)
}

#' Clean up cache based on age and size limits
#'
#' @param force_size_limit Force cleanup if size limit exceeded
#' @param verbose Print cleanup information
#' @return List with cleanup statistics
#' @export
#' @examples
#' \dontrun{
#' # Regular cleanup
#' cache_cleanup()
#' 
#' # Force cleanup with details
#' stats <- cache_cleanup(force_size_limit = TRUE, verbose = TRUE)
#' }
cache_cleanup <- function(force_size_limit = FALSE, verbose = TRUE) {
  
  if (!cache_is_initialized()) {
    return(list(removed = 0, size_freed_mb = 0))
  }
  
  config <- .diversityGPT_cache$config
  removed_count <- 0
  size_freed_mb <- 0
  
  # Cleanup memory cache
  if (config$enable_memory_cache) {
    memory_keys <- ls(envir = .diversityGPT_cache$memory)
    
    for (key in memory_keys) {
      entry <- .diversityGPT_cache$memory[[key]]
      age_days <- as.numeric(difftime(Sys.time(), entry$created, units = "days"))
      
      # Remove if too old
      if (age_days > config$max_age_days) {
        size_freed_mb <- size_freed_mb + entry$size_mb
        cache_remove(key)
        removed_count <- removed_count + 1
      }
    }
    
    # Check size limit
    if (force_size_limit || .diversityGPT_cache$memory_stats$size_mb > config$max_size_mb * 0.8) {
      # Remove least recently used items
      memory_keys <- ls(envir = .diversityGPT_cache$memory)
      if (length(memory_keys) > 0) {
        entries <- lapply(memory_keys, function(k) {
          entry <- .diversityGPT_cache$memory[[k]]
          entry$key <- k
          entry
        })
        
        # Sort by last accessed (oldest first)
        entries <- entries[order(sapply(entries, function(x) x$last_accessed))]
        
        # Remove until under limit
        for (entry in entries) {
          if (.diversityGPT_cache$memory_stats$size_mb <= config$max_size_mb * 0.7) break
          
          size_freed_mb <- size_freed_mb + entry$size_mb
          cache_remove(entry$key)
          removed_count <- removed_count + 1
        }
      }
    }
  }
  
  # Cleanup disk cache
  if (config$enable_disk_cache) {
    disk_keys <- names(.diversityGPT_cache$disk_index)
    
    for (key in disk_keys) {
      entry <- .diversityGPT_cache$disk_index[[key]]
      
      # Check if file exists
      if (!file.exists(entry$file)) {
        cache_remove(key)
        removed_count <- removed_count + 1
        next
      }
      
      # Check age
      age_days <- as.numeric(difftime(Sys.time(), entry$created, units = "days"))
      if (age_days > config$max_age_days) {
        size_freed_mb <- size_freed_mb + entry$size_mb
        cache_remove(key)
        removed_count <- removed_count + 1
      }
    }
    
    # Check size limit
    if (force_size_limit || .diversityGPT_cache$disk_stats$size_mb > config$max_size_mb * 0.8) {
      disk_keys <- names(.diversityGPT_cache$disk_index)
      if (length(disk_keys) > 0) {
        entries <- lapply(disk_keys, function(k) {
          entry <- .diversityGPT_cache$disk_index[[k]]
          entry$key <- k
          entry
        })
        
        # Sort by last accessed (oldest first)
        entries <- entries[order(sapply(entries, function(x) x$last_accessed))]
        
        # Remove until under limit
        for (entry in entries) {
          if (.diversityGPT_cache$disk_stats$size_mb <= config$max_size_mb * 0.7) break
          
          size_freed_mb <- size_freed_mb + entry$size_mb
          cache_remove(entry$key)
          removed_count <- removed_count + 1
        }
      }
    }
  }
  
  if (verbose && removed_count > 0) {
    message("Cache cleanup completed:")
    message("  Removed items: ", removed_count)
    message("  Space freed: ", round(size_freed_mb, 1), " MB")
  }
  
  return(list(
    removed = removed_count,
    size_freed_mb = size_freed_mb
  ))
}

#' Get cache statistics
#'
#' @return List with cache statistics
#' @export
#' @examples
#' \dontrun{
#' stats <- cache_stats()
#' print(stats)
#' }
cache_stats <- function() {
  
  if (!cache_is_initialized()) {
    return(list(status = "not_initialized"))
  }
  
  config <- .diversityGPT_cache$config
  
  # Memory cache stats
  memory_stats <- NULL
  if (config$enable_memory_cache) {
    memory_keys <- ls(envir = .diversityGPT_cache$memory)
    memory_stats <- list(
      enabled = TRUE,
      items = length(memory_keys),
      size_mb = round(.diversityGPT_cache$memory_stats$size_mb, 2),
      hits = .diversityGPT_cache$memory_stats$hits,
      misses = .diversityGPT_cache$memory_stats$misses,
      hit_rate = if (.diversityGPT_cache$memory_stats$hits + .diversityGPT_cache$memory_stats$misses > 0) {
        round(.diversityGPT_cache$memory_stats$hits / 
              (.diversityGPT_cache$memory_stats$hits + .diversityGPT_cache$memory_stats$misses), 3)
      } else { 0 }
    )
  }
  
  # Disk cache stats
  disk_stats <- NULL
  if (config$enable_disk_cache) {
    disk_keys <- names(.diversityGPT_cache$disk_index)
    disk_stats <- list(
      enabled = TRUE,
      items = length(disk_keys),
      size_mb = round(.diversityGPT_cache$disk_stats$size_mb, 2),
      hits = .diversityGPT_cache$disk_stats$hits,
      misses = .diversityGPT_cache$disk_stats$misses,
      hit_rate = if (.diversityGPT_cache$disk_stats$hits + .diversityGPT_cache$disk_stats$misses > 0) {
        round(.diversityGPT_cache$disk_stats$hits / 
              (.diversityGPT_cache$disk_stats$hits + .diversityGPT_cache$disk_stats$misses), 3)
      } else { 0 }
    )
  }
  
  return(list(
    status = "initialized",
    config = config,
    memory = memory_stats,
    disk = disk_stats,
    uptime_hours = round(as.numeric(difftime(Sys.time(), config$initialized, units = "hours")), 1)
  ))
}

#' Clear all cache
#'
#' @param confirm Require confirmation (default: TRUE)
#' @return Logical indicating success
#' @export
#' @examples
#' \dontrun{
#' # Clear with confirmation
#' cache_clear()
#' 
#' # Force clear without confirmation
#' cache_clear(confirm = FALSE)
#' }
cache_clear <- function(confirm = TRUE) {
  
  if (!cache_is_initialized()) {
    message("Cache not initialized")
    return(FALSE)
  }
  
  if (confirm) {
    response <- readline("Are you sure you want to clear all cache? (y/N): ")
    if (tolower(response) != "y") {
      message("Cache clear cancelled")
      return(FALSE)
    }
  }
  
  config <- .diversityGPT_cache$config
  
  # Clear memory cache
  if (config$enable_memory_cache) {
    rm(list = ls(envir = .diversityGPT_cache$memory), envir = .diversityGPT_cache$memory)
    .diversityGPT_cache$memory_stats <- list(hits = 0, misses = 0, size_mb = 0)
  }
  
  # Clear disk cache
  if (config$enable_disk_cache) {
    # Remove all cached files
    unlink(file.path(config$cache_dir, "*"), recursive = TRUE)
    
    # Recreate directories
    dir.create(config$datasets_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(config$transformations_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(config$temp_dir, recursive = TRUE, showWarnings = FALSE)
    
    # Reset index and stats
    .diversityGPT_cache$disk_index <- list()
    .diversityGPT_cache$disk_stats <- list(hits = 0, misses = 0, size_mb = 0)
  }
  
  message("Cache cleared successfully")
  return(TRUE)
}

#' Check if cache system is initialized
#'
#' @return Logical indicating if cache is initialized
#' @export
cache_is_initialized <- function() {
  exists("config", envir = .diversityGPT_cache, inherits = FALSE)
}

#' Cached wrapper for expensive operations
#'
#' @param key Cache key
#' @param expr Expression to evaluate if not in cache
#' @param type Cache type (default: "temp")
#' @param force Force re-evaluation even if cached
#' @param metadata Optional metadata for cached result
#'
#' @return Result of expression (from cache or computed)
#' @export
#' @examples
#' \dontrun{
#' # Cache expensive computation
#' result <- cache_with_key("expensive_calc", {
#'   # Some expensive computation
#'   heavy_calculation(data)
#' })
#' 
#' # With metadata
#' result <- cache_with_key("analysis_v2", {
#'   analyze_data(dataset)
#' }, type = "transformation", metadata = list(version = "2.0"))
#' }
cache_with_key <- function(key, expr, type = "temp", force = FALSE, metadata = NULL) {
  
  # Check cache first (unless forcing)
  if (!force && cache_exists(key)) {
    return(cache_get(key))
  }
  
  # Evaluate expression
  result <- eval(expr, envir = parent.frame())
  
  # Store in cache
  cache_store(key, result, type = type, metadata = metadata)
  
  return(result)
}