#' Dataset Loader Functions for diversityGPT
#'
#' Functions to load datasets from various sources including built-in,
#' precomputed, and external repositories. Includes intelligent caching
#' for improved performance.
#'

#' Load dataset by ID
#'
#' Main function to load any dataset from the registry
#'
#' @param dataset_id Character string identifying the dataset
#' @param source Optional source category to search within
#' @param use_cache Whether to use caching (default: TRUE)
#' @param force_reload Force reload even if cached (default: FALSE)
#' @param ... Additional arguments passed to specific loaders
#'
#' @return phyloseq object or universal_information object
#'
#' @export
#' @examples
#' # Load built-in dataset
#' physeq <- load_dataset("globalpatterns")
#' 
#' # Load precomputed results
#' universal_results <- load_dataset("hmp_gut_16s", source = "precomputed")
#'
load_dataset <- function(dataset_id, source = NULL, use_cache = TRUE, force_reload = FALSE, ...) {
  
  # Load cache system if not already loaded
  if (!exists("cache_is_initialized") || !cache_is_initialized()) {
    # Try to source cache system
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
  
  # Get dataset info from registry
  dataset_info <- get_dataset_info(dataset_id, source)
  
  if (is.null(dataset_info)) {
    stop("Dataset not found: ", dataset_id, "\n",
         "Use list_available_datasets() to see available options.")
  }
  
  # Generate cache key
  cache_key <- NULL
  if (use_cache && exists("generate_cache_key")) {
    cache_key <- generate_cache_key(
      dataset_id = dataset_id,
      source = source,
      loader_args = list(...),
      prefix = "dataset"
    )
    
    # Check cache first (unless forcing reload)
    if (!force_reload && exists("cache_get")) {
      cached_result <- cache_get(cache_key)
      if (!is.null(cached_result)) {
        message("Loaded dataset from cache: ", dataset_info$name)
        return(cached_result)
      }
    }
  }
  
  # Check if it's a collection
  if (!is.null(dataset_info$datasets)) {
    message("This is a dataset collection. Available datasets:")
    for (name in names(dataset_info$datasets)) {
      message("  - ", name, ": ", dataset_info$datasets[[name]])
    }
    stop("Please specify a specific dataset from the collection.")
  }
  
  # Check if this dataset requires format conversion
  if (!is.null(dataset_info$file_format) && !is.null(dataset_info$file_path)) {
    # Load format converters
    if (!exists("convert_to_phyloseq")) {
      converter_file <- system.file("R", "format_converters.R", package = "diversityGPT")
      if (converter_file == "" || !file.exists(converter_file)) {
        converter_file <- "R/format_converters.R"
      }
      source(converter_file)
    }
    
    message("Loading dataset: ", dataset_info$name, " (format: ", dataset_info$file_format, ")")
    result <- convert_to_phyloseq(
      file_path = dataset_info$file_path,
      format = dataset_info$file_format,
      ...
    )
  } else {
    # Use standard loader functions
    loader_func <- switch(dataset_info$loader,
      "load_phyloseq_builtin" = load_phyloseq_builtin,
      "load_precomputed" = load_precomputed,
      "load_curated_mgd" = load_curated_mgd,
      "load_microbiome_pkg" = load_microbiome_pkg,
      "load_hmp16s" = load_hmp16s,
      "load_format_converted" = load_format_converted,
      stop("Unknown loader: ", dataset_info$loader)
    )
    
    # Load the dataset
    message("Loading dataset: ", dataset_info$name)
    result <- loader_func(dataset_info, ...)
  }
  
  # Store in cache if enabled
  if (use_cache && !is.null(cache_key) && !is.null(result) && exists("cache_store")) {
    cache_metadata <- list(
      dataset_id = dataset_id,
      source = if (is.null(source)) "unknown" else source,
      loader = dataset_info$loader,
      loaded_at = Sys.time(),
      dataset_name = dataset_info$name
    )
    
    cache_store(cache_key, result, type = "dataset", metadata = cache_metadata)
    message("Dataset cached for future use")
  }
  
  return(result)
}

#' Load built-in phyloseq dataset
#'
#' @param dataset_info List containing dataset metadata
#' @param ... Additional arguments (unused)
#'
#' @return phyloseq object
#' @keywords internal
#'
load_phyloseq_builtin <- function(dataset_info, ...) {
  if (!requireNamespace("phyloseq", quietly = TRUE)) {
    stop("Package 'phyloseq' is required but not installed.")
  }
  
  # Load the data
  data_name <- dataset_info$data_name
  data(list = data_name, package = "phyloseq", envir = environment())
  
  # Return the loaded object
  physeq <- get(data_name, envir = environment())
  
  message("Loaded ", data_name, ": ",
          nsamples(physeq), " samples, ",
          ntaxa(physeq), " taxa")
  
  return(physeq)
}

#' Load precomputed universal results
#'
#' @param dataset_info List containing dataset metadata
#' @param return_physeq If TRUE, return phyloseq object if available
#' @param ... Additional arguments (unused)
#'
#' @return universal_information object or phyloseq object
#' @keywords internal
#'
load_precomputed <- function(dataset_info, return_physeq = FALSE, ...) {
  
  # Check if file exists
  data_file <- system.file("data", dataset_info$file, package = "diversityGPT")
  
  if (data_file == "" || !file.exists(data_file)) {
    # Try data-raw directory during development
    alt_file <- file.path("inst/data", dataset_info$file)
    if (file.exists(alt_file)) {
      data_file <- alt_file
    } else {
      stop("Precomputed file not found: ", dataset_info$file, "\n",
           "Run the data processing scripts in data-raw/ to generate this file.")
    }
  }
  
  # Load the data
  result <- readRDS(data_file)
  
  # Check what was loaded
  if (inherits(result, "list") && !is.null(result$universal_info)) {
    # This is a full results object
    if (return_physeq && !is.null(result$phyloseq)) {
      message("Loaded phyloseq object from precomputed results")
      return(result$phyloseq)
    } else {
      message("Loaded precomputed universal transformation results")
      message("Mean R²: ", round(result$universal_info$deconvolution_quality$mean_r_squared, 3))
      return(result$universal_info)
    }
  } else if (inherits(result, "universal_information")) {
    # Direct universal_info object
    message("Loaded precomputed universal transformation results")
    message("Mean R²: ", round(result$deconvolution_quality$mean_r_squared, 3))
    return(result)
  } else if (inherits(result, "phyloseq")) {
    # Direct phyloseq object
    message("Loaded phyloseq object: ",
            nsamples(result), " samples, ",
            ntaxa(result), " taxa")
    return(result)
  } else {
    stop("Unexpected object type in precomputed file: ", class(result)[1])
  }
}

#' Load dataset from curatedMetagenomicData
#'
#' @param dataset_info List containing dataset metadata
#' @param study_name Specific study to load
#' @param dryrun If TRUE, only show available data
#' @param counts Return count data (vs relative abundance)
#' @param ... Additional arguments passed to curatedMetagenomicData()
#'
#' @return phyloseq object
#' @keywords internal
#'
load_curated_mgd <- function(dataset_info, study_name = NULL, 
                            dryrun = FALSE, counts = TRUE, ...) {
  
  if (!requireNamespace("curatedMetagenomicData", quietly = TRUE)) {
    stop("Package 'curatedMetagenomicData' is required but not installed.\n",
         "Install from Bioconductor:\n",
         "BiocManager::install('curatedMetagenomicData')")
  }
  
  if (dryrun || is.null(study_name)) {
    # Show available studies
    message("Available studies in curatedMetagenomicData:")
    message("Use sampleMetadata to explore available studies")
    return(NULL)
  }
  
  # Load the requested study
  message("Loading ", study_name, " from curatedMetagenomicData...")
  
  # Get the data
  result <- curatedMetagenomicData::curatedMetagenomicData(
    study_name,
    dryrun = FALSE,
    counts = counts,
    bugs.as.phyloseq = TRUE,
    ...
  )
  
  # Convert to phyloseq if needed
  if (inherits(result, "list") && length(result) == 1) {
    result <- result[[1]]
  }
  
  if (!inherits(result, "phyloseq")) {
    stop("Failed to convert curatedMetagenomicData to phyloseq format")
  }
  
  message("Loaded: ", nsamples(result), " samples, ", ntaxa(result), " taxa")
  return(result)
}

#' Load dataset from microbiome package
#'
#' @param dataset_info List containing dataset metadata
#' @param dataset_name Specific dataset to load
#' @param ... Additional arguments (unused)
#'
#' @return phyloseq object
#' @keywords internal
#'
load_microbiome_pkg <- function(dataset_info, dataset_name = NULL, ...) {
  
  if (!requireNamespace("microbiome", quietly = TRUE)) {
    stop("Package 'microbiome' is required but not installed.\n",
         "Install from CRAN: install.packages('microbiome')")
  }
  
  if (is.null(dataset_name)) {
    message("Available datasets in microbiome package:")
    message("  - atlas1006: 1006 western adults gut microbiome")
    message("  - dietswap: Diet swap experiment")
    message("  - peerj32: Peer-reviewed study")
    stop("Please specify dataset_name")
  }
  
  # Load the data
  data(list = dataset_name, package = "microbiome", envir = environment())
  physeq <- get(dataset_name, envir = environment())
  
  if (!inherits(physeq, "phyloseq")) {
    stop("Loaded object is not a phyloseq object")
  }
  
  message("Loaded ", dataset_name, ": ",
          nsamples(physeq), " samples, ",
          ntaxa(physeq), " taxa")
  
  return(physeq)
}

#' Load dataset from HMP16SData
#'
#' @param dataset_info List containing dataset metadata
#' @param region Which 16S region ("V13" or "V35")
#' @param body_site Specific body site to load
#' @param ... Additional arguments passed to HMP16SData functions
#'
#' @return phyloseq object
#' @keywords internal
#'
load_hmp16s <- function(dataset_info, region = "V35", body_site = NULL, ...) {
  
  if (!requireNamespace("HMP16SData", quietly = TRUE)) {
    stop("Package 'HMP16SData' is required but not installed.\n",
         "Install from Bioconductor:\n",
         "BiocManager::install('HMP16SData')")
  }
  
  # Get appropriate data based on region
  if (region == "V13") {
    data_func <- HMP16SData::V13
  } else if (region == "V35") {
    data_func <- HMP16SData::V35
  } else {
    stop("region must be 'V13' or 'V35'")
  }
  
  # Load the data
  message("Loading HMP 16S ", region, " data...")
  result <- data_func()
  
  # Convert to phyloseq
  if (!inherits(result, "phyloseq")) {
    # Try to convert if it's a different format
    stop("HMP16SData did not return a phyloseq object")
  }
  
  # Filter by body site if requested
  if (!is.null(body_site)) {
    if ("body_site" %in% names(sample_data(result))) {
      result <- subset_samples(result, body_site == body_site)
      message("Filtered to body site: ", body_site)
    } else {
      warning("No 'body_site' variable found in sample data")
    }
  }
  
  message("Loaded: ", nsamples(result), " samples, ", ntaxa(result), " taxa")
  return(result)
}

#' Load dataset using format converters
#'
#' @param dataset_info List containing dataset metadata
#' @param ... Additional arguments passed to format converters
#'
#' @return phyloseq object
#' @keywords internal
#'
load_format_converted <- function(dataset_info, ...) {
  
  # Load format converters if not already available
  if (!exists("convert_to_phyloseq")) {
    converter_file <- system.file("R", "format_converters.R", package = "diversityGPT")
    if (converter_file == "" || !file.exists(converter_file)) {
      converter_file <- "R/format_converters.R"
    }
    source(converter_file)
  }
  
  # Check if file exists
  if (!file.exists(dataset_info$file_path)) {
    stop("Data file not found: ", dataset_info$file_path)
  }
  
  # Convert using appropriate format converter
  result <- convert_to_phyloseq(
    file_path = dataset_info$file_path,
    format = dataset_info$file_format,
    ...
  )
  
  return(result)
}

#' Download and cache external dataset
#'
#' @param url URL to download from
#' @param cache_name Name for cached file
#' @param force Force re-download even if cached
#'
#' @return Path to downloaded file
#' @keywords internal
#'
download_dataset <- function(url, cache_name, force = FALSE) {
  
  # Create cache directory
  cache_dir <- file.path(
    rappdirs::user_cache_dir("diversityGPT"), 
    "downloads"
  )
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Check if already cached
  cache_file <- file.path(cache_dir, cache_name)
  
  if (file.exists(cache_file) && !force) {
    # Check age - re-download if older than 30 days
    age_days <- as.numeric(difftime(Sys.time(), file.mtime(cache_file), units = "days"))
    
    if (age_days < 30) {
      message("Using cached file (", round(age_days, 1), " days old)")
      return(cache_file)
    } else {
      message("Cached file is ", round(age_days), " days old, re-downloading...")
    }
  }
  
  # Download file
  message("Downloading from: ", url)
  message("This may take a while...")
  
  tryCatch({
    download.file(url, cache_file, mode = "wb", quiet = FALSE)
    message("Download complete!")
    return(cache_file)
  }, error = function(e) {
    stop("Failed to download dataset: ", e$message)
  })
}

#' Create a demo subset of a large dataset
#'
#' Useful for quick testing and demonstrations
#'
#' @param physeq phyloseq object to subset
#' @param n_samples Number of samples to keep
#' @param n_taxa Number of taxa to keep (most abundant)
#' @param seed Random seed for reproducibility
#'
#' @return Subsetted phyloseq object
#'
#' @export
#' @examples
#' # Create a small demo dataset
#' demo_data <- create_demo_subset(GlobalPatterns, n_samples = 10, n_taxa = 100)
#'
create_demo_subset <- function(physeq, n_samples = 50, n_taxa = 500, seed = 123) {
  
  set.seed(seed)
  
  # Subset samples
  if (nsamples(physeq) > n_samples) {
    keep_samples <- sample(sample_names(physeq), n_samples)
    physeq <- prune_samples(keep_samples, physeq)
  }
  
  # Subset taxa (keep most abundant)
  if (ntaxa(physeq) > n_taxa) {
    # Calculate total abundance per taxon
    taxon_sums <- taxa_sums(physeq)
    keep_taxa <- names(sort(taxon_sums, decreasing = TRUE)[1:n_taxa])
    physeq <- prune_taxa(keep_taxa, physeq)
  }
  
  message("Created demo subset: ", 
          nsamples(physeq), " samples, ",
          ntaxa(physeq), " taxa")
  
  return(physeq)
}