#' Dataset Registry for diversityGPT
#'
#' Centralized registry of all available microbiome datasets with metadata
#'
#' @format A nested list containing dataset information organized by category:
#' \describe{
#'   \item{builtin}{Datasets included with phyloseq package}
#'   \item{precomputed}{Datasets with pre-calculated universal transformations}
#'   \item{external}{Datasets from external repositories}
#' }
#'
#' @examples
#' # Get all built-in datasets
#' names(diversityGPT_datasets$builtin)
#' 
#' # Get info for a specific dataset
#' diversityGPT_datasets$builtin$globalpatterns
#' 
#' @export
diversityGPT_datasets <- list(
  builtin = list(
    globalpatterns = list(
      id = "globalpatterns",
      name = "Global Patterns of 16S Diversity",
      type = "16S",
      source = "phyloseq",
      samples = 26,
      taxa = 19216,
      has_tree = TRUE,
      has_metadata = TRUE,
      metadata_vars = c("SampleType", "Description", "Primer", "Barcode_full_length"),
      description = "Global survey of bacterial and archaeal diversity across diverse environments",
      citation = "Caporaso et al. (2011) PNAS 108:4516-4522",
      tags = c("environmental", "survey", "16S V4", "diverse habitats"),
      data_name = "GlobalPatterns",
      loader = "load_phyloseq_builtin"
    ),
    
    enterotype = list(
      id = "enterotype",
      name = "Human Gut Enterotypes",
      type = "16S", 
      source = "phyloseq",
      samples = 280,
      taxa = 553,
      has_tree = FALSE,
      has_metadata = TRUE,
      metadata_vars = c("Enterotype", "Nationality", "Gender", "Age", "ClinicalStatus", "SeqTech"),
      description = "Human gut microbiome samples showing distinct enterotype clusters",
      citation = "Arumugam et al. (2011) Nature 473:174-180",
      tags = c("human gut", "enterotypes", "16S V3-V4", "healthy adults"),
      data_name = "enterotype",
      loader = "load_phyloseq_builtin"
    ),
    
    soilrep = list(
      id = "soilrep",
      name = "Soil Microbiome Reproducibility",
      type = "16S",
      source = "phyloseq",
      samples = 56,
      taxa = 16825,
      has_tree = FALSE,
      has_metadata = TRUE,
      metadata_vars = c("Treatment", "warmed", "clipped", "Sample"),
      description = "Soil warming and plant clipping experiment to test reproducibility",
      citation = "Shade et al. (2012) ISME J 7:540-544",
      tags = c("soil", "environmental", "climate change", "experimental", "16S"),
      data_name = "soilrep",
      loader = "load_phyloseq_builtin"
    )
  ),
  
  precomputed = list(
    globalpatterns_demo = list(
      id = "globalpatterns_demo",
      name = "Global Patterns (Precomputed Demo)",
      type = "16S",
      source = "precomputed",
      samples = 26,
      taxa = 2000,
      has_tree = TRUE,
      has_metadata = TRUE,
      metadata_vars = c("SampleType", "Description", "Primer", "Barcode_full_length"),
      description = "Pre-analyzed subset of GlobalPatterns with universal transformations (R² = 0.884)",
      citation = "Caporaso et al. (2011) PNAS 108:4516-4522",
      file = "globalpatterns_demo_universal.rds",
      tags = c("environmental", "demo", "precomputed", "16S V4", "universal analysis"),
      loader = "load_precomputed"
    ),
    
    enterotype_demo = list(
      id = "enterotype_demo",
      name = "Enterotype (Precomputed Demo)",
      type = "16S",
      source = "precomputed",
      samples = 100,
      taxa = 553,
      has_tree = FALSE,
      has_metadata = TRUE,
      metadata_vars = c("Enterotype", "Nationality", "Gender", "Age", "ClinicalStatus"),
      description = "Pre-analyzed subset of enterotype data with universal transformations (R² = 0.901)",
      citation = "Arumugam et al. (2011) Nature 473:174-180",
      file = "enterotype_demo_universal.rds",
      tags = c("human gut", "demo", "precomputed", "enterotypes", "universal analysis"),
      loader = "load_precomputed"
    ),
    
    soilrep_demo = list(
      id = "soilrep_demo",
      name = "Soil Microbiome (Precomputed Demo)",
      type = "16S",
      source = "precomputed",
      samples = 56,
      taxa = 2000,
      has_tree = FALSE,
      has_metadata = TRUE,
      metadata_vars = c("Treatment", "warmed", "clipped", "Sample"),
      description = "Pre-analyzed soil experiment with universal transformations (R² = 0.956)",
      citation = "Shade et al. (2012) ISME J 7:540-544",
      file = "soilrep_demo_universal.rds",
      tags = c("soil", "demo", "precomputed", "climate change", "universal analysis"),
      loader = "load_precomputed"
    )
    
    # Additional datasets available for processing:
    # - hmp_gut_16s: Human Microbiome Project gut samples
    # - curatedmgd_ibd: IBD metagenomes from curatedMetagenomicData
    # - moving_pictures: Daily sampling time series  
    # - emp_soils: Global soil microbiome survey
    # Run data processing scripts in data-raw/ to enable these datasets
  ),
  
  external = list(
    curatedMetagenomicData = list(
      id = "curatedMetagenomicData",
      name = "curatedMetagenomicData Collection",
      type = "shotgun",
      source = "bioconductor",
      description = "20,000+ standardized human microbiome shotgun metagenomes",
      package = "curatedMetagenomicData",
      loader = "load_curated_mgd",
      requires = c("curatedMetagenomicData", "TreeSummarizedExperiment"),
      tags = c("human", "shotgun", "large-scale", "standardized", "meta-analysis"),
      datasets = list(
        healthy_gut = "Healthy human gut baseline",
        ibd_studies = "Inflammatory bowel disease cohorts",
        crc_studies = "Colorectal cancer cohorts",
        t2d_studies = "Type 2 diabetes cohorts"
      )
    ),
    
    microbiome_pkg = list(
      id = "microbiome_pkg",
      name = "microbiome Package Datasets", 
      type = "16S",
      source = "github",
      description = "Additional curated 16S datasets from microbiome R package",
      package = "microbiome",
      loader = "load_microbiome_pkg",
      requires = "microbiome",
      tags = c("16S", "curated", "diverse"),
      datasets = list(
        atlas1006 = "1006 western adults gut microbiome",
        dietswap = "Diet swap experiment (African vs Western)",
        peerj32 = "Peer-reviewed microbiome study"
      )
    ),
    
    hmp16sdata = list(
      id = "hmp16sdata",
      name = "HMP 16S Data Package",
      type = "16S", 
      source = "bioconductor",
      description = "Human Microbiome Project 16S rRNA sequencing data",
      package = "HMP16SData",
      loader = "load_hmp16s",
      requires = "HMP16SData",
      tags = c("HMP", "16S", "human", "multi-site", "healthy baseline"),
      datasets = list(
        V13 = "16S V1-V3 region data",
        V35 = "16S V3-V5 region data"
      )
    ),
    
    # Example format conversion datasets
    example_biom = list(
      id = "example_biom",
      name = "Example BIOM Format Dataset",
      type = "16S",
      source = "external",
      description = "Example dataset in BIOM format for testing format converters",
      file_path = "inst/example_data/example_table.biom",
      file_format = "biom",
      loader = "load_format_converted",
      tags = c("example", "biom", "format test"),
      samples = NA,
      taxa = NA
    ),
    
    example_qiime2 = list(
      id = "example_qiime2", 
      name = "Example QIIME2 Artifact",
      type = "16S",
      source = "external",
      description = "Example QIIME2 feature table artifact (.qza)",
      file_path = "inst/example_data/feature_table.qza",
      file_format = "qiime2",
      loader = "load_format_converted",
      tags = c("example", "qiime2", "format test"),
      samples = NA,
      taxa = NA
    ),
    
    example_metaphlan = list(
      id = "example_metaphlan",
      name = "Example MetaPhlAn Profile", 
      type = "shotgun",
      source = "external",
      description = "Example MetaPhlAn taxonomic profile output",
      file_path = "inst/example_data/metaphlan_profile.txt",
      file_format = "metaphlan",
      loader = "load_format_converted",
      tags = c("example", "metaphlan", "shotgun", "format test"),
      samples = NA,
      taxa = NA
    )
  )
)

#' Get dataset information from registry
#'
#' @param dataset_id Character string identifying the dataset
#' @param source Optional source category to search within
#'
#' @return List containing dataset metadata, or NULL if not found
#'
#' @export
#' @examples
#' # Get GlobalPatterns info
#' get_dataset_info("globalpatterns")
#' 
#' # Get info from specific source
#' get_dataset_info("hmp_gut_16s", source = "precomputed")
#'
get_dataset_info <- function(dataset_id, source = NULL) {
  dataset_id <- tolower(dataset_id)
  
  if (!is.null(source)) {
    # Search in specific source
    if (source %in% names(diversityGPT_datasets)) {
      datasets <- diversityGPT_datasets[[source]]
      if (dataset_id %in% names(datasets)) {
        return(datasets[[dataset_id]])
      }
    }
    return(NULL)
  }
  
  # Search all sources
  for (source_name in names(diversityGPT_datasets)) {
    datasets <- diversityGPT_datasets[[source_name]]
    if (dataset_id %in% names(datasets)) {
      info <- datasets[[dataset_id]]
      info$source_category <- source_name
      return(info)
    }
  }
  
  return(NULL)
}

#' List available datasets
#'
#' @param type Filter by data type ("16S", "shotgun", or NULL for all)
#' @param source Filter by source category
#' @param tags Filter by tags (character vector)
#'
#' @return Data frame with dataset information
#'
#' @export
#' @examples
#' # List all datasets
#' list_available_datasets()
#' 
#' # List only 16S datasets
#' list_available_datasets(type = "16S")
#' 
#' # List datasets with specific tags
#' list_available_datasets(tags = c("human gut", "healthy"))
#'
list_available_datasets <- function(type = NULL, source = NULL, tags = NULL) {
  datasets_list <- list()
  
  # Determine which sources to search
  sources_to_search <- if (!is.null(source)) {
    intersect(source, names(diversityGPT_datasets))
  } else {
    names(diversityGPT_datasets)
  }
  
  # Collect all datasets
  for (source_name in sources_to_search) {
    source_datasets <- diversityGPT_datasets[[source_name]]
    
    for (dataset_name in names(source_datasets)) {
      dataset <- source_datasets[[dataset_name]]
      
      # Apply filters
      if (!is.null(type) && dataset$type != type) next
      
      if (!is.null(tags)) {
        if (!any(tags %in% dataset$tags)) next
      }
      
      # Add to list
      datasets_list[[length(datasets_list) + 1]] <- data.frame(
        id = dataset$id,
        name = dataset$name,
        type = dataset$type,
        source = source_name,
        samples = ifelse(is.null(dataset$samples), NA, dataset$samples),
        taxa = ifelse(is.null(dataset$taxa), NA, dataset$taxa),
        has_tree = ifelse(is.null(dataset$has_tree), FALSE, dataset$has_tree),
        description = dataset$description,
        stringsAsFactors = FALSE
      )
    }
  }
  
  # Combine into single data frame
  if (length(datasets_list) > 0) {
    do.call(rbind, datasets_list)
  } else {
    data.frame(
      id = character(0),
      name = character(0),
      type = character(0),
      source = character(0),
      samples = numeric(0),
      taxa = numeric(0),
      has_tree = logical(0),
      description = character(0),
      stringsAsFactors = FALSE
    )
  }
}

#' Search datasets by keyword
#'
#' @param keyword Search term to find in name, description, or tags
#' @param fields Which fields to search in
#'
#' @return Data frame with matching datasets
#'
#' @export
#' @examples
#' # Search for gut-related datasets
#' search_datasets("gut")
#' 
#' # Search for time series data
#' search_datasets("longitudinal")
#'
search_datasets <- function(keyword, fields = c("name", "description", "tags")) {
  keyword <- tolower(keyword)
  matches <- list()
  
  for (source_name in names(diversityGPT_datasets)) {
    source_datasets <- diversityGPT_datasets[[source_name]]
    
    for (dataset_name in names(source_datasets)) {
      dataset <- source_datasets[[dataset_name]]
      found <- FALSE
      
      # Search in specified fields
      if ("name" %in% fields && !is.null(dataset$name)) {
        if (grepl(keyword, tolower(dataset$name))) found <- TRUE
      }
      
      if ("description" %in% fields && !is.null(dataset$description)) {
        if (grepl(keyword, tolower(dataset$description))) found <- TRUE
      }
      
      if ("tags" %in% fields && !is.null(dataset$tags)) {
        if (any(grepl(keyword, tolower(dataset$tags)))) found <- TRUE
      }
      
      if (found) {
        matches[[length(matches) + 1]] <- data.frame(
          id = dataset$id,
          name = dataset$name,
          type = dataset$type,
          source = source_name,
          samples = ifelse(is.null(dataset$samples), NA, dataset$samples),
          description = dataset$description,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  if (length(matches) > 0) {
    do.call(rbind, matches)
  } else {
    data.frame()
  }
}