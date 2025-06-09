# diversityGPT Dataset Expansion Implementation Plan

## Overview
This plan outlines the strategy to expand diversityGPT's dataset capabilities from 3 built-in phyloseq datasets to a comprehensive collection including 16S amplicon, shotgun metagenomics, and environmental microbiome data.

## Current Status
- ✅ Bug Fix Applied: All three phyloseq datasets (GlobalPatterns, enterotype, soilrep) now load properly
- ✅ 16S datasets confirmed working with phylogenetic trees where available
- ✅ Universal Framework works with any OTU/ASV table format

## Phase 1: Enhanced Dataset System (Week 1)

### 1.1 Dataset Registry Infrastructure
Create a centralized dataset registry with metadata:

```r
# R/dataset_registry.R
diversityGPT_datasets <- list(
  builtin = list(
    globalpatterns = list(
      name = "Global Patterns of 16S Diversity",
      type = "16S",
      source = "phyloseq",
      samples = 26,
      taxa = 19216,
      has_tree = TRUE,
      has_metadata = TRUE,
      metadata_vars = c("SampleType", "Description"),
      description = "Global survey of bacterial/archaeal diversity",
      citation = "Caporaso et al. 2011 PNAS",
      tags = c("environmental", "survey", "16S V4")
    ),
    enterotype = list(
      name = "Human Gut Enterotypes",
      type = "16S", 
      source = "phyloseq",
      samples = 280,
      taxa = 553,
      has_tree = FALSE,
      has_metadata = TRUE,
      metadata_vars = c("Enterotype", "Nationality", "Gender", "Age"),
      description = "Human gut microbiome enterotypes study",
      citation = "Arumugam et al. 2011 Nature",
      tags = c("human gut", "enterotypes", "16S V3-V4")
    ),
    soilrep = list(
      name = "Soil Microbiome Reproducibility",
      type = "16S",
      source = "phyloseq",
      samples = 56,
      taxa = 16825,
      has_tree = FALSE,
      has_metadata = TRUE,
      metadata_vars = c("Treatment", "warmed", "clipped"),
      description = "Soil warming experiment reproducibility",
      citation = "Shade et al. 2012 ISME J",
      tags = c("soil", "environmental", "climate change")
    )
  ),
  
  precomputed = list(
    hmp_gut_16s = list(
      name = "Human Microbiome Project - Gut 16S",
      type = "16S",
      source = "precomputed",
      samples = 300,
      taxa = 5000,
      has_tree = TRUE,
      has_metadata = TRUE,
      metadata_vars = c("visit_number", "sex", "body_site"),
      description = "Healthy human gut microbiome baseline",
      file = "hmp_gut_16s_universal.rda",
      tags = c("human gut", "healthy", "HMP", "16S V3-V5")
    ),
    
    curatedmgd_ibd = list(
      name = "IBD Metagenomes (curatedMetagenomicData)",
      type = "shotgun",
      source = "precomputed", 
      samples = 200,
      taxa = 800,
      has_tree = FALSE,
      has_metadata = TRUE,
      metadata_vars = c("disease", "age", "antibiotics", "study"),
      description = "Inflammatory bowel disease shotgun metagenomes",
      file = "cmd_ibd_universal.rda",
      tags = c("human gut", "IBD", "disease", "shotgun", "WGS")
    ),
    
    emp_soils = list(
      name = "Earth Microbiome Project - Global Soils",
      type = "16S",
      source = "precomputed",
      samples = 500,
      taxa = 10000,
      has_tree = TRUE,
      has_metadata = TRUE,
      metadata_vars = c("ph", "temperature", "latitude", "longitude", "biome"),
      description = "Global soil microbiome survey",
      file = "emp_soils_universal.rda",
      tags = c("soil", "environmental", "global", "EMP", "16S V4")
    )
  ),
  
  external = list(
    curatedMetagenomicData = list(
      name = "curatedMetagenomicData Collection",
      type = "shotgun",
      source = "bioconductor",
      description = "20,000+ standardized human microbiome samples",
      loader = "load_curated_mgd",
      requires = "curatedMetagenomicData",
      tags = c("human", "shotgun", "large-scale")
    ),
    
    microbiome_pkg = list(
      name = "microbiome Package Datasets", 
      type = "16S",
      source = "cran",
      description = "Additional curated 16S datasets",
      loader = "load_microbiome_pkg",
      requires = "microbiome",
      tags = c("16S", "curated")
    )
  )
)
```

### 1.2 Dataset Loader Functions
```r
# R/dataset_loaders.R

#' Load dataset from registry
#' @export
load_dataset <- function(dataset_id, source = NULL) {
  
  # Find dataset in registry
  dataset_info <- find_dataset(dataset_id, source)
  
  if (is.null(dataset_info)) {
    stop("Dataset not found: ", dataset_id)
  }
  
  # Load based on source type
  switch(dataset_info$source,
    "phyloseq" = load_phyloseq_builtin(dataset_id),
    "precomputed" = load_precomputed(dataset_info$file),
    "bioconductor" = load_from_bioconductor(dataset_id),
    "cran" = load_from_cran(dataset_id),
    "url" = load_from_url(dataset_info$url),
    stop("Unknown source type: ", dataset_info$source)
  )
}

#' Load precomputed universal results
#' @export  
load_precomputed_universal <- function(dataset_id) {
  dataset_info <- find_dataset(dataset_id, "precomputed")
  
  if (is.null(dataset_info$file)) {
    stop("No precomputed file for dataset: ", dataset_id)
  }
  
  # Load from package data
  data_file <- system.file("data", dataset_info$file, package = "diversityGPT")
  
  if (!file.exists(data_file)) {
    stop("Precomputed file not found. Run data-raw scripts to generate.")
  }
  
  readRDS(data_file)
}
```

## Phase 2: Precomputed Dataset Generation (Week 1-2)

### 2.1 Data Processing Scripts
Create `data-raw/` directory with processing scripts:

```r
# data-raw/01_process_hmp.R
library(diversityGPT)
library(phyloseq)
library(HMP16SData)

# Load HMP gut samples
hmp_gut <- load_hmp_gut_16s()

# Extract universal information
hmp_universal <- extract_universal_information(
  hmp_gut,
  groups = "visit_number",
  include_phylogenetic = TRUE
)

# Add metadata
hmp_universal$dataset_info <- list(
  name = "HMP Gut 16S",
  processing_date = Sys.Date(),
  version = packageVersion("diversityGPT")
)

# Save
saveRDS(hmp_universal, "inst/data/hmp_gut_16s_universal.rda")
```

```r
# data-raw/02_process_curatedMGD.R  
library(curatedMetagenomicData)
library(diversityGPT)

# Get IBD studies
ibd_data <- curatedMetagenomicData(
  "LewisJD_2015|NielsenHB_2014|HallAB_2017",
  dryrun = FALSE,
  counts = TRUE,
  bugs.as.phyloseq = TRUE
)

# Convert to phyloseq and merge
ibd_phyloseq <- merge_phyloseq_list(ibd_data)

# Extract universal information
ibd_universal <- extract_universal_information(
  ibd_phyloseq,
  groups = "disease", 
  include_phylogenetic = FALSE  # No tree for shotgun
)

# Save
saveRDS(ibd_universal, "inst/data/cmd_ibd_universal.rda")
```

### 2.2 Automated Processing Pipeline
```r
# data-raw/process_all_datasets.R
source("data-raw/dataset_processors.R")

datasets_to_process <- list(
  list(id = "hmp_gut_16s", processor = process_hmp_gut),
  list(id = "cmd_ibd", processor = process_cmd_ibd),
  list(id = "emp_soils", processor = process_emp_soils),
  list(id = "metaphlan_profiles", processor = process_metaphlan)
)

for (dataset in datasets_to_process) {
  message("Processing ", dataset$id, "...")
  
  tryCatch({
    result <- dataset$processor()
    save_path <- file.path("inst/data", paste0(dataset$id, "_universal.rda"))
    saveRDS(result, save_path)
    message("✓ Saved to ", save_path)
  }, error = function(e) {
    warning("✗ Failed to process ", dataset$id, ": ", e$message)
  })
}
```

## Phase 3: Enhanced Shiny UI (Week 2)

### 3.1 Dataset Browser UI
```r
# inst/shiny/diversity_explorer/modules/dataset_browser.R

dataset_browser_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Dataset Browser"),
    
    # Filter controls
    fluidRow(
      column(4,
        selectInput(ns("data_type"), "Data Type:",
          choices = c("All" = "all", "16S" = "16s", "Shotgun" = "shotgun"),
          selected = "all"
        )
      ),
      column(4,
        selectInput(ns("sample_type"), "Sample Type:",
          choices = c("All" = "all", "Human Gut" = "human_gut", 
                      "Soil" = "soil", "Marine" = "marine"),
          selected = "all"
        )
      ),
      column(4,
        selectInput(ns("source"), "Source:",
          choices = c("All" = "all", "Built-in" = "builtin",
                      "Precomputed" = "precomputed", "External" = "external"),
          selected = "all"
        )
      )
    ),
    
    # Dataset cards
    div(
      id = ns("dataset_cards"),
      class = "dataset-grid",
      uiOutput(ns("dataset_display"))
    ),
    
    # Selected dataset info
    conditionalPanel(
      condition = paste0("output['", ns("dataset_selected"), "']"),
      wellPanel(
        h4("Selected Dataset"),
        uiOutput(ns("dataset_details")),
        actionButton(ns("load_dataset"), "Load This Dataset", 
                    class = "btn-primary btn-lg")
      )
    )
  )
}
```

### 3.2 Dataset Cards Display
```r
create_dataset_card <- function(dataset_id, dataset_info) {
  tags <- paste(dataset_info$tags, collapse = ", ")
  
  div(
    class = "dataset-card",
    `data-dataset-id` = dataset_id,
    
    div(class = "dataset-card-header",
      h4(dataset_info$name),
      span(class = paste0("badge badge-", dataset_info$type), 
           toupper(dataset_info$type))
    ),
    
    div(class = "dataset-card-body",
      p(dataset_info$description),
      
      div(class = "dataset-stats",
        span(icon("vial"), dataset_info$samples, "samples"),
        span(icon("bacteria"), dataset_info$taxa, "taxa"),
        if (dataset_info$has_tree) span(icon("tree"), "Phylogeny") else NULL
      ),
      
      div(class = "dataset-tags",
        tags
      )
    ),
    
    div(class = "dataset-card-footer",
      if (!is.null(dataset_info$citation)) {
        tags$small(dataset_info$citation)
      }
    )
  )
}
```

## Phase 4: Data Format Converters (Week 2-3)

### 4.1 Universal Data Importer
```r
# R/data_importers.R

#' Import microbiome data from various formats
#' @export
import_microbiome_data <- function(file_path, format = "auto", ...) {
  
  if (format == "auto") {
    format <- detect_format(file_path)
  }
  
  importer <- switch(format,
    "biom" = import_biom,
    "qiime2" = import_qiime2,
    "metaphlan" = import_metaphlan,
    "tsv" = import_tsv,
    "csv" = import_csv,
    "phyloseq" = readRDS,
    stop("Unsupported format: ", format)
  )
  
  # Import data
  data <- importer(file_path, ...)
  
  # Convert to phyloseq if needed
  if (!inherits(data, "phyloseq")) {
    data <- convert_to_phyloseq(data, format)
  }
  
  # Validate
  validate_phyloseq(data)
  
  return(data)
}

#' Import MetaPhlAn profiles
import_metaphlan <- function(file_path, tax_level = "species") {
  # Read MetaPhlAn output
  profiles <- read.table(file_path, header = TRUE, sep = "\t", 
                        row.names = 1, check.names = FALSE)
  
  # Filter to desired taxonomic level
  keep_rows <- grepl(paste0("\\|", tax_level[1], "__"), rownames(profiles))
  profiles <- profiles[keep_rows, ]
  
  # Create OTU table
  otu <- otu_table(as.matrix(profiles), taxa_are_rows = TRUE)
  
  # Parse taxonomy
  tax <- parse_metaphlan_taxonomy(rownames(profiles))
  
  # Create phyloseq
  phyloseq(otu, tax)
}
```

### 4.2 Batch Processing Support
```r
#' Process multiple datasets in parallel
#' @export
batch_process_datasets <- function(dataset_list, ncores = 4) {
  
  # Setup parallel processing
  cl <- makeCluster(ncores)
  registerDoParallel(cl)
  
  results <- foreach(dataset = dataset_list, 
                    .packages = c("diversityGPT", "phyloseq")) %dopar% {
    
    tryCatch({
      # Load dataset
      physeq <- load_dataset(dataset$id, dataset$source)
      
      # Extract universal information
      universal_info <- extract_universal_information(
        physeq,
        include_phylogenetic = dataset$has_tree
      )
      
      # Return result
      list(
        dataset_id = dataset$id,
        success = TRUE,
        universal_info = universal_info,
        quality = universal_info$deconvolution_quality$overall_quality
      )
      
    }, error = function(e) {
      list(
        dataset_id = dataset$id,
        success = FALSE,
        error = e$message
      )
    })
  }
  
  stopCluster(cl)
  
  return(results)
}
```

## Phase 5: Caching and Performance (Week 3)

### 5.1 Smart Caching System
```r
# R/cache_manager.R

#' Cache manager for dataset operations
DiversityGPTCache <- R6Class("DiversityGPTCache",
  public = list(
    cache_dir = NULL,
    max_size_gb = 5,
    
    initialize = function(cache_dir = NULL) {
      if (is.null(cache_dir)) {
        cache_dir <- file.path(rappdirs::user_cache_dir("diversityGPT"), "datasets")
      }
      self$cache_dir <- cache_dir
      dir.create(self$cache_dir, recursive = TRUE, showWarnings = FALSE)
    },
    
    get = function(key) {
      cache_file <- file.path(self$cache_dir, paste0(key, ".rds"))
      if (file.exists(cache_file)) {
        # Check age
        if (difftime(Sys.time(), file.mtime(cache_file), units = "days") < 30) {
          return(readRDS(cache_file))
        }
      }
      return(NULL)
    },
    
    set = function(key, value) {
      # Check cache size
      self$check_size()
      
      cache_file <- file.path(self$cache_dir, paste0(key, ".rds"))
      saveRDS(value, cache_file)
    },
    
    check_size = function() {
      # Get cache size
      files <- list.files(self$cache_dir, full.names = TRUE)
      sizes <- file.info(files)$size
      total_size <- sum(sizes, na.rm = TRUE) / 1e9  # GB
      
      # Clean if too large
      if (total_size > self$max_size_gb) {
        # Remove oldest files
        mtimes <- file.info(files)$mtime
        oldest <- order(mtimes)[1:floor(length(files) * 0.3)]
        unlink(files[oldest])
      }
    }
  )
)

# Global cache instance
diversity_cache <- DiversityGPTCache$new()
```

### 5.2 Lazy Loading for Large Datasets
```r
#' Lazy phyloseq object for large datasets
LazyPhyloseq <- R6Class("LazyPhyloseq",
  public = list(
    otu_file = NULL,
    sample_file = NULL,
    tax_file = NULL,
    tree_file = NULL,
    loaded = FALSE,
    physeq = NULL,
    
    initialize = function(otu_file, sample_file = NULL, 
                         tax_file = NULL, tree_file = NULL) {
      self$otu_file <- otu_file
      self$sample_file <- sample_file
      self$tax_file <- tax_file
      self$tree_file <- tree_file
    },
    
    load = function() {
      if (!self$loaded) {
        # Load components
        otu <- read.table(self$otu_file, header = TRUE, row.names = 1)
        otu <- otu_table(as.matrix(otu), taxa_are_rows = TRUE)
        
        components <- list(otu)
        
        if (!is.null(self$sample_file)) {
          samp <- read.table(self$sample_file, header = TRUE, row.names = 1)
          components <- append(components, sample_data(samp))
        }
        
        # Create phyloseq
        self$physeq <- do.call(phyloseq, components)
        self$loaded <- TRUE
      }
      
      return(self$physeq)
    },
    
    subset = function(samples = NULL, taxa = NULL) {
      # Load only if needed
      if (!is.null(samples) || !is.null(taxa)) {
        physeq <- self$load()
        
        if (!is.null(samples)) {
          physeq <- prune_samples(samples, physeq)
        }
        
        if (!is.null(taxa)) {
          physeq <- prune_taxa(taxa, physeq)
        }
        
        return(physeq)
      }
    }
  )
)
```

## Phase 6: Documentation and Examples (Week 3-4)

### 6.1 Dataset Vignettes
Create comprehensive vignettes for each dataset type:

- `vignettes/datasets-16s.Rmd`: Working with 16S amplicon data
- `vignettes/datasets-shotgun.Rmd`: Shotgun metagenomics analysis
- `vignettes/datasets-timeseries.Rmd`: Longitudinal microbiome studies
- `vignettes/datasets-large-scale.Rmd`: Handling large datasets efficiently

### 6.2 Gallery of Examples
```r
# inst/examples/dataset_gallery.R

# Example 1: Compare 16S vs Shotgun on same samples
hmp_16s <- load_dataset("hmp_gut_16s", "precomputed")
hmp_shotgun <- load_dataset("hmp_gut_shotgun", "precomputed")

comparison <- compare_data_types(hmp_16s, hmp_shotgun)
plot_data_type_comparison(comparison)

# Example 2: Global patterns across environments
datasets <- c("globalpatterns", "emp_soils", "emp_ocean", "emp_host")
global_patterns <- analyze_global_patterns(datasets)
plot_global_diversity_map(global_patterns)

# Example 3: Disease progression
ibd_data <- load_dataset("cmd_ibd", "precomputed")
progression <- track_diversity_progression(
  ibd_data,
  time_var = "week",
  subject_var = "subject_id"
)
plot_progression_trajectories(progression)
```

## Timeline Summary

### Week 1
- ✅ Fix current dataset loading bug (DONE)
- Implement dataset registry system
- Create basic dataset loaders
- Start precomputed data generation

### Week 2
- Complete precomputed datasets (HMP, curatedMGD, EMP)
- Implement enhanced Shiny dataset browser
- Add data format converters
- Test with real external data

### Week 3
- Implement caching system
- Add lazy loading for large datasets
- Create batch processing pipeline
- Write dataset vignettes

### Week 4
- Polish UI/UX
- Complete documentation
- Create example gallery
- Performance optimization

## Success Metrics

1. **Dataset Variety**: Support for 20+ diverse microbiome datasets
2. **Performance**: Load and analyze 1000-sample dataset in <30 seconds
3. **User Experience**: Intuitive dataset browser with filtering
4. **Format Support**: Import from 5+ common formats
5. **Documentation**: Comprehensive guides for each data type

## Future Extensions

1. **Real-time Data**: Connect to live sequencing pipelines
2. **Cloud Integration**: AWS/GCP bucket support
3. **Database Backend**: PostgreSQL for very large studies
4. **API Access**: RESTful API for programmatic access
5. **Multi-omics**: Integration with metabolomics, proteomics data