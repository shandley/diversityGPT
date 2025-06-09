#' Data Format Converters for diversityGPT
#'
#' Functions to convert various microbiome data formats to phyloseq objects
#'
#' @name format_converters
#' @import phyloseq
#' @import biomformat
#' @importFrom jsonlite fromJSON
NULL

#' Detect data format from file
#'
#' @param file_path Path to the data file
#' @return Character string indicating format: "biom", "qiime2", "metaphlan", "tsv", "unknown"
#' @export
detect_data_format <- function(file_path) {
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }
  
  # Get file extension
  ext <- tolower(tools::file_ext(file_path))
  
  # Check by extension first
  if (ext == "biom") {
    return("biom")
  } else if (ext == "qza" || ext == "qzv") {
    return("qiime2")
  } else if (ext %in% c("txt", "tsv", "csv")) {
    # Check content for MetaPhlAn format
    first_lines <- readLines(file_path, n = 5)
    if (any(grepl("^#SampleID|^#clade_name|k__Bacteria", first_lines))) {
      return("metaphlan")
    }
    return("tsv")
  }
  
  # Try to detect by content
  tryCatch({
    # Check if it's JSON (BIOM format)
    json_test <- jsonlite::fromJSON(file_path, simplifyVector = FALSE)
    if (!is.null(json_test$format) && grepl("Biological Observation Matrix", json_test$format)) {
      return("biom")
    }
  }, error = function(e) NULL)
  
  return("unknown")
}

#' Convert BIOM file to phyloseq
#'
#' @param biom_file Path to BIOM file
#' @param tree_file Optional path to tree file (Newick format)
#' @param sample_metadata Optional data frame or path to sample metadata file
#' @param parse_taxonomy Logical, whether to parse taxonomy from observation metadata
#' @return phyloseq object
#' @export
#' @examples
#' \dontrun{
#' # Convert BIOM file
#' ps <- biom_to_phyloseq("otu_table.biom", 
#'                       tree_file = "tree.nwk",
#'                       sample_metadata = "sample_data.tsv")
#' }
biom_to_phyloseq <- function(biom_file, 
                            tree_file = NULL, 
                            sample_metadata = NULL,
                            parse_taxonomy = TRUE) {
  
  message("Converting BIOM file to phyloseq...")
  
  # Check if biomformat is available
  if (!requireNamespace("biomformat", quietly = TRUE)) {
    stop("Package 'biomformat' is required. Install with: BiocManager::install('biomformat')")
  }
  
  # Read BIOM file
  biom_data <- biomformat::read_biom(biom_file)
  
  # Extract OTU table
  otu_table <- as(biomformat::biom_data(biom_data), "matrix")
  otu_table <- otu_table(otu_table, taxa_are_rows = TRUE)
  
  # Initialize phyloseq components
  components <- list(otu_table)
  
  # Extract taxonomy if available
  if (parse_taxonomy) {
    obs_metadata <- biomformat::observation_metadata(biom_data)
    if (!is.null(obs_metadata) && length(obs_metadata) > 0) {
      # Try to parse taxonomy
      tax_table <- parse_biom_taxonomy(obs_metadata)
      if (!is.null(tax_table)) {
        components[[length(components) + 1]] <- tax_table
        message("  Parsed taxonomy for ", nrow(tax_table), " taxa")
      }
    }
  }
  
  # Add sample metadata
  if (!is.null(sample_metadata)) {
    if (is.character(sample_metadata)) {
      # Read from file
      sample_data <- read_sample_metadata(sample_metadata)
    } else if (is.data.frame(sample_metadata)) {
      sample_data <- sample_data(sample_metadata)
    }
    
    # Match sample names
    common_samples <- intersect(rownames(sample_data), sample_names(otu_table))
    if (length(common_samples) == 0) {
      warning("No matching sample names between OTU table and metadata")
    } else {
      sample_data <- prune_samples(common_samples, sample_data)
      components[[length(components) + 1]] <- sample_data
      message("  Added sample metadata for ", length(common_samples), " samples")
    }
  }
  
  # Add tree if provided
  if (!is.null(tree_file) && file.exists(tree_file)) {
    tree <- read_tree(tree_file)
    components[[length(components) + 1]] <- tree
    message("  Added phylogenetic tree")
  }
  
  # Merge components
  physeq <- do.call(phyloseq, components)
  
  message("Created phyloseq object: ", 
          nsamples(physeq), " samples, ", 
          ntaxa(physeq), " taxa")
  
  return(physeq)
}

#' Convert QIIME2 artifact to phyloseq
#'
#' @param qza_file Path to QIIME2 artifact file (.qza)
#' @param temp_dir Temporary directory for extraction
#' @return phyloseq object or data depending on artifact type
#' @export
#' @examples
#' \dontrun{
#' # Convert QIIME2 feature table
#' ps <- qiime2_to_phyloseq("table.qza")
#' 
#' # Convert with multiple artifacts
#' ps <- merge_qiime2_artifacts(
#'   feature_table = "table.qza",
#'   taxonomy = "taxonomy.qza",
#'   tree = "rooted-tree.qza",
#'   metadata = "sample-metadata.tsv"
#' )
#' }
qiime2_to_phyloseq <- function(qza_file, temp_dir = tempdir()) {
  
  if (!file.exists(qza_file)) {
    stop("QIIME2 artifact file not found: ", qza_file)
  }
  
  message("Extracting QIIME2 artifact...")
  
  # Create extraction directory
  extract_dir <- file.path(temp_dir, paste0("qiime2_", basename(qza_file)))
  dir.create(extract_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Extract QZA (it's a zip file)
  unzip(qza_file, exdir = extract_dir)
  
  # Read metadata to determine artifact type
  metadata_file <- file.path(extract_dir, "metadata.yaml")
  if (!file.exists(metadata_file)) {
    stop("Invalid QIIME2 artifact: missing metadata.yaml")
  }
  
  # Parse metadata (simplified - would need yaml package for full parsing)
  metadata_lines <- readLines(metadata_file)
  type_line <- grep("type:", metadata_lines, value = TRUE)[1]
  artifact_type <- gsub(".*type: *", "", type_line)
  
  # Handle different artifact types
  if (grepl("FeatureTable", artifact_type)) {
    # Feature table (OTU/ASV table)
    biom_file <- list.files(file.path(extract_dir, "data"), 
                           pattern = "\\.biom$", 
                           full.names = TRUE)[1]
    result <- biom_to_phyloseq(biom_file, parse_taxonomy = FALSE)
    
  } else if (grepl("Phylogeny", artifact_type)) {
    # Tree
    tree_file <- list.files(file.path(extract_dir, "data"), 
                           pattern = "\\.nwk$|\\.tree$", 
                           full.names = TRUE)[1]
    result <- read_tree(tree_file)
    
  } else if (grepl("FeatureData\\[Taxonomy\\]", artifact_type)) {
    # Taxonomy
    tax_file <- list.files(file.path(extract_dir, "data"), 
                          pattern = "\\.tsv$", 
                          full.names = TRUE)[1]
    result <- read_qiime2_taxonomy(tax_file)
    
  } else {
    stop("Unsupported QIIME2 artifact type: ", artifact_type)
  }
  
  # Clean up
  unlink(extract_dir, recursive = TRUE)
  
  return(result)
}

#' Merge multiple QIIME2 artifacts into phyloseq
#'
#' @param feature_table Path to feature table QZA
#' @param taxonomy Optional path to taxonomy QZA
#' @param tree Optional path to tree QZA
#' @param metadata Optional path to sample metadata TSV
#' @return phyloseq object
#' @export
merge_qiime2_artifacts <- function(feature_table,
                                  taxonomy = NULL,
                                  tree = NULL,
                                  metadata = NULL) {
  
  message("Merging QIIME2 artifacts into phyloseq...")
  
  # Start with feature table
  physeq <- qiime2_to_phyloseq(feature_table)
  
  # Add taxonomy
  if (!is.null(taxonomy)) {
    tax_table <- qiime2_to_phyloseq(taxonomy)
    
    # Match taxa
    common_taxa <- intersect(taxa_names(physeq), rownames(tax_table))
    if (length(common_taxa) > 0) {
      tax_table <- tax_table[common_taxa, , drop = FALSE]
      tax_table(physeq) <- tax_table(tax_table)
      message("  Added taxonomy for ", length(common_taxa), " taxa")
    }
  }
  
  # Add tree
  if (!is.null(tree)) {
    tree_obj <- qiime2_to_phyloseq(tree)
    
    # Match taxa in tree
    common_taxa <- intersect(taxa_names(physeq), tree_obj$tip.label)
    if (length(common_taxa) > 0) {
      physeq <- prune_taxa(common_taxa, physeq)
      phy_tree(physeq) <- tree_obj
      message("  Added phylogenetic tree")
    }
  }
  
  # Add metadata
  if (!is.null(metadata)) {
    sample_data <- read_sample_metadata(metadata)
    
    # Match samples
    common_samples <- intersect(sample_names(physeq), rownames(sample_data))
    if (length(common_samples) > 0) {
      physeq <- prune_samples(common_samples, physeq)
      sample_data(physeq) <- sample_data
      message("  Added metadata for ", length(common_samples), " samples")
    }
  }
  
  message("Created phyloseq object: ",
          nsamples(physeq), " samples, ",
          ntaxa(physeq), " taxa")
  
  return(physeq)
}

#' Convert MetaPhlAn profile to phyloseq
#'
#' @param metaphlan_file Path to MetaPhlAn output file
#' @param sample_metadata Optional sample metadata
#' @param tax_level Taxonomic level to use (default: all levels)
#' @return phyloseq object
#' @export
#' @examples
#' \dontrun{
#' # Convert single MetaPhlAn profile
#' ps <- metaphlan_to_phyloseq("sample1_profile.txt")
#' 
#' # Convert merged MetaPhlAn table
#' ps <- metaphlan_to_phyloseq("merged_abundance_table.txt",
#'                            sample_metadata = "metadata.tsv")
#' }
metaphlan_to_phyloseq <- function(metaphlan_file,
                                 sample_metadata = NULL,
                                 tax_level = NULL) {
  
  message("Converting MetaPhlAn profile to phyloseq...")
  
  # Read MetaPhlAn output
  mp_data <- read.table(metaphlan_file, 
                       header = TRUE, 
                       sep = "\t", 
                       comment.char = "",
                       stringsAsFactors = FALSE)
  
  # Check format
  if (!any(grepl("^k__|^d__", mp_data[,1]))) {
    stop("File does not appear to be MetaPhlAn format")
  }
  
  # Extract taxonomy and abundance
  taxonomy_col <- 1
  abundance_cols <- 2:ncol(mp_data)
  
  # Filter by taxonomic level if specified
  if (!is.null(tax_level)) {
    tax_level_map <- c("kingdom" = "k__", "phylum" = "p__", 
                      "class" = "c__", "order" = "o__",
                      "family" = "f__", "genus" = "g__", 
                      "species" = "s__")
    
    if (tax_level %in% names(tax_level_map)) {
      prefix <- tax_level_map[tax_level]
      # Keep only rows at specified level
      level_rows <- grepl(paste0(prefix, "[^|]+$"), mp_data[,taxonomy_col])
      mp_data <- mp_data[level_rows, , drop = FALSE]
    }
  }
  
  # Parse taxonomy
  tax_strings <- mp_data[, taxonomy_col]
  tax_matrix <- parse_metaphlan_taxonomy(tax_strings)
  
  # Create OTU table
  otu_matrix <- as.matrix(mp_data[, abundance_cols, drop = FALSE])
  rownames(otu_matrix) <- rownames(tax_matrix)
  
  # Handle single sample case
  if (length(abundance_cols) == 1) {
    sample_name <- gsub("_profile$", "", colnames(mp_data)[abundance_cols])
    colnames(otu_matrix) <- sample_name
  }
  
  # Create phyloseq components
  otu_table <- otu_table(otu_matrix, taxa_are_rows = TRUE)
  tax_table <- tax_table(tax_matrix)
  
  components <- list(otu_table, tax_table)
  
  # Add sample metadata if provided
  if (!is.null(sample_metadata)) {
    if (is.character(sample_metadata)) {
      sample_data <- read_sample_metadata(sample_metadata)
    } else {
      sample_data <- sample_data(sample_metadata)
    }
    
    # Match samples
    common_samples <- intersect(sample_names(otu_table), rownames(sample_data))
    if (length(common_samples) > 0) {
      components[[length(components) + 1]] <- sample_data[common_samples, , drop = FALSE]
    }
  }
  
  # Create phyloseq object
  physeq <- do.call(phyloseq, components)
  
  message("Created phyloseq object: ",
          nsamples(physeq), " samples, ",
          ntaxa(physeq), " taxa")
  
  return(physeq)
}

#' Helper function to parse BIOM taxonomy
#' @noRd
parse_biom_taxonomy <- function(obs_metadata) {
  if (is.null(obs_metadata)) return(NULL)
  
  # Try different taxonomy formats
  tax_matrix <- NULL
  
  # Format 1: taxonomy as a single string
  if ("taxonomy" %in% names(obs_metadata[[1]])) {
    tax_strings <- sapply(obs_metadata, function(x) {
      paste(x$taxonomy, collapse = "; ")
    })
    tax_matrix <- parse_taxonomy_strings(tax_strings)
  }
  
  # Format 2: separate rank columns
  rank_names <- c("kingdom", "phylum", "class", "order", "family", "genus", "species")
  rank_cols <- intersect(rank_names, names(obs_metadata[[1]]))
  
  if (length(rank_cols) > 0) {
    tax_matrix <- do.call(rbind, lapply(obs_metadata, function(x) {
      unlist(x[rank_cols])
    }))
    colnames(tax_matrix) <- rank_cols
  }
  
  if (!is.null(tax_matrix)) {
    return(tax_table(tax_matrix))
  }
  
  return(NULL)
}

#' Helper function to parse MetaPhlAn taxonomy strings
#' @noRd
parse_metaphlan_taxonomy <- function(tax_strings) {
  # Split by |
  tax_split <- strsplit(tax_strings, "\\|")
  
  # Get max number of levels
  max_levels <- max(sapply(tax_split, length))
  
  # Create matrix
  tax_matrix <- matrix(NA, 
                      nrow = length(tax_strings), 
                      ncol = max_levels)
  
  # Fill matrix
  for (i in seq_along(tax_split)) {
    levels <- tax_split[[i]]
    tax_matrix[i, 1:length(levels)] <- levels
  }
  
  # Assign column names based on prefixes
  col_names <- character(max_levels)
  for (j in 1:max_levels) {
    # Get first non-NA value to determine level
    first_val <- na.omit(tax_matrix[, j])[1]
    if (!is.na(first_val)) {
      prefix <- substr(first_val, 1, 3)
      level_name <- switch(prefix,
                          "k__" = "Kingdom",
                          "p__" = "Phylum", 
                          "c__" = "Class",
                          "o__" = "Order",
                          "f__" = "Family",
                          "g__" = "Genus",
                          "s__" = "Species",
                          paste0("Level", j))
      col_names[j] <- level_name
    }
  }
  
  colnames(tax_matrix) <- col_names
  
  # Create unique row names
  # Use the most specific level available
  row_names <- apply(tax_matrix, 1, function(x) {
    non_na <- na.omit(x)
    if (length(non_na) > 0) {
      # Remove prefix from last level
      last_level <- non_na[length(non_na)]
      gsub("^[a-z]__", "", last_level)
    } else {
      paste0("Taxa_", sample(1:10000, 1))
    }
  })
  
  # Make row names unique
  row_names <- make.unique(row_names)
  rownames(tax_matrix) <- row_names
  
  return(tax_matrix)
}

#' Helper function to read QIIME2 taxonomy
#' @noRd
read_qiime2_taxonomy <- function(tax_file) {
  tax_data <- read.table(tax_file, 
                        header = TRUE, 
                        sep = "\t",
                        stringsAsFactors = FALSE)
  
  # Parse taxonomy column
  tax_strings <- tax_data$Taxon
  tax_matrix <- parse_taxonomy_strings(tax_strings)
  
  # Use Feature ID as row names
  if ("Feature.ID" %in% names(tax_data)) {
    rownames(tax_matrix) <- tax_data$Feature.ID
  }
  
  return(tax_table(tax_matrix))
}

#' Helper function to parse taxonomy strings
#' @noRd
parse_taxonomy_strings <- function(tax_strings, sep = "; ") {
  # Split strings
  tax_split <- strsplit(tax_strings, sep)
  
  # Get number of levels
  n_levels <- max(sapply(tax_split, length))
  
  # Create matrix
  tax_matrix <- matrix(NA, 
                      nrow = length(tax_strings),
                      ncol = n_levels)
  
  # Fill matrix
  for (i in seq_along(tax_split)) {
    levels <- tax_split[[i]]
    tax_matrix[i, 1:length(levels)] <- levels
  }
  
  # Try to determine column names
  rank_names <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
  if (n_levels <= length(rank_names)) {
    colnames(tax_matrix) <- rank_names[1:n_levels]
  } else {
    colnames(tax_matrix) <- paste0("Rank", 1:n_levels)
  }
  
  return(tax_matrix)
}

#' Helper function to read sample metadata
#' @noRd
read_sample_metadata <- function(metadata_file) {
  # Detect separator
  first_line <- readLines(metadata_file, n = 1)
  sep <- if (grepl("\t", first_line)) "\t" else ","
  
  # Read metadata
  metadata <- read.table(metadata_file,
                        header = TRUE,
                        sep = sep,
                        stringsAsFactors = FALSE,
                        row.names = 1)
  
  return(sample_data(metadata))
}

#' Convert any supported format to phyloseq
#'
#' @param file_path Path to input file
#' @param format Optional format specification. If NULL, will auto-detect
#' @param ... Additional arguments passed to specific converters
#' @return phyloseq object
#' @export
#' @examples
#' \dontrun{
#' # Auto-detect format
#' ps <- convert_to_phyloseq("data.biom")
#' 
#' # Specify format
#' ps <- convert_to_phyloseq("profile.txt", format = "metaphlan")
#' 
#' # With additional options
#' ps <- convert_to_phyloseq("table.qza", 
#'                          taxonomy = "taxonomy.qza",
#'                          tree = "tree.qza")
#' }
convert_to_phyloseq <- function(file_path, format = NULL, ...) {
  
  # Auto-detect format if not specified
  if (is.null(format)) {
    format <- detect_data_format(file_path)
    message("Detected format: ", format)
  }
  
  # Convert based on format
  result <- switch(format,
    "biom" = biom_to_phyloseq(file_path, ...),
    "qiime2" = qiime2_to_phyloseq(file_path, ...),
    "metaphlan" = metaphlan_to_phyloseq(file_path, ...),
    stop("Unsupported format: ", format)
  )
  
  return(result)
}