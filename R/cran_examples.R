#' CRAN-Safe Examples
#' 
#' @description Internal data and functions for CRAN-compliant examples
#' @name cran_examples
#' @keywords internal
NULL

#' Create Small Demo Dataset
#'
#' Creates a minimal phyloseq object for examples that run quickly
#'
#' @param n_samples Number of samples (default: 5)
#' @param n_taxa Number of taxa (default: 20) 
#' @param seed Random seed for reproducibility
#'
#' @return A phyloseq object
#' @export
#' @examples
#' # Create demo data
#' demo_data <- create_demo_phyloseq()
#' 
#' # Check structure
#' print(demo_data)
#' phyloseq::nsamples(demo_data)
#' phyloseq::ntaxa(demo_data)
create_demo_phyloseq <- function(n_samples = 5, n_taxa = 20, seed = 42) {
  set.seed(seed)
  
  # Create OTU table
  otu_mat <- matrix(
    rpois(n_samples * n_taxa, lambda = 5),
    nrow = n_taxa,
    ncol = n_samples
  )
  rownames(otu_mat) <- paste0("OTU", seq_len(n_taxa))
  colnames(otu_mat) <- paste0("Sample", seq_len(n_samples))
  
  # Create sample data
  sample_df <- data.frame(
    Treatment = rep(c("Control", "Treatment"), length.out = n_samples),
    Timepoint = seq_len(n_samples),
    row.names = colnames(otu_mat)
  )
  
  # Create taxonomy table  
  tax_mat <- matrix(
    paste0("Taxa", seq_len(n_taxa * 7)),
    nrow = n_taxa,
    ncol = 7
  )
  rownames(tax_mat) <- rownames(otu_mat)
  colnames(tax_mat) <- c("Kingdom", "Phylum", "Class", "Order", 
                         "Family", "Genus", "Species")
  
  # Create phyloseq object
  phyloseq::phyloseq(
    phyloseq::otu_table(otu_mat, taxa_are_rows = TRUE),
    phyloseq::sample_data(sample_df),
    phyloseq::tax_table(tax_mat)
  )
}

#' Create Demo Subset
#'
#' Creates a subset of a phyloseq object for faster examples
#'
#' @param physeq A phyloseq object
#' @param n_samples Number of samples to keep
#' @param n_taxa Number of taxa to keep
#' @param seed Random seed
#'
#' @return A smaller phyloseq object
#' @export
#' @examples
#' # Create a demo dataset first
#' demo_full <- create_demo_phyloseq(n_samples = 20, n_taxa = 100)
#' 
#' # Create a smaller subset
#' demo_small <- create_demo_subset(demo_full, n_samples = 5, n_taxa = 20)
#' 
#' # Check dimensions
#' phyloseq::nsamples(demo_small)
#' phyloseq::ntaxa(demo_small)
create_demo_subset <- function(physeq, n_samples = 5, n_taxa = 20, seed = 42) {
  set.seed(seed)
  
  # Get current dimensions
  curr_samples <- phyloseq::nsamples(physeq)
  curr_taxa <- phyloseq::ntaxa(physeq)
  
  # Sample indices
  keep_samples <- sample(curr_samples, min(n_samples, curr_samples))
  keep_taxa <- sample(curr_taxa, min(n_taxa, curr_taxa))
  
  # Subset
  physeq_sub <- phyloseq::prune_samples(
    phyloseq::sample_names(physeq)[keep_samples], 
    physeq
  )
  physeq_sub <- phyloseq::prune_taxa(
    phyloseq::taxa_names(physeq)[keep_taxa],
    physeq_sub
  )
  
  return(physeq_sub)
}