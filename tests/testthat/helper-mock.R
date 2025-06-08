# Helper functions for testing with mocked API calls

# Mock LLM response
mock_llm_response <- function(content = "This is a mock response") {
  list(
    content = list(
      list(
        text = content
      )
    )
  )
}

# Mock diversity data
create_mock_diversity_data <- function(n_samples = 10, n_metrics = 3) {
  metrics <- c("shannon", "simpson", "chao1", "faith_pd", "observed_otus")[seq_len(n_metrics)]
  
  data <- matrix(
    runif(n_samples * n_metrics, min = 0, max = 5),
    nrow = n_samples,
    ncol = n_metrics
  )
  
  colnames(data) <- metrics
  rownames(data) <- paste0("Sample", seq_len(n_samples))
  
  as.data.frame(data)
}

# Mock phyloseq object
create_mock_phyloseq <- function(n_samples = 10, n_taxa = 50) {
  # Note: This creates a simplified mock object
  # In real tests, you might want to use actual phyloseq objects
  
  otu_matrix <- matrix(
    rpois(n_samples * n_taxa, lambda = 5),
    nrow = n_taxa,
    ncol = n_samples
  )
  
  colnames(otu_matrix) <- paste0("Sample", seq_len(n_samples))
  rownames(otu_matrix) <- paste0("OTU", seq_len(n_taxa))
  
  list(
    otu_table = otu_matrix,
    n_samples = n_samples,
    n_taxa = n_taxa,
    class = "mock_phyloseq"
  )
}