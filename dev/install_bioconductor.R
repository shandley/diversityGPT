# Install Bioconductor packages (phyloseq)

if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}

# Install phyloseq
BiocManager::install("phyloseq", update = FALSE, ask = FALSE)

cli::cli_alert_success("phyloseq installed from Bioconductor")