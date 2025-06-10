#!/usr/bin/env Rscript
#' diversityGPT Performance Benchmark Script
#'
#' Run this script to benchmark diversityGPT performance on your system
#' Usage: Rscript benchmark_performance.R [dataset_size]
#'
#' Examples:
#'   Rscript benchmark_performance.R         # Use default size (medium)
#'   Rscript benchmark_performance.R small   # Small dataset (fast)
#'   Rscript benchmark_performance.R large   # Large dataset (slow)

library(diversityGPT)
library(phyloseq)

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)
dataset_size <- ifelse(length(args) > 0, args[1], "medium")

cat("=====================================\n")
cat("diversityGPT Performance Benchmarks\n")
cat("=====================================\n\n")

# System information
cat("System Information:\n")
cat("  R version:", R.version.string, "\n")
cat("  Platform:", R.version$platform, "\n")
cat("  CPU cores:", parallel::detectCores(), "\n")
cat("  diversityGPT version:", as.character(packageVersion("diversityGPT")), "\n")

# Memory info (if available)
if (.Platform$OS.type == "unix") {
  mem_info <- system("free -h | grep Mem", intern = TRUE, ignore.stderr = TRUE)
  if (length(mem_info) > 0) {
    cat("  Memory:", mem_info, "\n")
  }
}

cat("\n")

# Create test dataset based on size
cat("Creating test dataset (", dataset_size, ")...\n", sep = "")

if (dataset_size == "small") {
  n_samples <- 50
  n_taxa <- 500
} else if (dataset_size == "large") {
  n_samples <- 500
  n_taxa <- 5000
} else {
  n_samples <- 200
  n_taxa <- 2000
}

# Generate synthetic data
set.seed(42)
otu_mat <- matrix(
  rpois(n_samples * n_taxa, lambda = rexp(n_samples * n_taxa, rate = 0.1)),
  nrow = n_taxa,
  ncol = n_samples
)
rownames(otu_mat) <- paste0("OTU", 1:n_taxa)
colnames(otu_mat) <- paste0("Sample", 1:n_samples)

# Add sparsity
sparsity_level <- 0.8
zero_indices <- sample(length(otu_mat), size = round(length(otu_mat) * sparsity_level))
otu_mat[zero_indices] <- 0

# Create sample data
sample_df <- data.frame(
  SampleID = colnames(otu_mat),
  Group = rep(c("Control", "Treatment"), length.out = n_samples),
  Timepoint = rep(1:5, length.out = n_samples),
  row.names = colnames(otu_mat)
)

# Create phyloseq object
test_physeq <- phyloseq(
  otu_table(otu_mat, taxa_are_rows = TRUE),
  sample_data(sample_df)
)

cat("Dataset created: ", n_samples, " samples, ", n_taxa, " taxa\n", sep = "")
cat("Sparsity: ", round(sum(otu_mat == 0) / length(otu_mat) * 100), "%\n\n", sep = "")

# Run benchmarks
cat("Running benchmarks...\n")
cat("--------------------\n\n")

# Helper function for timing
time_operation <- function(expr, name) {
  cat(name, "... ", sep = "")
  start_time <- Sys.time()
  result <- tryCatch(
    expr,
    error = function(e) {
      cat("ERROR: ", e$message, "\n")
      return(NULL)
    }
  )
  end_time <- Sys.time()
  elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  if (!is.null(result)) {
    cat(sprintf("%.3f seconds\n", elapsed))
  }
  
  return(list(result = result, time = elapsed))
}

# Benchmark 1: Basic diversity calculation
cat("1. Basic Diversity Calculation\n")
bench1 <- time_operation(
  calculate_diversity(test_physeq, metrics = c("shannon", "simpson", "observed")),
  "   Standard calculation"
)

# Load optimizations if available
opt_file <- system.file("R", "performance_optimization.R", package = "diversityGPT")
if (file.exists(opt_file)) {
  source(opt_file)
  
  bench1_opt <- time_operation(
    optimized_calculate_diversity(
      test_physeq,
      metrics = c("shannon", "simpson", "observed"),
      use_sparse = TRUE,
      batch_size = 50
    ),
    "   Optimized calculation"
  )
  
  if (!is.null(bench1$result) && !is.null(bench1_opt$result)) {
    speedup <- bench1$time / bench1_opt$time
    cat(sprintf("   Speedup: %.2fx\n", speedup))
  }
}

cat("\n")

# Benchmark 2: Universal information extraction
cat("2. Universal Information Extraction\n")
bench2 <- time_operation(
  extract_universal_information(test_physeq),
  "   Standard extraction"
)

if (exists("parallel_extract_universal") && n_samples >= 100) {
  bench2_par <- time_operation(
    parallel_extract_universal(test_physeq, n_cores = 2, chunk_size = 50),
    "   Parallel extraction (2 cores)"
  )
  
  if (!is.null(bench2$result) && !is.null(bench2_par$result)) {
    speedup <- bench2$time / bench2_par$time
    cat(sprintf("   Speedup: %.2fx\n", speedup))
  }
}

cat("\n")

# Benchmark 3: Caching effectiveness
cat("3. Caching Performance\n")

# First run (cache miss)
cache_key <- paste0("benchmark_", dataset_size)
bench3_miss <- time_operation(
  cached_extract_universal_information(test_physeq, cache_key = cache_key),
  "   First run (cache miss)"
)

# Second run (cache hit)
bench3_hit <- time_operation(
  cached_extract_universal_information(test_physeq, cache_key = cache_key),
  "   Second run (cache hit)"
)

if (!is.null(bench3_miss$time) && !is.null(bench3_hit$time) && bench3_hit$time > 0) {
  speedup <- bench3_miss$time / bench3_hit$time
  cat(sprintf("   Cache speedup: %.0fx\n", speedup))
}

# Clean up cache
cache_remove(cache_key)

cat("\n")

# Benchmark 4: Metric transformation
if (!is.null(bench2$result)) {
  cat("4. Metric Transformation\n")
  
  # Create source metrics
  source_metrics <- data.frame(
    shannon = runif(100, 1.5, 3),
    observed = round(runif(100, 50, 200))
  )
  
  bench4 <- time_operation({
    tm <- bench2$result$transformation_matrix
    universal_diversity_transform(
      source_metrics,
      target_metrics = c("simpson", "chao1"),
      transformation_matrix = tm
    )
  },
  "   Transform 100 samples"
  )
  
  # Large scale transformation
  large_metrics <- data.frame(
    shannon = runif(10000, 1.5, 3),
    observed = round(runif(10000, 50, 200))
  )
  
  bench4_large <- time_operation({
    tm <- bench2$result$transformation_matrix
    universal_diversity_transform(
      large_metrics,
      target_metrics = c("simpson", "chao1"),
      transformation_matrix = tm
    )
  },
  "   Transform 10,000 samples"
  )
  
  if (!is.null(bench4$time) && !is.null(bench4_large$time)) {
    rate <- 10000 / bench4_large$time
    cat(sprintf("   Transformation rate: %.0f samples/second\n", rate))
  }
}

cat("\n")

# Summary
cat("=====================================\n")
cat("Benchmark Summary\n")
cat("=====================================\n")

total_time <- sum(c(
  bench1$time,
  bench1_opt$time %||% 0,
  bench2$time,
  bench2_par$time %||% 0,
  bench3_miss$time,
  bench3_hit$time,
  bench4$time %||% 0,
  bench4_large$time %||% 0
), na.rm = TRUE)

cat("Total benchmark time:", sprintf("%.1f seconds\n", total_time))
cat("\nPerformance grade: ")

# Simple performance grading based on times
if (n_samples == 200) {  # Medium dataset
  if (bench1$time < 0.5 && bench2$time < 2) {
    cat("EXCELLENT ⭐⭐⭐⭐⭐\n")
  } else if (bench1$time < 1 && bench2$time < 5) {
    cat("GOOD ⭐⭐⭐⭐\n")
  } else if (bench1$time < 2 && bench2$time < 10) {
    cat("ADEQUATE ⭐⭐⭐\n")
  } else {
    cat("NEEDS OPTIMIZATION ⭐⭐\n")
  }
}

cat("\nRecommendations:\n")

# Provide recommendations based on results
if (exists("bench1_opt") && !is.null(bench1_opt$time)) {
  if (bench1$time / bench1_opt$time < 1.2) {
    cat("- Consider using optimized functions for large datasets\n")
  }
}

if (n_samples > 100 && (!exists("bench2_par") || is.null(bench2_par$result))) {
  cat("- Enable parallel processing for large datasets\n")
}

if (!is.null(bench3_hit$time) && bench3_miss$time / bench3_hit$time > 10) {
  cat("- Caching is highly effective for your workflow\n")
}

sparsity <- sum(otu_mat == 0) / length(otu_mat)
if (sparsity > 0.7) {
  cat("- Your data is very sparse (", round(sparsity * 100), "%) - sparse matrix optimizations recommended\n", sep = "")
}

cat("\nFor more optimization tips, see vignette('performance-guide', package = 'diversityGPT')\n")

# Define %||% operator if not available
`%||%` <- function(x, y) if (is.null(x)) y else x