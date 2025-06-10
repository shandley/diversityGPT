test_that("detect_data_format identifies formats correctly", {
  # Create temporary test files
  temp_dir <- tempdir()
  
  # BIOM file (has specific header)
  biom_file <- file.path(temp_dir, "test.biom")
  writeLines('{"id": "test", "format": "Biological Observation Matrix"}', biom_file)
  expect_equal(detect_data_format(biom_file), "biom")
  
  # QIIME2 artifact (.qza extension)
  qza_file <- file.path(temp_dir, "table.qza")
  file.create(qza_file)
  expect_equal(detect_data_format(qza_file), "qiime2")
  
  # MetaPhlAn profile (has characteristic header)
  metaphlan_file <- file.path(temp_dir, "profile.txt")
  writeLines(c("#SampleID\tMetaphlan_Analysis", 
               "k__Bacteria\t50.0"), metaphlan_file)
  expect_equal(detect_data_format(metaphlan_file), "metaphlan")
  
  # TSV file
  tsv_file <- file.path(temp_dir, "data.tsv")
  writeLines("col1\tcol2\tcol3\n1\t2\t3", tsv_file)
  expect_equal(detect_data_format(tsv_file), "tsv")
  
  # CSV file
  csv_file <- file.path(temp_dir, "data.csv")
  writeLines("col1,col2,col3\n1,2,3", csv_file)
  expect_equal(detect_data_format(csv_file), "csv")
  
  # Phyloseq RDS
  phyloseq_file <- file.path(temp_dir, "physeq.rds")
  data(GlobalPatterns, package = "phyloseq")
  saveRDS(GlobalPatterns, phyloseq_file)
  expect_equal(detect_data_format(phyloseq_file), "phyloseq")
  
  # Clean up
  unlink(c(biom_file, qza_file, metaphlan_file, tsv_file, csv_file, phyloseq_file))
})

test_that("convert_to_phyloseq handles auto-detection", {
  # Create test phyloseq RDS
  temp_file <- tempfile(fileext = ".rds")
  data(GlobalPatterns, package = "phyloseq")
  gp_small <- phyloseq::prune_taxa(phyloseq::taxa_names(GlobalPatterns)[1:50], 
                                   GlobalPatterns)
  saveRDS(gp_small, temp_file)
  
  # Test auto-detection
  result <- convert_to_phyloseq(temp_file)
  expect_s3_class(result, "phyloseq")
  expect_equal(phyloseq::ntaxa(result), 50)
  
  # Test with explicit format
  result2 <- convert_to_phyloseq(temp_file, format = "phyloseq")
  expect_identical(result, result2)
  
  # Clean up
  unlink(temp_file)
})

test_that("biom_to_phyloseq handles errors gracefully", {
  # Test with non-existent file
  expect_error(
    biom_to_phyloseq("nonexistent.biom"),
    "not found"
  )
  
  # Test with invalid biom content
  if (requireNamespace("biomformat", quietly = TRUE)) {
    temp_file <- tempfile(fileext = ".biom")
    writeLines("not a valid biom file", temp_file)
    expect_error(biom_to_phyloseq(temp_file))
    unlink(temp_file)
  }
})

test_that("qiime2_to_phyloseq handles .qza files", {
  # Skip if no example qza file available
  skip("QIIME2 file conversion requires actual .qza files")
  
  # Would test:
  # - Reading feature table from .qza
  # - Filtering by min_reads and min_prevalence
  # - Extracting metadata from artifact
})

test_that("metaphlan_to_phyloseq parses profiles correctly", {
  # Create test MetaPhlAn profile
  temp_file <- tempfile(fileext = ".txt")
  profile_content <- c(
    "#SampleID\tSample1",
    "k__Bacteria|p__Firmicutes|c__Bacilli|o__Lactobacillales|f__Lactobacillaceae|g__Lactobacillus|s__Lactobacillus_casei\t25.5",
    "k__Bacteria|p__Actinobacteria|c__Actinobacteria|o__Bifidobacteriales|f__Bifidobacteriaceae|g__Bifidobacterium|s__Bifidobacterium_longum\t15.3",
    "k__Bacteria|p__Proteobacteria|c__Gammaproteobacteria|o__Enterobacterales|f__Enterobacteriaceae|g__Escherichia|s__Escherichia_coli\t10.2"
  )
  writeLines(profile_content, temp_file)
  
  # Convert
  result <- metaphlan_to_phyloseq(temp_file)
  
  expect_s3_class(result, "phyloseq")
  expect_equal(phyloseq::nsamples(result), 1)
  expect_equal(phyloseq::ntaxa(result), 3)
  
  # Check taxonomy parsing
  tax <- as.data.frame(phyloseq::tax_table(result))
  expect_equal(ncol(tax), 7)  # Kingdom through Species
  expect_true("Species" %in% colnames(tax))
  
  # Check abundances
  otu <- as.vector(phyloseq::otu_table(result))
  expect_equal(sum(otu), 51.0, tolerance = 0.1)
  
  unlink(temp_file)
})

test_that("metaphlan_to_phyloseq handles multiple samples", {
  # Create test files
  temp_dir <- tempdir()
  
  # Sample 1
  file1 <- file.path(temp_dir, "sample1_profile.txt")
  writeLines(c(
    "#SampleID\tSample1",
    "k__Bacteria|p__Firmicutes|c__Bacilli|o__Lactobacillales|f__Lactobacillaceae|g__Lactobacillus|s__Lactobacillus_casei\t30.0",
    "k__Bacteria|p__Actinobacteria|c__Actinobacteria|o__Bifidobacteriales|f__Bifidobacteriaceae|g__Bifidobacterium|s__Bifidobacterium_longum\t20.0"
  ), file1)
  
  # Sample 2
  file2 <- file.path(temp_dir, "sample2_profile.txt")
  writeLines(c(
    "#SampleID\tSample2",
    "k__Bacteria|p__Firmicutes|c__Bacilli|o__Lactobacillales|f__Lactobacillaceae|g__Lactobacillus|s__Lactobacillus_casei\t15.0",
    "k__Bacteria|p__Proteobacteria|c__Gammaproteobacteria|o__Enterobacterales|f__Enterobacteriaceae|g__Escherichia|s__Escherichia_coli\t25.0"
  ), file2)
  
  # Convert multiple files
  result <- metaphlan_to_phyloseq(c(file1, file2))
  
  expect_s3_class(result, "phyloseq")
  expect_equal(phyloseq::nsamples(result), 2)
  expect_equal(phyloseq::ntaxa(result), 3)  # 3 unique species
  
  # Check sample names
  expect_equal(phyloseq::sample_names(result), c("sample1", "sample2"))
  
  unlink(c(file1, file2))
})

test_that("merge_qiime2_artifacts combines components", {
  # Skip - requires actual QIIME2 files
  skip("QIIME2 merge requires actual .qza files")
  
  # Would test:
  # - Merging feature table, taxonomy, and tree
  # - Adding metadata
  # - Handling missing components
})

test_that("format converters handle edge cases", {
  # Empty file
  empty_file <- tempfile()
  file.create(empty_file)
  expect_error(convert_to_phyloseq(empty_file))
  unlink(empty_file)
  
  # Invalid format specification
  expect_error(
    convert_to_phyloseq("any_file.txt", format = "invalid_format"),
    "Unknown format"
  )
})

test_that("TSV/CSV conversion works", {
  # Create test TSV
  temp_tsv <- tempfile(fileext = ".tsv")
  otu_data <- data.frame(
    OTU1 = c(10, 20, 30),
    OTU2 = c(5, 15, 25),
    OTU3 = c(0, 10, 20),
    row.names = c("Sample1", "Sample2", "Sample3")
  )
  write.table(otu_data, temp_tsv, sep = "\t", quote = FALSE)
  
  # This would need the actual implementation
  # result <- convert_to_phyloseq(temp_tsv, format = "tsv", taxa_are_rows = FALSE)
  # expect_s3_class(result, "phyloseq")
  
  unlink(temp_tsv)
})