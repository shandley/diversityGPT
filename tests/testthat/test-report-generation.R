# test-report-generation.R
# Tests for report generation functions

test_that("generate_diversity_report creates output file", {
  # Create test data
  test_physeq <- create_demo_phyloseq(n_samples = 10, n_taxa = 20)
  div_results <- calculate_diversity(test_physeq)
  universal_info <- extract_universal_information(test_physeq)
  
  # Create temporary output file
  temp_file <- tempfile(fileext = ".html")
  
  # Generate report
  result <- generate_diversity_report(
    diversity_results = div_results,
    universal_info = universal_info,
    output_file = temp_file,
    title = "Test Report"
  )
  
  # Check file was created
  expect_true(file.exists(temp_file))
  expect_equal(result, temp_file)
  
  # Check file has content
  expect_true(file.size(temp_file) > 0)
  
  # Clean up
  unlink(temp_file)
})

test_that("generate_diversity_report handles different formats", {
  test_physeq <- create_demo_phyloseq(n_samples = 5, n_taxa = 10)
  div_results <- calculate_diversity(test_physeq)
  
  # Test HTML format
  temp_html <- tempfile(fileext = ".html")
  result_html <- generate_diversity_report(
    diversity_results = div_results,
    output_file = temp_html,
    format = "html"
  )
  expect_true(file.exists(temp_html))
  unlink(temp_html)
  
  # Test PDF format (if pandoc available)
  if (rmarkdown::pandoc_available()) {
    temp_pdf <- tempfile(fileext = ".pdf")
    result_pdf <- generate_diversity_report(
      diversity_results = div_results,
      output_file = temp_pdf,
      format = "pdf"
    )
    expect_true(file.exists(temp_pdf))
    unlink(temp_pdf)
  }
})

test_that("generate_diversity_report includes all sections", {
  test_physeq <- create_demo_phyloseq(n_samples = 8, n_taxa = 15)
  div_results <- calculate_diversity(test_physeq)
  universal_info <- extract_universal_information(test_physeq)
  
  temp_file <- tempfile(fileext = ".html")
  
  # Generate report with all components
  generate_diversity_report(
    diversity_results = div_results,
    universal_info = universal_info,
    consensus_results = consensus_diversity(test_physeq),
    output_file = temp_file,
    include_sections = c("overview", "diversity", "universal", "consensus")
  )
  
  # Read content
  content <- readLines(temp_file)
  
  # Check for expected sections
  expect_true(any(grepl("Overview", content, ignore.case = TRUE)))
  expect_true(any(grepl("Diversity", content, ignore.case = TRUE)))
  expect_true(any(grepl("Universal", content, ignore.case = TRUE)))
  expect_true(any(grepl("Consensus", content, ignore.case = TRUE)))
  
  unlink(temp_file)
})

test_that("generate_quick_summary creates summary", {
  test_physeq <- create_demo_phyloseq(n_samples = 10, n_taxa = 20)
  div_results <- calculate_diversity(test_physeq)
  
  # Generate quick summary
  summary <- generate_quick_summary(
    diversity_results = div_results,
    format = "text"
  )
  
  # Check output
  expect_type(summary, "character")
  expect_true(length(summary) > 0)
  expect_true(any(grepl("samples|taxa|diversity", summary, ignore.case = TRUE)))
})

test_that("generate_quick_summary handles different formats", {
  test_physeq <- create_demo_phyloseq(n_samples = 5, n_taxa = 10)
  div_results <- calculate_diversity(test_physeq)
  universal_info <- extract_universal_information(test_physeq)
  
  # Test text format
  summary_text <- generate_quick_summary(
    diversity_results = div_results,
    format = "text"
  )
  expect_type(summary_text, "character")
  
  # Test markdown format
  summary_md <- generate_quick_summary(
    diversity_results = div_results,
    universal_info = universal_info,
    format = "markdown"
  )
  expect_type(summary_md, "character")
  expect_true(any(grepl("#|\\*", summary_md)))  # Markdown formatting
  
  # Test HTML format
  summary_html <- generate_quick_summary(
    diversity_results = div_results,
    format = "html"
  )
  expect_type(summary_html, "character")
  expect_true(any(grepl("<|>", summary_html)))  # HTML tags
})

test_that("generate_diversity_report with ecological intelligence", {
  test_physeq <- create_demo_phyloseq(n_samples = 10, n_taxa = 20)
  div_results <- calculate_diversity(test_physeq)
  universal_info <- extract_universal_information(test_physeq)
  
  # Add ecological intelligence components
  mechanisms <- detect_assembly_mechanisms(universal_info)
  hypotheses <- generate_ecological_hypotheses(universal_info)
  
  temp_file <- tempfile(fileext = ".html")
  
  # Generate comprehensive report
  generate_diversity_report(
    diversity_results = div_results,
    universal_info = universal_info,
    assembly_mechanisms = mechanisms,
    hypotheses = hypotheses,
    output_file = temp_file,
    include_sections = c("overview", "diversity", "universal", 
                        "mechanisms", "hypotheses")
  )
  
  # Check file exists and has content
  expect_true(file.exists(temp_file))
  expect_true(file.size(temp_file) > 1000)  # Should be substantial
  
  unlink(temp_file)
})

test_that("generate_diversity_report handles missing components gracefully", {
  test_physeq <- create_demo_phyloseq(n_samples = 5, n_taxa = 10)
  div_results <- calculate_diversity(test_physeq)
  
  temp_file <- tempfile(fileext = ".html")
  
  # Generate report with only diversity results
  result <- generate_diversity_report(
    diversity_results = div_results,
    output_file = temp_file
  )
  
  expect_true(file.exists(temp_file))
  unlink(temp_file)
})

test_that("generate_diversity_report custom CSS works", {
  test_physeq <- create_demo_phyloseq(n_samples = 5, n_taxa = 10)
  div_results <- calculate_diversity(test_physeq)
  
  temp_file <- tempfile(fileext = ".html")
  
  # Custom CSS
  custom_css <- "
  body { font-family: Arial, sans-serif; }
  h1 { color: #2c3e50; }
  "
  
  generate_diversity_report(
    diversity_results = div_results,
    output_file = temp_file,
    custom_css = custom_css
  )
  
  # Read content and check for CSS
  content <- paste(readLines(temp_file), collapse = "\n")
  expect_true(grepl("font-family: Arial", content))
  expect_true(grepl("#2c3e50", content))
  
  unlink(temp_file)
})

test_that("generate_diversity_report interactive plots option", {
  test_physeq <- create_demo_phyloseq(n_samples = 5, n_taxa = 10)
  div_results <- calculate_diversity(test_physeq)
  universal_info <- extract_universal_information(test_physeq)
  
  temp_file <- tempfile(fileext = ".html")
  
  # Generate with interactive plots
  generate_diversity_report(
    diversity_results = div_results,
    universal_info = universal_info,
    output_file = temp_file,
    interactive_plots = TRUE
  )
  
  # Check for plotly in output
  content <- paste(readLines(temp_file), collapse = "\n")
  expect_true(grepl("plotly|htmlwidgets", content, ignore.case = TRUE))
  
  unlink(temp_file)
})

test_that("generate_quick_summary includes key metrics", {
  test_physeq <- create_demo_phyloseq(n_samples = 10, n_taxa = 25)
  div_results <- calculate_diversity(test_physeq)
  universal_info <- extract_universal_information(test_physeq)
  
  summary <- generate_quick_summary(
    diversity_results = div_results,
    universal_info = universal_info,
    format = "text"
  )
  
  # Should include key information
  expect_true(grepl("10.*samples", summary))
  expect_true(grepl("25.*taxa", summary))
  expect_true(any(grepl("Shannon|Simpson|diversity", summary, ignore.case = TRUE)))
  expect_true(any(grepl("component|information", summary, ignore.case = TRUE)))
})

test_that("generate_diversity_report handles errors gracefully", {
  # Test with invalid input
  expect_error(generate_diversity_report("not_diversity_results"))
  
  # Test with invalid output path
  expect_error(
    generate_diversity_report(
      diversity_results = list(),
      output_file = "/invalid/path/report.html"
    )
  )
})