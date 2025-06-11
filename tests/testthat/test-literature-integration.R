# test-literature-integration.R
# Tests for literature search and integration functions

test_that("search_literature works with basic parameters", {
  # Create test data
  test_physeq <- create_demo_phyloseq(n_samples = 10, n_taxa = 20)
  universal_info <- extract_universal_information(test_physeq)
  
  # Test basic literature search
  result <- search_literature(
    universal_info,
    study_context = list(
      environment = "gut_microbiome",
      keywords = c("diversity", "assembly")
    ),
    search_databases = "pubmed",
    max_papers = 5,
    relevance_threshold = 0.5
  )
  
  # Check structure
  expect_s3_class(result, "literature_search")
  expect_type(result, "list")
  expect_true(all(c("papers", "search_summary", "search_terms", 
                   "databases_searched") %in% names(result)))
  
  # Check papers
  expect_type(result$papers, "list")
  if (length(result$papers) > 0) {
    paper <- result$papers[[1]]
    expect_true(all(c("title", "authors", "year", "journal", 
                     "relevance_score") %in% names(paper)))
    expect_true(paper$relevance_score >= 0 && paper$relevance_score <= 1)
  }
  
  # Check search summary
  expect_type(result$search_summary, "list")
  expect_true("total_papers" %in% names(result$search_summary))
  expect_true("databases" %in% names(result$search_summary))
})

test_that("search_literature handles multiple databases", {
  test_physeq <- create_demo_phyloseq(n_samples = 5, n_taxa = 10)
  universal_info <- extract_universal_information(test_physeq)
  
  result <- search_literature(
    universal_info,
    search_databases = c("pubmed", "crossref"),
    max_papers = 3
  )
  
  expect_s3_class(result, "literature_search")
  expect_equal(result$databases_searched, c("pubmed", "crossref"))
})

test_that("search_literature generates appropriate search terms", {
  test_physeq <- create_demo_phyloseq(n_samples = 5, n_taxa = 10)
  universal_info <- extract_universal_information(test_physeq)
  
  result <- search_literature(
    universal_info,
    study_context = list(
      environment = "soil_microbiome",
      organism = "bacteria",
      keywords = c("pH", "temperature", "nutrients")
    ),
    max_papers = 2
  )
  
  # Check search terms were generated
  expect_type(result$search_terms, "list")
  expect_true("primary" %in% names(result$search_terms))
  expect_true("secondary" %in% names(result$search_terms))
  
  # Should include context keywords
  all_terms <- c(result$search_terms$primary, result$search_terms$secondary)
  expect_true(any(grepl("soil", all_terms, ignore.case = TRUE)))
  expect_true(any(grepl("bacteria", all_terms, ignore.case = TRUE)))
})

test_that("search_literature with assembly mechanisms", {
  test_physeq <- create_demo_phyloseq(n_samples = 8, n_taxa = 15)
  universal_info <- extract_universal_information(test_physeq)
  
  # Create mock assembly mechanisms
  mock_mechanisms <- list(
    mechanisms = list(
      environmental_filtering = list(
        detected = TRUE,
        confidence = 0.8,
        evidence = "Strong pH correlation"
      )
    ),
    interpretation = "Environmental filtering dominant"
  )
  class(mock_mechanisms) <- c("assembly_mechanisms", "list")
  
  result <- search_literature(
    universal_info,
    assembly_mechanisms = mock_mechanisms,
    max_papers = 3
  )
  
  expect_s3_class(result, "literature_search")
  # Search terms should reflect assembly mechanisms
  all_terms <- unlist(result$search_terms)
  expect_true(any(grepl("environmental.*filtering|filtering", all_terms, 
                       ignore.case = TRUE)))
})

test_that("search_literature with hypotheses integration", {
  test_physeq <- create_demo_phyloseq(n_samples = 5, n_taxa = 10)
  universal_info <- extract_universal_information(test_physeq)
  
  # Create mock hypotheses
  mock_hypotheses <- list(
    hypotheses = list(
      list(
        hypothesis = "pH drives species sorting",
        type = "mechanistic",
        keywords = c("pH", "species sorting", "environmental gradient")
      )
    )
  )
  class(mock_hypotheses) <- c("ecological_hypotheses", "list")
  
  result <- search_literature(
    universal_info,
    hypotheses = mock_hypotheses,
    max_papers = 2
  )
  
  expect_s3_class(result, "literature_search")
  # Should incorporate hypothesis keywords
  all_terms <- unlist(result$search_terms)
  expect_true(any(grepl("pH|species sorting", all_terms, ignore.case = TRUE)))
})

test_that("print.literature_search works", {
  test_physeq <- create_demo_phyloseq(n_samples = 5, n_taxa = 10)
  universal_info <- extract_universal_information(test_physeq)
  
  result <- search_literature(
    universal_info,
    max_papers = 2
  )
  
  # Capture print output
  output <- capture.output(print(result))
  
  expect_true(any(grepl("Literature Search Results", output)))
  expect_true(any(grepl("Search Summary:", output)))
  expect_true(any(grepl("Papers found:", output)))
})

test_that("plot.literature_search creates visualization", {
  test_physeq <- create_demo_phyloseq(n_samples = 5, n_taxa = 10)
  universal_info <- extract_universal_information(test_physeq)
  
  result <- search_literature(
    universal_info,
    max_papers = 5
  )
  
  # Test network plot
  p <- plot(result, type = "network")
  expect_true(!is.null(p))  # Should return something (even if mock)
  
  # Test relevance plot
  p2 <- plot(result, type = "relevance")
  expect_true(!is.null(p2))
})

test_that("search_literature handles empty results", {
  test_physeq <- create_demo_phyloseq(n_samples = 3, n_taxa = 5)
  universal_info <- extract_universal_information(test_physeq)
  
  # Use very high relevance threshold to get no results
  result <- search_literature(
    universal_info,
    relevance_threshold = 0.99,
    max_papers = 1
  )
  
  expect_s3_class(result, "literature_search")
  expect_type(result$papers, "list")
  # Should handle empty results gracefully
})

test_that("search_literature respects max_papers limit", {
  test_physeq <- create_demo_phyloseq(n_samples = 5, n_taxa = 10)
  universal_info <- extract_universal_information(test_physeq)
  
  result <- search_literature(
    universal_info,
    max_papers = 3,
    relevance_threshold = 0.1  # Low threshold to get more results
  )
  
  expect_true(length(result$papers) <= 3)
})

test_that("search_literature citation generation works", {
  test_physeq <- create_demo_phyloseq(n_samples = 5, n_taxa = 10)
  universal_info <- extract_universal_information(test_physeq)
  
  result <- search_literature(
    universal_info,
    max_papers = 2,
    include_citations = TRUE
  )
  
  if (length(result$papers) > 0 && !is.null(result$papers[[1]]$citations)) {
    citations <- result$papers[[1]]$citations
    expect_true("apa" %in% names(citations) || "bibtex" %in% names(citations))
  }
})

test_that("search_literature summarize abstracts works", {
  test_physeq <- create_demo_phyloseq(n_samples = 5, n_taxa = 10)
  universal_info <- extract_universal_information(test_physeq)
  
  result <- search_literature(
    universal_info,
    max_papers = 2,
    summarize_abstracts = TRUE
  )
  
  if (length(result$papers) > 0) {
    paper <- result$papers[[1]]
    expect_true("abstract" %in% names(paper) || "summary" %in% names(paper))
  }
})

test_that("search_literature identifies research gaps", {
  test_physeq <- create_demo_phyloseq(n_samples = 5, n_taxa = 10)
  universal_info <- extract_universal_information(test_physeq)
  
  result <- search_literature(
    universal_info,
    study_context = list(
      environment = "extreme_environment",
      organism = "archaea"
    ),
    max_papers = 3
  )
  
  # Should identify research gaps
  expect_true("research_gaps" %in% names(result) || 
              "gaps_identified" %in% names(result$search_summary))
})

test_that("search_literature novel connections work", {
  test_physeq <- create_demo_phyloseq(n_samples = 5, n_taxa = 10)
  universal_info <- extract_universal_information(test_physeq)
  
  result <- search_literature(
    universal_info,
    max_papers = 5
  )
  
  # Check for novel connections
  expect_true("novel_connections" %in% names(result) ||
              "connections_found" %in% names(result$search_summary))
})

test_that("search_literature handles errors gracefully", {
  # Test with invalid input
  expect_error(search_literature("not_universal_info"))
  
  # Test with invalid database
  test_physeq <- create_demo_phyloseq(n_samples = 5, n_taxa = 10)
  universal_info <- extract_universal_information(test_physeq)
  
  expect_message(
    search_literature(
      universal_info,
      search_databases = "invalid_db",
      max_papers = 1
    ),
    "Unknown database"
  )
})