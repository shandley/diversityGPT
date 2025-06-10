test_that("identify_taxa_drivers works with basic inputs", {
  # Create test data
  demo_data <- create_demo_phyloseq(n_samples = 10, n_taxa = 50)
  
  # Run taxa driver analysis
  drivers <- identify_taxa_drivers(demo_data, top_n = 5, verbose = FALSE)
  
  # Check structure
  expect_s3_class(drivers, "taxa_drivers")
  expect_true(all(c("richness_drivers", "evenness_drivers", 
                   "phylogenetic_drivers", "spatial_drivers") %in% names(drivers)))
  
  # Check each component has correct structure
  for (comp in c("richness_drivers", "evenness_drivers", 
                 "phylogenetic_drivers", "spatial_drivers")) {
    expect_true(is.data.frame(drivers[[comp]]))
    expect_true(all(c("taxon", "contribution", "rank", "component") %in% 
                   names(drivers[[comp]])))
    expect_equal(nrow(drivers[[comp]]), 5)  # top_n = 5
  }
  
  # Check summary
  expect_true("summary" %in% names(drivers))
  expect_true("overall" %in% names(drivers$summary))
})

test_that("identify_taxa_drivers works with different methods", {
  demo_data <- create_demo_phyloseq(n_samples = 10, n_taxa = 30)
  
  # Test each method
  methods <- c("contribution", "variance", "correlation")
  
  for (method in methods) {
    drivers <- identify_taxa_drivers(demo_data, 
                                    method = method,
                                    top_n = 5,
                                    verbose = FALSE)
    
    expect_s3_class(drivers, "taxa_drivers")
    expect_equal(drivers$method, method)
    
    # Check all components have results
    for (comp in c("richness_drivers", "evenness_drivers",
                   "phylogenetic_drivers", "spatial_drivers")) {
      expect_true(nrow(drivers[[comp]]) > 0)
      expect_true(all(drivers[[comp]]$contribution >= 0))
    }
  }
})

test_that("identify_richness_drivers calculates correct metrics", {
  # Create simple test data
  otu_mat <- matrix(c(
    10, 10, 10, 10,  # Taxon 1: present everywhere, even abundance
    20, 0, 0, 0,     # Taxon 2: present in one sample only
    5, 5, 0, 0,      # Taxon 3: present in half samples
    1, 2, 3, 4       # Taxon 4: present everywhere, variable
  ), nrow = 4, byrow = TRUE)
  
  rownames(otu_mat) <- paste0("OTU", 1:4)
  colnames(otu_mat) <- paste0("Sample", 1:4)
  
  demo_physeq <- phyloseq::phyloseq(
    phyloseq::otu_table(otu_mat, taxa_are_rows = TRUE)
  )
  
  components <- extract_universal_information(demo_physeq)
  
  # Test contribution method
  richness_drivers <- identify_richness_drivers(
    otu_mat, components, top_n = 4, 
    method = "contribution", groups = NULL, normalize = FALSE
  )
  
  expect_equal(nrow(richness_drivers), 4)
  expect_true(all(richness_drivers$presence_frequency >= 0))
  expect_true(all(richness_drivers$presence_frequency <= 1))
  
  # Taxon 1 should have high presence frequency
  taxon1_idx <- which(richness_drivers$taxon == "OTU1")
  expect_equal(richness_drivers$presence_frequency[taxon1_idx], 1.0)
})

test_that("identify_evenness_drivers detects dominant taxa", {
  # Create data with one dominant taxon
  otu_mat <- matrix(c(
    100, 100, 100, 100,  # Taxon 1: dominant
    1, 1, 1, 1,          # Taxon 2: rare
    10, 10, 10, 10,      # Taxon 3: intermediate
    5, 5, 5, 5           # Taxon 4: intermediate
  ), nrow = 4, byrow = TRUE)
  
  rownames(otu_mat) <- paste0("OTU", 1:4)
  colnames(otu_mat) <- paste0("Sample", 1:4)
  
  demo_physeq <- phyloseq::phyloseq(
    phyloseq::otu_table(otu_mat, taxa_are_rows = TRUE)
  )
  
  components <- extract_universal_information(demo_physeq)
  
  evenness_drivers <- identify_evenness_drivers(
    otu_mat, components, top_n = 4,
    method = "contribution", groups = NULL, normalize = FALSE
  )
  
  # OTU1 should be identified as top evenness driver (reduces evenness)
  expect_equal(evenness_drivers$taxon[1], "OTU1")
  expect_true(evenness_drivers$mean_relative_abundance[1] > 0.5)
})

test_that("identify_phylogenetic_drivers handles missing tree", {
  demo_data <- create_demo_phyloseq(n_samples = 5, n_taxa = 20)
  components <- extract_universal_information(demo_data)
  
  # Should work without tree using taxonomic information
  phylo_drivers <- identify_phylogenetic_drivers(
    demo_data, components, top_n = 5,
    method = "contribution", groups = NULL, normalize = TRUE
  )
  
  expect_true(is.data.frame(phylo_drivers))
  expect_equal(nrow(phylo_drivers), 5)
  expect_true(all(phylo_drivers$contribution >= 0))
})

test_that("identify_spatial_drivers detects patchy distributions", {
  # Create data with patchy distribution
  otu_mat <- matrix(c(
    100, 100, 0, 0,    # Taxon 1: patchy (high in half samples)
    10, 10, 10, 10,    # Taxon 2: even distribution
    50, 0, 50, 0,      # Taxon 3: very patchy
    5, 6, 4, 5         # Taxon 4: relatively even
  ), nrow = 4, byrow = TRUE)
  
  rownames(otu_mat) <- paste0("OTU", 1:4)
  colnames(otu_mat) <- paste0("Sample", 1:4)
  
  demo_physeq <- phyloseq::phyloseq(
    phyloseq::otu_table(otu_mat, taxa_are_rows = TRUE)
  )
  
  components <- extract_universal_information(demo_physeq)
  
  spatial_drivers <- identify_spatial_drivers(
    demo_physeq, components, top_n = 4,
    method = "contribution", groups = NULL, normalize = FALSE
  )
  
  # Check dispersion index
  expect_true(all(spatial_drivers$dispersion_index >= 0))
  
  # Taxa with patchy distributions should have high dispersion
  patchy_taxa <- spatial_drivers$taxon %in% c("OTU1", "OTU3")
  expect_true(any(spatial_drivers$dispersion_index[patchy_taxa] > 1))
})

test_that("taxa driver normalization works correctly", {
  demo_data <- create_demo_phyloseq(n_samples = 5, n_taxa = 20)
  
  # With normalization
  drivers_norm <- identify_taxa_drivers(demo_data, 
                                       top_n = 10,
                                       normalize = TRUE,
                                       verbose = FALSE)
  
  # Without normalization  
  drivers_no_norm <- identify_taxa_drivers(demo_data,
                                          top_n = 10, 
                                          normalize = FALSE,
                                          verbose = FALSE)
  
  # Check normalized contributions sum to 1
  for (comp in c("richness_drivers", "evenness_drivers",
                 "phylogenetic_drivers", "spatial_drivers")) {
    if (normalize) {
      total_contrib <- sum(drivers_norm[[comp]]$contribution)
      expect_true(abs(total_contrib - 1) < 0.01 || total_contrib == 0)
    }
  }
  
  # Rankings should be the same
  expect_equal(drivers_norm$richness_drivers$taxon,
               drivers_no_norm$richness_drivers$taxon)
})

test_that("summarize_taxa_drivers provides correct statistics", {
  demo_data <- create_demo_phyloseq(n_samples = 10, n_taxa = 50)
  drivers <- identify_taxa_drivers(demo_data, top_n = 10, verbose = FALSE)
  
  summary_stats <- drivers$summary
  
  # Check structure
  expect_true(all(c("richness", "evenness", "phylogenetic", "spatial", "overall") %in% 
                 names(summary_stats)))
  
  # Check component summaries
  for (comp in c("richness", "evenness", "phylogenetic", "spatial")) {
    comp_summary <- summary_stats[[comp]]
    expect_true(all(c("n_drivers", "total_contribution", "top_taxon",
                     "top_contribution", "mean_contribution", "sd_contribution") %in%
                   names(comp_summary)))
    expect_equal(comp_summary$n_drivers, 10)  # top_n = 10
  }
  
  # Check overall summary
  expect_true("total_unique_drivers" %in% names(summary_stats$overall))
  expect_true("n_multi_component" %in% names(summary_stats$overall))
  expect_equal(summary_stats$overall$method, "contribution")
})

test_that("print.taxa_drivers works correctly", {
  demo_data <- create_demo_phyloseq(n_samples = 5, n_taxa = 20)
  drivers <- identify_taxa_drivers(demo_data, top_n = 5, verbose = FALSE)
  
  # Capture output
  output <- capture.output(print(drivers))
  
  # Check output contains expected sections
  expect_true(any(grepl("Taxa Driver Analysis Results", output)))
  expect_true(any(grepl("RICHNESS DRIVERS:", output)))
  expect_true(any(grepl("EVENNESS DRIVERS:", output)))
  expect_true(any(grepl("SUMMARY:", output)))
})

test_that("plot.taxa_drivers creates valid plots", {
  demo_data <- create_demo_phyloseq(n_samples = 10, n_taxa = 30)
  drivers <- identify_taxa_drivers(demo_data, top_n = 5, verbose = FALSE)
  
  # Test bar plot
  p_bar <- plot(drivers, type = "bar", top_n = 5)
  expect_s3_class(p_bar, "ggplot")
  
  # Test heatmap
  p_heat <- plot(drivers, type = "heatmap", top_n = 5)
  expect_s3_class(p_heat, "ggplot")
  
  # Test contribution plot
  p_contrib <- plot(drivers, type = "contribution", top_n = 5)
  expect_s3_class(p_contrib, "ggplot")
  
  # Test network plot (static version)
  p_network <- plot(drivers, type = "network", top_n = 5, interactive = FALSE)
  expect_s3_class(p_network, c("ggraph", "ggplot"))
})

test_that("taxa drivers work with grouped analysis", {
  # Create data with groups
  demo_data <- create_demo_phyloseq(n_samples = 10, n_taxa = 30)
  phyloseq::sample_data(demo_data)$Group <- rep(c("A", "B"), each = 5)
  
  drivers <- identify_taxa_drivers(demo_data,
                                  groups = "Group",
                                  top_n = 5,
                                  method = "variance",
                                  verbose = FALSE)
  
  expect_s3_class(drivers, "taxa_drivers")
  
  # Spatial drivers should use group information
  expect_true(nrow(drivers$spatial_drivers) > 0)
})