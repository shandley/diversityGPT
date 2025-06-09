test_that("plot_diversity_network works with basic input", {
  skip_if_not_installed("igraph")
  
  # Create minimal universal_info object
  universal_info <- list(
    transformation_matrix = data.frame(
      metric = c("shannon", "simpson", "observed"),
      r_squared = c(0.8, 0.7, 0.6),
      adj_r_squared = c(0.75, 0.65, 0.55),
      rmse = c(0.1, 0.15, 5.0),
      n_obs = c(10, 10, 10),
      R_component = c(0.5, 0.3, 0.8),
      E_component = c(0.3, 0.5, 0.1),
      P_component = c(0.1, 0.1, 0.05),
      S_component = c(0.1, 0.1, 0.05),
      stringsAsFactors = FALSE
    ),
    information_components = data.frame(
      sample_id = paste0("Sample_", 1:5),
      R_component = runif(5),
      E_component = runif(5),
      P_component = runif(5),
      S_component = runif(5),
      stringsAsFactors = FALSE
    )
  )
  
  class(universal_info) <- c("universal_information", "list")
  
  # Test static network plot (interactive = FALSE)
  expect_silent({
    plot <- plot_diversity_network(
      universal_info = universal_info,
      interactive = FALSE,
      min_r_squared = 0.5
    )
  })
  
  # Check that it returns an igraph object
  expect_s3_class(plot, "igraph")
})

test_that("plot_information_components creates dashboard", {
  skip_if_not_installed("ggplot2")
  
  # Create test data
  components <- data.frame(
    sample_id = paste0("Sample_", 1:5),
    R_component = c(0.3, 0.4, 0.5, 0.2, 0.6),
    E_component = c(0.2, 0.3, 0.2, 0.4, 0.1),
    P_component = c(0.3, 0.2, 0.2, 0.3, 0.2),
    S_component = c(0.2, 0.1, 0.1, 0.1, 0.1),
    total_information = rep(1, 5),
    R_proportion = c(0.3, 0.4, 0.5, 0.2, 0.6),
    E_proportion = c(0.2, 0.3, 0.2, 0.4, 0.1),
    P_proportion = c(0.3, 0.2, 0.2, 0.3, 0.2),
    S_proportion = c(0.2, 0.1, 0.1, 0.1, 0.1),
    stringsAsFactors = FALSE
  )
  
  trans_matrix <- data.frame(
    metric = c("shannon", "simpson"),
    r_squared = c(0.8, 0.7),
    stringsAsFactors = FALSE
  )
  
  quality <- list(
    overall_quality = "Good",
    mean_r_squared = 0.75,
    component_importance = data.frame(
      component = c("R_component", "E_component", "P_component", "S_component"),
      mean_coefficient = c(0.5, 0.3, 0.15, 0.05),
      contribution_frequency = c(0.9, 0.8, 0.6, 0.3),
      stringsAsFactors = FALSE
    )
  )
  
  universal_info <- list(
    information_components = components,
    transformation_matrix = trans_matrix,
    deconvolution_quality = quality
  )
  
  class(universal_info) <- c("universal_information", "list")
  
  # Test dashboard creation
  expect_silent({
    dashboard <- plot_information_components(
      universal_info,
      plot_type = "grid"
    )
  })
  
  # Should create multiple plots
  expect_true(is.list(dashboard) || inherits(dashboard, "patchwork"))
})

test_that("plot.universal_information method works", {
  skip_if_not_installed("ggplot2")
  
  # Create minimal universal_info
  universal_info <- list(
    transformation_matrix = data.frame(
      metric = c("shannon", "simpson"),
      r_squared = c(0.8, 0.7),
      stringsAsFactors = FALSE
    ),
    information_components = data.frame(
      sample_id = "Sample_1",
      R_component = 0.4,
      E_component = 0.3,
      P_component = 0.2,
      S_component = 0.1,
      stringsAsFactors = FALSE
    ),
    deconvolution_quality = list(
      overall_quality = "Good",
      mean_r_squared = 0.75
    )
  )
  
  class(universal_info) <- c("universal_information", "list")
  
  # Test quality plot
  expect_silent({
    plot(universal_info, type = "quality")
  })
  
  # Test components plot  
  expect_silent({
    plot(universal_info, type = "components")
  })
})

test_that("plot_transformation_quality works", {
  skip_if_not_installed("ggplot2")
  
  # Create test data
  trans_matrix <- data.frame(
    metric = c("shannon", "simpson", "observed", "chao1"),
    r_squared = c(0.9, 0.8, 0.6, 0.4),
    stringsAsFactors = FALSE
  )
  
  quality <- list(
    overall_quality = "Good",
    mean_r_squared = 0.675,
    high_quality_metrics = 2,
    medium_quality_metrics = 1,
    low_quality_metrics = 1
  )
  
  universal_info <- list(
    transformation_matrix = trans_matrix,
    deconvolution_quality = quality
  )
  
  class(universal_info) <- c("universal_information", "list")
  
  # Test matrix plot
  expect_silent({
    plot <- plot_transformation_quality(
      universal_info,
      plot_type = "matrix"
    )
  })
  
  expect_s3_class(plot, "ggplot")
})

test_that("plot_transformation_results works", {
  skip_if_not_installed("ggplot2")
  
  # Create test transformation object
  predicted_metrics <- data.frame(
    sample_id = "Sample_1",
    shannon = 2.3,
    simpson = 0.8,
    observed = 45,
    stringsAsFactors = FALSE
  )
  
  quality_details <- data.frame(
    metric = c("shannon", "simpson", "observed"),
    r_squared = c(0.9, 0.8, 0.6),
    reliable = c(TRUE, TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  
  transformation <- list(
    predicted_metrics = predicted_metrics,
    transformation_quality = list(
      overall_quality = "Good",
      quality_details = quality_details,
      reliable_predictions = 2,
      total_predictions = 3
    )
  )
  
  class(transformation) <- c("universal_transformation", "list")
  
  # Test without actual values
  expect_silent({
    plot1 <- plot_transformation_results(transformation)
  })
  
  expect_s3_class(plot1, "ggplot")
  
  # Test with actual values
  actual <- c(shannon = 2.5, simpson = 0.75, observed = 50)
  
  expect_silent({
    plot2 <- plot_transformation_results(transformation, actual_values = actual)
  })
  
  expect_s3_class(plot2, "ggplot")
})

test_that("color schemes work correctly", {
  skip_if_not_installed("ggplot2")
  
  # Create minimal test data
  components <- data.frame(
    sample_id = paste0("Sample_", 1:3),
    R_component = c(0.4, 0.3, 0.5),
    E_component = c(0.3, 0.4, 0.2),
    P_component = c(0.2, 0.2, 0.2),
    S_component = c(0.1, 0.1, 0.1),
    R_proportion = c(0.4, 0.3, 0.5),
    E_proportion = c(0.3, 0.4, 0.2),
    P_proportion = c(0.2, 0.2, 0.2),
    S_proportion = c(0.1, 0.1, 0.1),
    stringsAsFactors = FALSE
  )
  
  universal_info <- list(
    information_components = components,
    transformation_matrix = data.frame(
      metric = "shannon",
      r_squared = 0.8,
      stringsAsFactors = FALSE
    ),
    deconvolution_quality = list(overall_quality = "Good")
  )
  
  class(universal_info) <- c("universal_information", "list")
  
  # Test different color schemes
  for (scheme in c("default", "viridis")) {
    expect_silent({
      plot_information_components(
        universal_info,
        color_scheme = scheme,
        plot_type = "grid"
      )
    })
  }
})

test_that("edge cases are handled gracefully", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("igraph")
  
  # Empty transformation matrix
  universal_info <- list(
    transformation_matrix = data.frame(
      metric = character(0),
      r_squared = numeric(0),
      stringsAsFactors = FALSE
    ),
    information_components = data.frame(
      sample_id = "Sample_1",
      R_component = 0.25,
      E_component = 0.25,
      P_component = 0.25,
      S_component = 0.25,
      stringsAsFactors = FALSE
    )
  )
  
  class(universal_info) <- c("universal_information", "list")
  
  # Should handle empty data gracefully
  expect_error({
    plot_diversity_network(universal_info, interactive = FALSE)
  })
  
  # Very high R² threshold (no edges)
  universal_info$transformation_matrix <- data.frame(
    metric = c("shannon", "simpson"),
    r_squared = c(0.3, 0.2),
    R_component = c(0.5, 0.5),
    E_component = c(0.5, 0.5),
    P_component = c(0, 0),
    S_component = c(0, 0),
    stringsAsFactors = FALSE
  )
  
  expect_silent({
    plot <- plot_diversity_network(
      universal_info,
      min_r_squared = 0.9,  # Higher than any R² value
      interactive = FALSE
    )
  })
})