test_that("Shiny app launches can be called", {
  # These tests just verify the functions exist and can be called
  # Actual UI testing would require shinytest2
  
  # Check functions exist
  expect_true(exists("launch_diversity_explorer"))
  expect_true(exists("launch_simple_explorer"))
  expect_true(exists("launch_component_explorer"))
  expect_true(exists("launch_enhanced_explorer"))
  
  # Check they are functions
  expect_type(launch_diversity_explorer, "closure")
  expect_type(launch_simple_explorer, "closure")
  expect_type(launch_component_explorer, "closure")
  expect_type(launch_enhanced_explorer, "closure")
})

test_that("explore_example function works", {
  # Function should exist
  expect_true(exists("explore_example"))
  
  # Should accept dataset parameter
  expect_error(
    explore_example("invalid_dataset"),
    "should be one of"
  )
})

test_that("Shiny app files exist", {
  # Check main app files
  app_dir <- system.file("shiny", "diversity_explorer", package = "diversityGPT")
  expect_true(dir.exists(app_dir))
  
  # Check for key files
  expect_true(file.exists(file.path(app_dir, "app.R")))
  expect_true(file.exists(file.path(app_dir, "ui.R")))
  expect_true(file.exists(file.path(app_dir, "server.R")))
  expect_true(file.exists(file.path(app_dir, "simple_app.R")))
  
  # Check modules directory
  modules_dir <- file.path(app_dir, "modules")
  expect_true(dir.exists(modules_dir))
  
  # Check for module files
  expect_true(file.exists(file.path(modules_dir, "dataset_browser.R")))
  expect_true(file.exists(file.path(modules_dir, "enhanced_component_explorer_module.R")))
})

test_that("Shiny module functions are available", {
  # Source module files
  app_dir <- system.file("shiny", "diversity_explorer", package = "diversityGPT")
  
  # Test dataset browser module
  source(file.path(app_dir, "modules", "dataset_browser.R"))
  expect_true(exists("dataset_browser_ui"))
  expect_true(exists("dataset_browser_server"))
  expect_type(dataset_browser_ui, "closure")
  expect_type(dataset_browser_server, "closure")
  
  # Test enhanced component explorer
  source(file.path(app_dir, "modules", "enhanced_component_explorer_module.R"))
  expect_true(exists("enhanced_component_explorer_ui"))
  expect_true(exists("enhanced_component_explorer_server"))
})

test_that("Shiny app helper functions work", {
  # Test create_dataset_browser_tab
  app_dir <- system.file("shiny", "diversity_explorer", package = "diversityGPT")
  source(file.path(app_dir, "modules", "dataset_browser.R"))
  
  tab <- create_dataset_browser_tab()
  expect_s3_class(tab, "shiny.tag")
  expect_equal(tab$name, "div")
})

# More comprehensive tests would use shinytest2
test_that("Note: Full Shiny testing requires shinytest2", {
  skip("Full Shiny app testing requires shinytest2 package and interactive session")
  
  # Example of what shinytest2 tests would look like:
  # library(shinytest2)
  # 
  # test_that("Diversity explorer loads and displays data", {
  #   app <- AppDriver$new(
  #     system.file("shiny/diversity_explorer", package = "diversityGPT"),
  #     height = 800, width = 1200
  #   )
  #   
  #   # Wait for app to load
  #   app$wait_for_idle()
  #   
  #   # Click on dataset browser
  #   app$click("dataset_browser")
  #   
  #   # Select a dataset
  #   app$click("load_globalpatterns")
  #   
  #   # Verify data loaded
  #   output <- app$get_value(output = "data_summary")
  #   expect_match(output, "phyloseq")
  #   
  #   app$stop()
  # })
})