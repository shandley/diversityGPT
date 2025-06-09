# Test Shiny App Launch
# This script tests the launch_diversity_explorer() function

devtools::load_all()

cat("Testing diversityGPT Shiny Interactive Explorer\n")
cat("==============================================\n\n")

# Check if the function exists
if (exists("launch_diversity_explorer")) {
  cat("✓ launch_diversity_explorer() function found\n\n")
  
  # Check for required packages
  required_packages <- c("shiny", "shinydashboard", "plotly", "DT")
  missing <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  
  if (length(missing) > 0) {
    cat("✗ Missing required packages:", paste(missing, collapse = ", "), "\n")
    cat("Install with: install.packages(c('", paste(missing, collapse = "', '"), "'))\n")
  } else {
    cat("✓ All required packages installed\n\n")
    
    # Get app directory
    app_dir <- system.file("shiny", "diversity_explorer", package = "diversityGPT")
    
    if (app_dir == "") {
      cat("✗ Shiny app directory not found in package\n")
      cat("App should be in: inst/shiny/diversity_explorer/\n")
    } else {
      cat("✓ Shiny app directory found:", app_dir, "\n\n")
      
      # Check for app files
      app_files <- c("app.R", "ui.R", "server.R")
      for (file in app_files) {
        if (file.exists(file.path(app_dir, file))) {
          cat("✓", file, "exists\n")
        } else {
          cat("✗", file, "missing\n")
        }
      }
      
      cat("\nTo launch the app, run:\n")
      cat("launch_diversity_explorer()\n\n")
      
      cat("Or to launch with example data:\n")
      cat("explore_example('GlobalPatterns')\n")
    }
  }
} else {
  cat("✗ launch_diversity_explorer() function not found\n")
  cat("Make sure R/shiny_app.R is properly loaded\n")
}

# Also test if we can access the app directory directly
cat("\n\nDirect app directory test:\n")
if (dir.exists("inst/shiny/diversity_explorer")) {
  cat("✓ inst/shiny/diversity_explorer exists\n")
  cat("Files:", paste(list.files("inst/shiny/diversity_explorer"), collapse = ", "), "\n")
} else {
  cat("✗ inst/shiny/diversity_explorer not found\n")
}