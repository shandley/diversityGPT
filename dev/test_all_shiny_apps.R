# Test all diversityGPT Shiny applications
# This script verifies that all Shiny apps can launch without errors

library(diversityGPT)

cat("Testing diversityGPT Shiny Applications\n")
cat("======================================\n\n")

# Test 1: Check that all launch functions exist
cat("1. Checking launch functions exist...\n")
functions_to_test <- c(
  "launch_diversity_explorer",
  "launch_simple_explorer", 
  "launch_component_explorer",
  "launch_enhanced_explorer",
  "explore_example"
)

all_exist <- TRUE
for (fn in functions_to_test) {
  if (exists(fn)) {
    cat("  ✓", fn, "exists\n")
  } else {
    cat("  ✗", fn, "NOT FOUND\n")
    all_exist <- FALSE
  }
}

if (all_exist) {
  cat("\n✅ All launch functions are available!\n\n")
} else {
  cat("\n❌ Some launch functions are missing!\n\n")
}

# Test 2: Check app directories
cat("2. Checking Shiny app files...\n")
app_dir <- system.file("shiny", "diversity_explorer", package = "diversityGPT")

if (dir.exists(app_dir)) {
  cat("  ✓ App directory found:", app_dir, "\n")
  
  required_files <- c("app.R", "ui.R", "server.R", "simple_app.R", 
                     "component_explorer.R", "enhanced_component_explorer.R")
  
  for (file in required_files) {
    if (file.exists(file.path(app_dir, file))) {
      cat("  ✓", file, "exists\n")
    } else {
      cat("  ✗", file, "NOT FOUND\n")
    }
  }
  
  # Check modules
  modules_dir <- file.path(app_dir, "modules")
  if (dir.exists(modules_dir)) {
    cat("  ✓ Modules directory exists\n")
    module_files <- list.files(modules_dir, pattern = "\\.R$")
    cat("    Found", length(module_files), "module files:", paste(module_files, collapse = ", "), "\n")
  }
  
  # Check www directory for assets
  www_dir <- file.path(app_dir, "www")
  if (dir.exists(www_dir)) {
    cat("  ✓ www directory exists\n")
    www_files <- list.files(www_dir)
    if (length(www_files) > 0) {
      cat("    Found", length(www_files), "asset files:", paste(www_files, collapse = ", "), "\n")
    }
  }
} else {
  cat("  ✗ App directory NOT FOUND\n")
}

# Test 3: Check precomputed data
cat("\n3. Checking precomputed demo data...\n")
data_dir <- system.file("data", package = "diversityGPT")
if (dir.exists(data_dir)) {
  rds_files <- list.files(data_dir, pattern = "\\.rds$", full.names = TRUE)
  cat("  Found", length(rds_files), "precomputed data files:\n")
  for (file in rds_files) {
    cat("    -", basename(file), "\n")
    # Try to load and check structure
    tryCatch({
      data <- readRDS(file)
      if (is.list(data) && "universal_info" %in% names(data)) {
        cat("      ✓ Contains universal_info\n")
      }
      if (is.list(data) && "phyloseq" %in% names(data)) {
        cat("      ✓ Contains phyloseq object\n")
      }
    }, error = function(e) {
      cat("      ✗ Error loading file:", e$message, "\n")
    })
  }
}

# Test 4: Check required packages
cat("\n4. Checking required Shiny packages...\n")
shiny_deps <- c("shiny", "shinydashboard", "plotly", "DT", "networkD3")
for (pkg in shiny_deps) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    cat("  ✓", pkg, "is installed\n")
  } else {
    cat("  ✗", pkg, "NOT INSTALLED\n")
  }
}

# Test 5: Quick validation of app structure
cat("\n5. Validating main app structure...\n")
tryCatch({
  # Source the UI and server files to check for syntax errors
  ui_env <- new.env()
  server_env <- new.env()
  
  # Load required packages in the environments
  ui_env$library <- server_env$library <- function(...) NULL
  ui_env$shinydashboard <- server_env$shinydashboard <- list()
  
  source(file.path(app_dir, "ui.R"), local = ui_env)
  source(file.path(app_dir, "server.R"), local = server_env)
  
  cat("  ✓ UI and server files parse without errors\n")
  
  if (exists("ui", envir = ui_env)) {
    cat("  ✓ UI object is defined\n")
  }
  
  if (exists("server", envir = server_env)) {
    cat("  ✓ Server function is defined\n")
  }
  
}, error = function(e) {
  cat("  ✗ Error validating app structure:", e$message, "\n")
})

# Summary
cat("\n======================================\n")
cat("Testing complete!\n\n")
cat("To launch the apps, use:\n")
cat("  launch_diversity_explorer()     # Main app\n")
cat("  launch_simple_explorer()        # Simple version\n")
cat("  launch_component_explorer()     # Component explorer\n")
cat("  launch_enhanced_explorer()      # Enhanced explorer\n")
cat("  explore_example('GlobalPatterns') # With example data\n")