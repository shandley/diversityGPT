#!/usr/bin/env Rscript
#' Build pkgdown documentation website for diversityGPT
#'
#' This script builds the complete documentation website using pkgdown

# Load required packages
if (!requireNamespace("pkgdown", quietly = TRUE)) {
  message("Installing pkgdown...")
  install.packages("pkgdown")
}

library(pkgdown)

cat("=====================================\n")
cat("Building diversityGPT Documentation\n")
cat("=====================================\n\n")

# Check if we're in the package directory
if (!file.exists("DESCRIPTION")) {
  stop("Must run from package root directory")
}

# Clean previous build
if (dir.exists("docs")) {
  cat("Cleaning previous build...\n")
  unlink("docs", recursive = TRUE)
}

# Build site
cat("Building pkgdown site...\n")
cat("This may take a few minutes...\n\n")

tryCatch({
  # Build the site
  pkgdown::build_site(
    pkg = ".",
    preview = FALSE,
    new_process = FALSE,
    devel = FALSE
  )
  
  cat("\n✅ Documentation built successfully!\n")
  cat("📁 Output location: docs/\n")
  cat("🌐 To view locally: browseURL('docs/index.html')\n")
  
  # Optionally open in browser
  if (interactive()) {
    cat("\nOpen in browser? (y/n): ")
    response <- readline()
    if (tolower(response) == "y") {
      browseURL("docs/index.html")
    }
  }
  
}, error = function(e) {
  cat("\n❌ Error building documentation:\n")
  cat(e$message, "\n")
  
  # Common troubleshooting tips
  cat("\nTroubleshooting tips:\n")
  cat("1. Ensure all vignettes build: devtools::build_vignettes()\n")
  cat("2. Check for missing dependencies: devtools::check()\n")
  cat("3. Verify _pkgdown.yml syntax\n")
  cat("4. Try building individual components:\n")
  cat("   - pkgdown::build_home()\n")
  cat("   - pkgdown::build_reference()\n")
  cat("   - pkgdown::build_articles()\n")
})

# GitHub Pages setup reminder
cat("\n=====================================\n")
cat("GitHub Pages Deployment\n")
cat("=====================================\n")
cat("To deploy to GitHub Pages:\n")
cat("1. Commit the 'docs' folder to your repository\n")
cat("2. Go to Settings > Pages in your GitHub repo\n")
cat("3. Set Source to 'Deploy from a branch'\n")
cat("4. Select 'main' branch and '/docs' folder\n")
cat("5. Save and wait a few minutes for deployment\n")
cat("\nYour site will be available at:\n")
cat("https://shandley.github.io/diversityGPT/\n")