#!/usr/bin/env Rscript

# CRAN submission check script for diversityGPT

library(devtools)

cat("=== diversityGPT CRAN Check Process ===\n\n")

# Step 1: Document the package
cat("Step 1: Documenting package...\n")
tryCatch({
  devtools::document()
  cat("✓ Documentation generated\n\n")
}, error = function(e) {
  cat("✗ Documentation failed:", e$message, "\n\n")
})

# Step 2: Build package
cat("Step 2: Building package...\n")
tryCatch({
  pkg_path <- devtools::build(path = ".")
  cat("✓ Package built:", pkg_path, "\n\n")
}, error = function(e) {
  cat("✗ Build failed:", e$message, "\n\n")
  pkg_path <- NULL
})

# Step 3: Run checks
if (!is.null(pkg_path)) {
  cat("Step 3: Running R CMD check...\n")
  
  # Basic check
  cat("\n--- Basic check (no examples/tests/vignettes) ---\n")
  system2("R", c("CMD", "check", pkg_path, "--no-examples", "--no-tests", "--no-vignettes"))
  
  # Full check (if requested)
  if (interactive()) {
    cat("\nRun full check? (y/n): ")
    if (tolower(readline()) == "y") {
      cat("\n--- Full check ---\n")
      check_results <- devtools::check(pkg_path)
      print(check_results)
    }
  }
}

# Step 4: Check for common CRAN issues
cat("\n\nStep 4: Checking for common CRAN issues...\n")

# Check DESCRIPTION
desc <- read.dcf("DESCRIPTION")
required_fields <- c("Package", "Version", "Title", "Description", "License", "Encoding")
missing <- setdiff(required_fields, colnames(desc))
if (length(missing) > 0) {
  cat("✗ Missing DESCRIPTION fields:", paste(missing, collapse = ", "), "\n")
} else {
  cat("✓ All required DESCRIPTION fields present\n")
}

# Check for non-standard files
cat("\nChecking for non-standard files...\n")
non_standard <- c(".git", ".DS_Store", "*.Rproj", "*.Rhistory")
for (pattern in non_standard) {
  files <- list.files(".", pattern = pattern, all.files = TRUE, recursive = TRUE)
  if (length(files) > 0) {
    cat("! Found", pattern, "files - make sure they're in .Rbuildignore\n")
  }
}

# Check examples
cat("\nChecking for \\dontrun{} in examples...\n")
r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
dontrun_count <- 0
for (file in r_files) {
  content <- readLines(file, warn = FALSE)
  if (any(grepl("\\\\dontrun\\{", content))) {
    dontrun_count <- dontrun_count + 1
  }
}
cat("Found", dontrun_count, "files with \\dontrun{} examples\n")

cat("\n=== Check Summary ===\n")
cat("1. Review any errors or warnings above\n")
cat("2. Ensure all examples can run without errors\n")
cat("3. Check that vignettes build successfully\n")
cat("4. Run spell check: spelling::spell_check_package()\n")
cat("5. Check URLs: urlchecker::url_check()\n")
cat("6. Submit to win-builder for additional checks\n")

cat("\nReady for CRAN submission? Run:\n")
cat("  devtools::release()\n")