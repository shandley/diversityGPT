# Update documentation for new Shiny functions
devtools::load_all()
devtools::document()

# Check that the new functions are exported
cat("Checking exports...\n")
exports <- getNamespaceExports("diversityGPT")
shiny_exports <- exports[grep("launch|explore", exports)]
cat("Shiny-related exports:", paste(shiny_exports, collapse = ", "), "\n")