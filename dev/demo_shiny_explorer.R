# Demo: diversityGPT Interactive Explorer
# This script demonstrates how to use the Shiny app

cat("diversityGPT Shiny Interactive Explorer Demo\n")
cat("===========================================\n\n")

# Load the package
devtools::load_all()

cat("The diversityGPT Interactive Explorer provides a user-friendly interface for:\n")
cat("• Uploading and exploring microbiome data\n")
cat("• Extracting universal information components\n")
cat("• Transforming between ANY diversity metrics\n")
cat("• Creating interactive visualizations\n")
cat("• Getting AI-powered interpretations\n")
cat("• Exporting comprehensive results\n\n")

cat("Launch Options:\n")
cat("===============\n\n")

cat("1. Basic launch (opens in browser):\n")
cat("   launch_diversity_explorer()\n\n")

cat("2. Launch with pre-loaded example:\n")
cat("   explore_example('GlobalPatterns')\n\n")

cat("3. Launch on specific port:\n")
cat("   launch_diversity_explorer(port = 3838)\n\n")

cat("4. Launch without opening browser:\n")
cat("   launch_diversity_explorer(launch.browser = FALSE)\n\n")

cat("App Features:\n")
cat("=============\n\n")

cat("📊 Data Upload Tab:\n")
cat("   - Load example datasets (GlobalPatterns, enterotype, soil)\n")
cat("   - Upload custom phyloseq objects (.RDS files)\n")
cat("   - View metadata and taxonomic overview\n\n")

cat("🔬 Universal Analysis Tab:\n")
cat("   - Extract R, E, P, S components from all metrics\n")
cat("   - View transformation quality (R² scores)\n")
cat("   - Explore component distributions\n\n")

cat("🔄 Transform Metrics Tab:\n")
cat("   - Input any known metric values\n")
cat("   - Predict ANY other metrics\n")
cat("   - See confidence intervals\n\n")

cat("📈 Visualizations Tab:\n")
cat("   - Interactive metric relationship networks\n")
cat("   - Information space scatter plots\n")
cat("   - Component dashboards\n\n")

cat("🧠 AI Interpretation Tab:\n")
cat("   - Get ecological insights\n")
cat("   - Generate testable hypotheses\n")
cat("   - Literature connections\n\n")

cat("💾 Export Tab:\n")
cat("   - Download results in multiple formats\n")
cat("   - Generate HTML/PDF reports\n")
cat("   - Save R objects for reproducibility\n\n")

# Check if required packages are installed
required_packages <- c("shiny", "shinydashboard", "plotly", "DT")
missing <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

if (length(missing) > 0) {
  cat("⚠️  Missing required packages:", paste(missing, collapse = ", "), "\n")
  cat("Install with:\n")
  cat("install.packages(c('", paste(missing, collapse = "', '"), "'))\n\n")
} else {
  cat("✅ All required packages installed!\n\n")
  cat("Ready to launch! Try:\n")
  cat("launch_diversity_explorer()\n")
}