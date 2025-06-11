# Test Assembly Mechanism Visualizations

library(devtools)
load_all()
library(ggplot2)
library(plotly)

cat("=== Testing Enhanced Assembly Mechanism Visualizations ===\n\n")

# Create test data
set.seed(123)
demo_data <- create_demo_phyloseq(n_samples = 20, n_taxa = 40)

# Add environmental data
env_data <- data.frame(
  pH = rnorm(20, mean = 7, sd = 1),
  temperature = rnorm(20, mean = 25, sd = 5),
  nutrients = runif(20, min = 1, max = 10)
)
rownames(env_data) <- sample_names(demo_data)
sample_data(demo_data) <- sample_data(env_data)

# Extract universal information
cat("Step 1: Extracting universal information...\n")
universal_info <- extract_universal_information(demo_data)
cat("✓ Universal information extracted\n\n")

# Detect assembly mechanisms
cat("Step 2: Detecting assembly mechanisms...\n")
mechanisms <- detect_assembly_mechanisms(
  universal_info,
  environmental_data = env_data,
  method = "comprehensive"
)
cat("✓ Assembly mechanisms detected\n\n")

# Test different plot types
cat("Step 3: Testing visualization types...\n\n")

# 1. Summary plot
cat("Creating summary plot...\n")
p1 <- plot(mechanisms, type = "summary")
print(p1)
cat("✓ Summary plot created\n\n")

# 2. Confidence plot
cat("Creating confidence radar plot...\n")
p2 <- plot(mechanisms, type = "confidence")
print(p2)
cat("✓ Confidence plot created\n\n")

# 3. Network plot
cat("Creating mechanism network plot...\n")
p3 <- plot(mechanisms, type = "network")
print(p3)
cat("✓ Network plot created\n\n")

# 4. Evidence plot
cat("Creating evidence heatmap...\n")
p4 <- plot(mechanisms, type = "evidence")
print(p4)
cat("✓ Evidence plot created\n\n")

# 5. Interactive plots
cat("Creating interactive versions...\n")
p1_int <- plot(mechanisms, type = "summary", interactive = TRUE)
p2_int <- plot(mechanisms, type = "confidence", interactive = TRUE)
cat("✓ Interactive plots created\n\n")

# 6. Test dashboard
cat("Creating assembly mechanism dashboard...\n")
dashboard_file <- tempfile(fileext = ".html")
assembly_mechanism_dashboard(mechanisms, save_path = dashboard_file)
cat("✓ Dashboard saved to:", dashboard_file, "\n\n")

# 7. Test with custom colors
cat("Testing custom color palette...\n")
custom_colors <- c(
  "Environmental Filtering" = "#FF6B6B",
  "Competitive Exclusion" = "#4ECDC4",
  "Neutral Drift" = "#45B7D1",
  "Dispersal Limitation" = "#F9CA24",
  "Phylogenetic Clustering" = "#6C5CE7"
)

p_custom <- plot(mechanisms, type = "summary", colors = custom_colors)
print(p_custom)
cat("✓ Custom colors applied\n\n")

# 8. Test all plots at once
cat("Creating all plots...\n")
all_plots <- plot(mechanisms, type = "all")
cat("✓ Created", length(all_plots), "plots\n\n")

cat("=== All visualization tests completed successfully! ===\n")
cat("\nKey features demonstrated:\n")
cat("• Summary bar plot with confidence scores\n")
cat("• Radar/spider plot for mechanism confidence\n")
cat("• Network visualization of mechanism relationships\n")
cat("• Evidence heatmap (when available)\n")
cat("• Interactive plotly versions\n")
cat("• HTML dashboard generation\n")
cat("• Custom color support\n\n")

cat("The enhanced visualizations provide intuitive ways to:\n")
cat("1. Compare mechanism confidence scores\n")
cat("2. Identify dominant assembly processes\n")
cat("3. Understand mechanism interactions\n")
cat("4. Present results in publications\n")
cat("5. Create interactive reports for stakeholders\n")