# Clean Network and Information Space Plots

devtools::load_all()

# Function to create clean network plot
create_clean_network_plot <- function(universal_info, min_r_squared = 0.5) {
  
  # Increase margins and plot size
  par(mar = c(2, 2, 4, 2))
  
  # Create the network
  plot_diversity_network(
    universal_info = universal_info,
    interactive = FALSE,
    min_r_squared = min_r_squared,
    node_size_by = "importance"
  )
  
  # Add title with more space
  title("Diversity Metric Relationship Network", line = 2.5, cex.main = 1.3)
  
  # Add subtitle
  mtext(paste("Edges show R² >", min_r_squared), side = 3, line = 0.5, cex = 0.9)
}

# Function to create clean information space plot
create_clean_info_space_plot <- function(universal_info, community_labels) {
  
  # Set up plot with good margins
  par(mar = c(5, 5, 4, 2))
  
  # Get components
  R_comp <- universal_info$information_components$R_component
  E_comp <- universal_info$information_components$E_component
  
  # Create color palette
  colors <- c("black", "red", "forestgreen")
  point_colors <- colors[as.numeric(factor(community_labels))]
  
  # Create the plot with better spacing
  plot(
    R_comp, E_comp,
    col = point_colors,
    pch = 19,
    cex = 1.5,
    xlab = "Richness Component (R)",
    ylab = "Evenness Component (E)", 
    main = "Information Space",
    xlim = c(-0.1, 1.1),
    ylim = c(-0.1, 1.1),
    las = 1,
    cex.lab = 1.2,
    cex.main = 1.3
  )
  
  # Add grid
  grid(col = "gray90")
  
  # Add better legend
  legend(
    "top",
    legend = unique(community_labels),
    col = colors[1:length(unique(community_labels))],
    pch = 19,
    cex = 1,
    bg = "white",
    box.lwd = 1
  )
  
  # Add axis labels for clarity
  mtext("Low Richness", side = 1, at = 0, line = 3, cex = 0.8)
  mtext("High Richness", side = 1, at = 1, line = 3, cex = 0.8)
  mtext("Low Evenness", side = 2, at = 0, line = 3, cex = 0.8, las = 0)
  mtext("High Evenness", side = 2, at = 1, line = 3, cex = 0.8, las = 0)
}

# Create figure with both plots
if (exists("universal_info") && exists("ps")) {
  
  # Set up 2-panel figure
  par(mfrow = c(1, 2))
  
  # Plot 1: Network
  create_clean_network_plot(universal_info)
  
  # Plot 2: Information Space
  community_labels <- sample_data(ps)$Community
  create_clean_info_space_plot(universal_info, community_labels)
  
  par(mfrow = c(1, 1))
  
} else {
  cat("Running network topology demo first to generate data...\n\n")
  source("dev/network_topology_demo.R")
}

# Alternative: Create separate windows for each plot
cat("\nTo view plots in separate windows:\n")
cat("dev.new(); create_clean_network_plot(universal_info)\n")
cat("dev.new(); create_clean_info_space_plot(universal_info, community_labels)\n")

# Save high-quality versions
cat("\nSaving high-quality versions...\n")

# Network plot
png("network_plot_clean.png", width = 800, height = 600, res = 120)
par(mar = c(2, 2, 4, 2))
plot_diversity_network(universal_info, interactive = FALSE, min_r_squared = 0.5)
title("Diversity Metric Relationship Network", line = 2.5, cex.main = 1.5)
mtext("Node color = dominant component, Edge width = R²", side = 3, line = 0.5)
dev.off()

# Information space plot  
png("information_space_clean.png", width = 800, height = 600, res = 120)
par(mar = c(5, 5, 4, 2))
plot(
  universal_info$information_components$R_component,
  universal_info$information_components$E_component,
  col = as.numeric(factor(sample_data(ps)$Community)),
  pch = 19,
  cex = 2,
  xlab = "Richness Component (R)",
  ylab = "Evenness Component (E)",
  main = "Information Space - Community Separation",
  xlim = c(-0.1, 1.1),
  ylim = c(-0.1, 1.1),
  las = 1,
  cex.lab = 1.3,
  cex.main = 1.5
)
grid(col = "gray90")
legend(
  "top",
  legend = levels(factor(sample_data(ps)$Community)),
  col = 1:3,
  pch = 19,
  cex = 1.2,
  bg = "white"
)
dev.off()

cat("Clean plots saved as PNG files!\n")