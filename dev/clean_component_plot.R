# Clean Information Components Visualization

devtools::load_all()

# Create a clean stacked bar chart for information components
create_clean_component_plot <- function(universal_info, n_samples = 10) {
  
  # Get component data
  components <- universal_info$information_components
  
  # Select first n samples
  if (nrow(components) > n_samples) {
    components <- components[1:n_samples, ]
  }
  
  # Extract proportions
  R_prop <- components$R_proportion
  E_prop <- components$E_proportion
  P_prop <- components$P_proportion
  S_prop <- components$S_proportion
  
  # Create matrix for barplot
  comp_matrix <- rbind(R_prop, E_prop, P_prop, S_prop)
  colnames(comp_matrix) <- paste0("S", 1:ncol(comp_matrix))
  
  # Set up colors
  colors <- c(
    "coral",      # R - Richness
    "skyblue",    # E - Evenness  
    "lightgreen", # P - Phylogenetic
    "gold"        # S - Spatial
  )
  
  # Create clean plot
  par(mar = c(5, 4, 4, 8), xpd = TRUE)  # Extra margin for legend
  
  barplot(
    comp_matrix,
    col = colors,
    border = "white",
    space = 0.1,
    ylim = c(0, 1),
    main = "Information Component Proportions",
    xlab = "Samples",
    ylab = "Proportion",
    las = 1
  )
  
  # Add legend outside plot
  legend(
    "topright",
    inset = c(-0.2, 0),
    legend = c("Richness (R)", "Evenness (E)", "Phylogenetic (P)", "Spatial (S)"),
    fill = colors,
    border = "white",
    bty = "n"
  )
  
  # Add grid for readability
  abline(h = seq(0, 1, 0.2), col = "gray80", lty = 2)
}

# If universal_info exists, create the plot
if (exists("universal_info")) {
  create_clean_component_plot(universal_info)
} else {
  # Create example data
  cat("Creating example component plot...\n")
  
  # Example proportions for 5 samples
  example_data <- data.frame(
    R_proportion = c(0.5, 0.5, 0.45, 0.35, 0.5),
    E_proportion = c(0.5, 0.45, 0.5, 0.65, 0.5),
    P_proportion = c(0, 0.05, 0.05, 0, 0),
    S_proportion = c(0, 0, 0, 0, 0)
  )
  
  # Create matrix
  comp_matrix <- t(as.matrix(example_data))
  colnames(comp_matrix) <- paste0("S", 1:5)
  
  # Plot
  par(mar = c(5, 4, 4, 8), xpd = TRUE)
  
  barplot(
    comp_matrix,
    col = c("coral", "skyblue", "lightgreen", "gold"),
    border = "white",
    space = 0.1,
    ylim = c(0, 1),
    main = "Information Component Proportions",
    xlab = "Samples", 
    ylab = "Proportion",
    las = 1,
    axes = TRUE
  )
  
  # Add legend
  legend(
    "topright",
    inset = c(-0.25, 0),
    legend = c("Richness (R)", "Evenness (E)", "Phylogenetic (P)", "Spatial (S)"),
    fill = c("coral", "skyblue", "lightgreen", "gold"),
    border = "white",
    bty = "n",
    cex = 0.9
  )
  
  # Add value labels on bars
  # For each sample
  for (i in 1:5) {
    # Calculate cumulative heights for label positioning
    cumsum_heights <- cumsum(comp_matrix[, i])
    prev_heights <- c(0, cumsum_heights[-length(cumsum_heights)])
    mid_points <- prev_heights + comp_matrix[, i] / 2
    
    # Add labels for components > 0.1
    for (j in 1:4) {
      if (comp_matrix[j, i] > 0.1) {
        text(
          x = i - 1 + 0.2 * i,  # x position
          y = mid_points[j],    # y position
          labels = round(comp_matrix[j, i], 2),
          cex = 0.8
        )
      }
    }
  }
}

# Alternative: Create a cleaner ggplot2 version
if (require("ggplot2") && require("tidyr")) {
  cat("\nCreating ggplot2 version...\n")
  
  # Prepare data
  if (exists("universal_info")) {
    plot_data <- universal_info$information_components[1:5, ]
  } else {
    plot_data <- data.frame(
      sample_id = paste0("Sample", 1:5),
      R_proportion = c(0.5, 0.5, 0.45, 0.35, 0.5),
      E_proportion = c(0.5, 0.45, 0.5, 0.65, 0.5),
      P_proportion = c(0, 0.05, 0.05, 0, 0),
      S_proportion = c(0, 0, 0, 0, 0)
    )
  }
  
  # Reshape for ggplot
  plot_data_long <- tidyr::pivot_longer(
    plot_data,
    cols = ends_with("_proportion"),
    names_to = "Component",
    values_to = "Proportion"
  )
  
  # Clean component names
  plot_data_long$Component <- gsub("_proportion", "", plot_data_long$Component)
  plot_data_long$Component <- factor(
    plot_data_long$Component,
    levels = c("S", "P", "E", "R"),  # Order for stacking
    labels = c("Spatial", "Phylogenetic", "Evenness", "Richness")
  )
  
  # Create plot
  p <- ggplot2::ggplot(plot_data_long, ggplot2::aes(x = sample_id, y = Proportion, fill = Component)) +
    ggplot2::geom_bar(stat = "identity", width = 0.8) +
    ggplot2::scale_fill_manual(
      values = c(Richness = "coral", Evenness = "skyblue", 
                 Phylogenetic = "lightgreen", Spatial = "gold")
    ) +
    ggplot2::labs(
      title = "Information Component Proportions",
      x = "Samples",
      y = "Proportion",
      fill = "Component"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "right",
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5)
    ) +
    ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, 1))
  
  print(p)
}

cat("\nThe clean plots show how each sample's diversity\n")
cat("is decomposed into fundamental components:\n")
cat("• Richness (R): Species count information\n")
cat("• Evenness (E): Distribution uniformity\n") 
cat("• Phylogenetic (P): Evolutionary diversity\n")
cat("• Spatial (S): Geographic patterns\n")