# Server logic for diversityGPT Interactive Explorer

function(input, output, session) {
  
  # Reactive values to store data and results
  values <- reactiveValues(
    physeq = NULL,
    universal_info = NULL,
    transform_results = NULL,
    interpretation = NULL,
    source_metrics = list(),
    plots = list()
  )
  
  # Load example dataset
  observeEvent(input$load_example, {
    withProgress(message = "Loading dataset...", {
      if (input$dataset_choice == "global") {
        values$physeq <- GlobalPatterns
        showNotification("GlobalPatterns dataset loaded", type = "success")
      } else if (input$dataset_choice == "entero") {
        values$physeq <- enterotype
        showNotification("Enterotype dataset loaded", type = "success")
      } else if (input$dataset_choice == "soil") {
        values$physeq <- soilrep
        showNotification("Soil microbiome dataset loaded", type = "success")
      }
    })
    
    # Update UI elements with new data
    updateUIElements()
  })
  
  # Upload custom phyloseq data
  observeEvent(input$phyloseq_file, {
    req(input$phyloseq_file)
    
    tryCatch({
      withProgress(message = "Loading custom dataset...", {
        values$physeq <- readRDS(input$phyloseq_file$datapath)
        showNotification("Custom dataset loaded successfully", type = "success")
      })
      updateUIElements()
    }, error = function(e) {
      showNotification(
        paste("Error loading file:", e$message),
        type = "error",
        duration = NULL
      )
    })
  })
  
  # Update UI elements when data changes
  updateUIElements <- function() {
    req(values$physeq)
    
    # Update group variable choices
    sample_vars <- names(sample_data(values$physeq))
    updateSelectInput(session, "group_var", choices = sample_vars, selected = sample_vars[1])
    updateSelectInput(session, "color_by", choices = sample_vars, selected = sample_vars[1])
  }
  
  # Data Overview Outputs
  output$data_summary <- renderPrint({
    req(values$physeq)
    values$physeq
  })
  
  output$n_samples <- renderValueBox({
    valueBox(
      value = ifelse(is.null(values$physeq), 0, nsamples(values$physeq)),
      subtitle = "Samples",
      icon = icon("vial"),
      color = "blue"
    )
  })
  
  output$n_taxa <- renderValueBox({
    valueBox(
      value = ifelse(is.null(values$physeq), 0, ntaxa(values$physeq)),
      subtitle = "Taxa",
      icon = icon("bacteria"),
      color = "green"
    )
  })
  
  output$n_groups <- renderValueBox({
    n_groups <- 0
    if (!is.null(values$physeq) && !is.null(input$group_var)) {
      n_groups <- length(unique(sample_data(values$physeq)[[input$group_var]]))
    }
    
    valueBox(
      value = n_groups,
      subtitle = "Groups",
      icon = icon("layer-group"),
      color = "yellow"
    )
  })
  
  # Metadata table
  output$metadata_table <- renderDataTable({
    req(values$physeq)
    
    DT::datatable(
      as.data.frame(sample_data(values$physeq)),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      extensions = 'Buttons'
    )
  })
  
  # Taxonomic overview plot
  output$taxa_plot <- renderPlotly({
    req(values$physeq)
    
    # Get top 20 taxa by abundance
    top_taxa <- names(sort(taxa_sums(values$physeq), decreasing = TRUE)[1:20])
    ps_top <- prune_taxa(top_taxa, values$physeq)
    
    # Calculate mean abundance
    abundance_mat <- as.matrix(otu_table(ps_top))
    if (!taxa_are_rows(ps_top)) {
      abundance_mat <- t(abundance_mat)
    }
    
    mean_abundance <- rowMeans(abundance_mat)
    taxa_names <- names(mean_abundance)
    
    # Create plot
    plot_ly(
      x = mean_abundance,
      y = factor(taxa_names, levels = taxa_names[order(mean_abundance)]),
      type = "bar",
      orientation = "h",
      marker = list(color = viridis::viridis(20))
    ) %>%
      layout(
        title = "Top 20 Taxa by Mean Abundance",
        xaxis = list(title = "Mean Abundance", type = "log"),
        yaxis = list(title = ""),
        margin = list(l = 200)
      )
  })
  
  # Universal Analysis
  observeEvent(input$run_universal, {
    req(values$physeq)
    
    withProgress(message = "Extracting universal information...", {
      incProgress(0.2, detail = "Calculating diversity metrics...")
      
      tryCatch({
        values$universal_info <- extract_universal_information(
          values$physeq,
          groups = input$group_var,
          include_phylogenetic = input$include_phylo
        )
        
        incProgress(0.6, detail = "Building transformation matrix...")
        
        showNotification(
          paste("Universal extraction complete! R² =", 
                round(values$universal_info$deconvolution_quality$mean_r_squared, 3)),
          type = "success",
          duration = 5
        )
      }, error = function(e) {
        showNotification(
          paste("Error in universal extraction:", e$message),
          type = "error",
          duration = NULL
        )
      })
    })
  })
  
  # Universal analysis completion flag
  output$universal_complete <- reactive({
    !is.null(values$universal_info)
  })
  outputOptions(output, "universal_complete", suspendWhenHidden = FALSE)
  
  # Quality info boxes
  output$overall_quality <- renderInfoBox({
    req(values$universal_info)
    
    quality <- values$universal_info$deconvolution_quality$overall_quality
    color <- switch(quality,
                    "Excellent" = "green",
                    "Good" = "yellow",
                    "Moderate" = "orange",
                    "red")
    
    infoBox(
      "Overall Quality",
      quality,
      icon = icon("check-circle"),
      color = color,
      fill = TRUE
    )
  })
  
  output$mean_r2 <- renderInfoBox({
    req(values$universal_info)
    
    r2 <- round(values$universal_info$deconvolution_quality$mean_r_squared, 3)
    color <- ifelse(r2 > 0.9, "green", ifelse(r2 > 0.7, "yellow", "red"))
    
    infoBox(
      "Mean R²",
      r2,
      icon = icon("chart-line"),
      color = color,
      fill = TRUE
    )
  })
  
  output$n_components <- renderInfoBox({
    req(values$universal_info)
    
    components <- values$universal_info$deconvolution_quality$components_present
    n_comp <- sum(components)
    
    infoBox(
      "Active Components",
      paste(n_comp, "/ 4"),
      subtitle = paste(names(components)[components], collapse = ", "),
      icon = icon("cubes"),
      color = "purple",
      fill = TRUE
    )
  })
  
  # Components visualization
  output$components_plot <- renderPlotly({
    req(values$universal_info)
    
    # Get first 20 samples for clarity
    n_show <- min(20, nrow(values$universal_info$information_components))
    components <- values$universal_info$information_components[1:n_show, ]
    
    # Create stacked bar chart
    plot_ly() %>%
      add_trace(
        x = components$sample_id,
        y = components$R_proportion * 100,
        name = "Richness (R)",
        type = "bar",
        marker = list(color = "#FF6B6B"),
        text = paste0(round(components$R_proportion * 100, 1), "%"),
        textposition = "inside",
        hovertemplate = "Richness: %{y:.1f}%<extra></extra>"
      ) %>%
      add_trace(
        x = components$sample_id,
        y = components$E_proportion * 100,
        name = "Evenness (E)",
        type = "bar",
        marker = list(color = "#4ECDC4"),
        text = paste0(round(components$E_proportion * 100, 1), "%"),
        textposition = "inside",
        hovertemplate = "Evenness: %{y:.1f}%<extra></extra>"
      ) %>%
      add_trace(
        x = components$sample_id,
        y = components$P_proportion * 100,
        name = "Phylogenetic (P)",
        type = "bar",
        marker = list(color = "#45B7D1"),
        text = paste0(round(components$P_proportion * 100, 1), "%"),
        textposition = "inside",
        hovertemplate = "Phylogenetic: %{y:.1f}%<extra></extra>"
      ) %>%
      add_trace(
        x = components$sample_id,
        y = components$S_proportion * 100,
        name = "Spatial (S)",
        type = "bar",
        marker = list(color = "#FFA07A"),
        text = paste0(round(components$S_proportion * 100, 1), "%"),
        textposition = "inside",
        hovertemplate = "Spatial: %{y:.1f}%<extra></extra>"
      ) %>%
      layout(
        barmode = "stack",
        title = "Information Component Proportions by Sample",
        xaxis = list(title = "Sample ID", tickangle = -45),
        yaxis = list(title = "Proportion (%)", range = c(0, 100)),
        hovermode = "x unified",
        showlegend = TRUE,
        legend = list(x = 0.7, y = 1)
      )
  })
  
  # Component distribution plot
  output$component_dist <- renderPlotly({
    req(values$universal_info)
    
    components <- values$universal_info$information_components
    
    # Create box plots for each component
    plot_ly() %>%
      add_trace(
        y = components$R_component,
        name = "Richness",
        type = "box",
        marker = list(color = "#FF6B6B")
      ) %>%
      add_trace(
        y = components$E_component,
        name = "Evenness",
        type = "box",
        marker = list(color = "#4ECDC4")
      ) %>%
      add_trace(
        y = components$P_component,
        name = "Phylogenetic",
        type = "box",
        marker = list(color = "#45B7D1")
      ) %>%
      add_trace(
        y = components$S_component,
        name = "Spatial",
        type = "box",
        marker = list(color = "#FFA07A")
      ) %>%
      layout(
        title = "Distribution of Information Components",
        yaxis = list(title = "Component Value"),
        showlegend = FALSE
      )
  })
  
  # Transformation matrix table
  output$transformation_matrix <- renderDataTable({
    req(values$universal_info)
    
    trans_mat <- values$universal_info$transformation_matrix
    trans_mat$r_squared <- round(trans_mat$r_squared, 3)
    
    # Color code by R²
    DT::datatable(
      trans_mat,
      options = list(
        pageLength = 15,
        order = list(list(2, 'desc'))  # Sort by R² descending
      )
    ) %>%
      formatStyle(
        'r_squared',
        backgroundColor = styleInterval(
          c(0.7, 0.9),
          c('#ffcccc', '#ffffcc', '#ccffcc')
        )
      )
  })
  
  # Quality plot
  output$quality_plot <- renderPlotly({
    req(values$universal_info)
    
    quality_data <- values$universal_info$metric_quality
    
    plot_ly(
      x = quality_data$metric,
      y = quality_data$mean_r_squared,
      type = "bar",
      marker = list(
        color = quality_data$mean_r_squared,
        colorscale = list(c(0, "red"), c(0.5, "yellow"), c(1, "green")),
        showscale = TRUE,
        colorbar = list(title = "R²")
      ),
      text = round(quality_data$mean_r_squared, 3),
      textposition = "outside"
    ) %>%
      layout(
        title = "Transformation Quality by Metric",
        xaxis = list(title = "Metric", tickangle = -45),
        yaxis = list(title = "Mean R²", range = c(0, 1)),
        margin = list(b = 100)
      )
  })
  
  # Metric Transformation
  observeEvent(input$add_source, {
    values$source_metrics[[input$source_metric]] <- input$source_value
    showNotification(
      paste("Added", input$source_metric, "=", input$source_value),
      type = "info"
    )
  })
  
  output$source_list <- renderPrint({
    if (length(values$source_metrics) == 0) {
      cat("No source metrics added yet.\nCurrent selection: ", 
          input$source_metric, " = ", input$source_value)
    } else {
      cat("Source metrics:\n")
      for (metric in names(values$source_metrics)) {
        cat(paste0("  ", metric, " = ", values$source_metrics[[metric]], "\n"))
      }
    }
  })
  
  observeEvent(input$transform, {
    req(values$universal_info)
    req(length(input$target_metrics) > 0)
    
    # Use current input if no sources added
    if (length(values$source_metrics) == 0) {
      values$source_metrics[[input$source_metric]] <- input$source_value
    }
    
    withProgress(message = "Transforming metrics...", {
      tryCatch({
        values$transform_results <- universal_diversity_transform(
          source_metrics = values$source_metrics,
          target_metrics = input$target_metrics,
          transformation_matrix = values$universal_info$transformation_matrix
        )
        
        showNotification(
          "Transformation complete!",
          type = "success"
        )
      }, error = function(e) {
        showNotification(
          paste("Transformation error:", e$message),
          type = "error"
        )
      })
    })
  })
  
  output$transform_complete <- reactive({
    !is.null(values$transform_results)
  })
  outputOptions(output, "transform_complete", suspendWhenHidden = FALSE)
  
  # Transformation results
  output$transform_results <- renderPlotly({
    req(values$transform_results)
    
    # Create bar plot of predictions
    pred_data <- data.frame(
      metric = names(values$transform_results$predictions),
      value = unlist(values$transform_results$predictions),
      lower = unlist(values$transform_results$confidence_intervals$lower),
      upper = unlist(values$transform_results$confidence_intervals$upper)
    )
    
    plot_ly(pred_data) %>%
      add_trace(
        x = ~metric,
        y = ~value,
        type = "bar",
        marker = list(color = "steelblue"),
        error_y = list(
          type = "data",
          symmetric = FALSE,
          array = ~upper - value,
          arrayminus = ~value - lower,
          color = "black"
        ),
        text = round(pred_data$value, 3),
        textposition = "outside"
      ) %>%
      layout(
        title = "Predicted Metric Values with 95% CI",
        xaxis = list(title = "Metric"),
        yaxis = list(title = "Predicted Value"),
        showlegend = FALSE
      )
  })
  
  output$predicted_table <- renderTable({
    req(values$transform_results)
    
    data.frame(
      Metric = names(values$transform_results$predictions),
      Predicted = round(unlist(values$transform_results$predictions), 3),
      `95% CI` = paste0(
        "[", 
        round(unlist(values$transform_results$confidence_intervals$lower), 3),
        ", ",
        round(unlist(values$transform_results$confidence_intervals$upper), 3),
        "]"
      ),
      check.names = FALSE
    )
  })
  
  output$transform_quality <- renderPlotly({
    req(values$transform_results)
    
    quality_data <- data.frame(
      metric = names(values$transform_results$transformation_quality),
      r2 = unlist(values$transform_results$transformation_quality)
    )
    
    plot_ly(
      labels = quality_data$metric,
      values = quality_data$r2,
      type = "pie",
      marker = list(colors = viridis::viridis(nrow(quality_data))),
      textinfo = "label+percent",
      hovertemplate = "%{label}<br>R² = %{value:.3f}<extra></extra>"
    ) %>%
      layout(
        title = "Transformation Quality (R²)"
      )
  })
  
  # Network visualization
  output$network_plot <- renderPlotly({
    req(values$universal_info)
    
    # This would use the actual network plotting function
    # For now, create a placeholder
    showNotification(
      "Network visualization coming soon!",
      type = "info"
    )
    
    plot_ly() %>%
      layout(
        title = "Metric Relationship Network",
        annotations = list(
          text = "Network visualization will appear here",
          showarrow = FALSE,
          x = 0.5,
          y = 0.5,
          xref = "paper",
          yref = "paper"
        )
      )
  })
  
  # Information space plot
  output$info_space_plot <- renderPlotly({
    req(values$universal_info)
    req(input$x_component)
    req(input$y_component)
    
    components <- values$universal_info$information_components
    
    # Add color variable
    color_var <- if (!is.null(input$color_by) && !is.null(values$physeq)) {
      sample_data(values$physeq)[[input$color_by]][match(components$sample_id, sample_names(values$physeq))]
    } else {
      rep("All", nrow(components))
    }
    
    plot_ly(
      x = components[[input$x_component]],
      y = components[[input$y_component]],
      color = color_var,
      colors = viridis::viridis(length(unique(color_var))),
      type = "scatter",
      mode = "markers",
      marker = list(size = 10),
      text = components$sample_id,
      hovertemplate = paste(
        "Sample: %{text}<br>",
        input$x_component, ": %{x:.3f}<br>",
        input$y_component, ": %{y:.3f}<extra></extra>"
      )
    ) %>%
      layout(
        title = "Information Component Space",
        xaxis = list(title = input$x_component),
        yaxis = list(title = input$y_component)
      )
  })
  
  # AI Interpretation
  observeEvent(input$interpret, {
    req(values$universal_info)
    
    withProgress(message = "Generating AI interpretation...", {
      tryCatch({
        # Build context
        context <- list(
          environment = input$environment,
          condition = input$condition,
          organism = input$organism_type
        )
        
        # Add options
        if ("mechanisms" %in% input$interpretation_options) {
          context$include_mechanisms <- TRUE
        }
        if ("hypotheses" %in% input$interpretation_options) {
          context$include_hypotheses <- TRUE
        }
        
        values$interpretation <- interpret_diversity(
          values$universal_info,
          context = context
        )
        
        showNotification(
          "Interpretation generated!",
          type = "success"
        )
      }, error = function(e) {
        showNotification(
          paste("Interpretation error:", e$message),
          type = "error"
        )
      })
    })
  })
  
  output$interpretation_complete <- reactive({
    !is.null(values$interpretation)
  })
  outputOptions(output, "interpretation_complete", suspendWhenHidden = FALSE)
  
  output$interpretation_text <- renderUI({
    req(values$interpretation)
    
    HTML(paste0(
      "<div style='line-height: 1.6;'>",
      gsub("\n", "<br>", values$interpretation$interpretation),
      "</div>"
    ))
  })
  
  output$patterns_list <- renderUI({
    req(values$interpretation)
    
    if (!is.null(values$interpretation$patterns)) {
      pattern_items <- lapply(values$interpretation$patterns, function(p) {
        tags$li(p)
      })
      tags$ul(pattern_items)
    } else {
      tags$p("No specific patterns identified.")
    }
  })
  
  output$followup_list <- renderUI({
    req(values$interpretation)
    
    if (!is.null(values$interpretation$suggestions)) {
      suggestion_items <- lapply(values$interpretation$suggestions, function(s) {
        tags$li(s)
      })
      tags$ul(suggestion_items)
    } else {
      tags$p("No follow-up suggestions available.")
    }
  })
  
  # Export functionality
  output$analysis_summary <- renderPrint({
    cat("Analysis Summary\n")
    cat("================\n\n")
    
    if (!is.null(values$physeq)) {
      cat("Dataset loaded: ", nsamples(values$physeq), " samples, ", 
          ntaxa(values$physeq), " taxa\n")
    }
    
    if (!is.null(values$universal_info)) {
      cat("\nUniversal analysis complete:\n")
      cat("  - Mean R²: ", round(values$universal_info$deconvolution_quality$mean_r_squared, 3), "\n")
      cat("  - Quality: ", values$universal_info$deconvolution_quality$overall_quality, "\n")
    }
    
    if (!is.null(values$transform_results)) {
      cat("\nMetric transformations performed: ", 
          length(values$transform_results$predictions), " metrics predicted\n")
    }
    
    if (!is.null(values$interpretation)) {
      cat("\nAI interpretation generated\n")
    }
  })
  
  output$download_results <- downloadHandler(
    filename = function() {
      paste0("diversityGPT_results_", Sys.Date(), ".", input$export_format)
    },
    content = function(file) {
      withProgress(message = "Preparing export...", {
        if (input$export_format == "rds") {
          # Export as R object
          export_list <- list()
          
          if ("universal" %in% input$export_options) {
            export_list$universal_info <- values$universal_info
          }
          if ("matrices" %in% input$export_options) {
            export_list$transformation_matrix <- values$universal_info$transformation_matrix
          }
          if ("interpretations" %in% input$export_options) {
            export_list$interpretation <- values$interpretation
          }
          
          saveRDS(export_list, file)
          
        } else if (input$export_format == "html") {
          # Generate HTML report
          # This would use rmarkdown to create a report
          writeLines("<h1>diversityGPT Analysis Report</h1>", file)
          showNotification("Full HTML export coming soon!", type = "info")
        }
      })
    }
  )
}