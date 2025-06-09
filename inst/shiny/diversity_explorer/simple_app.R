# Simple fallback app without shinydashboard
# In case dashboard dependencies cause issues

library(shiny)
library(plotly)
library(DT)
library(diversityGPT)
library(phyloseq)
library(dplyr)

# Load example data
data(GlobalPatterns)
data(enterotype)
data(soilrep)

# Load dataset registry first
registry_file <- "../../R/dataset_registry.R"
if (file.exists(registry_file)) {
  source(registry_file)
}

# Source modules if available
module_file <- "modules/dataset_browser.R"
if (file.exists(module_file)) {
  source(module_file)
}

enhanced_module_file <- "modules/enhanced_component_explorer_module.R"
if (file.exists(enhanced_module_file)) {
  source(enhanced_module_file)
}

# Simple UI
ui <- fluidPage(
  titlePanel(
    div(
      h1("diversityGPT Interactive Explorer"),
      p("Universal diversity metric transformation and analysis"),
      style = "text-align: center; background: #3498db; color: white; padding: 20px; margin: -15px -15px 20px -15px;"
    )
  ),
  
  navbarPage(
    "",
    
    # Dataset Browser tab (if module available)
    if (exists("dataset_browser_ui")) {
      create_dataset_browser_tab()
    } else {
      NULL
    },
    
    # Data tab
    tabPanel(
      "Data",
      
      fluidRow(
        column(
          width = 6,
          
          h3("Dataset Management"),
          
          # Show currently loaded dataset
          conditionalPanel(
            condition = "output.data_loaded",
            wellPanel(
              style = "background: #e8f4f8; border: 1px solid #3498db;",
              h4("Currently Loaded Dataset:", style = "margin-top: 0;"),
              textOutput("current_dataset_name", container = tags$h5),
              tags$hr(style = "margin: 10px 0;"),
              tags$small("Use the Dataset Browser tab to explore and load different datasets")
            )
          ),
          
          # Upload your own data section
          wellPanel(
            style = "background: #f9f9f9; border: 1px solid #ddd;",
            h4("📁 Upload Your Own Data", style = "margin-top: 0; color: #2c3e50;"),
            p("Support for phyloseq objects, BIOM files, QIIME2 artifacts, and MetaPhlAn profiles:"),
            
            fileInput(
              "user_data_file",
              "Choose file:",
              accept = c(".rds", ".biom", ".qza", ".txt", ".tsv", ".csv"),
              placeholder = "No file selected"
            ),
            
            conditionalPanel(
              condition = "output.file_uploaded",
              selectInput(
                "data_format",
                "Data format:",
                choices = c(
                  "Auto-detect" = "auto",
                  "phyloseq object (.rds)" = "phyloseq",
                  "BIOM format" = "biom", 
                  "QIIME2 artifact (.qza)" = "qiime2",
                  "MetaPhlAn profile" = "metaphlan",
                  "Tab-separated values" = "tsv"
                ),
                selected = "auto"
              ),
              
              actionButton("load_user_data", "Load My Data", 
                          class = "btn-success", icon = icon("upload"))
            )
          )
        ),
        
        column(
          width = 6,
          
          conditionalPanel(
            condition = "output.data_loaded",
            h3("📊 Dataset Details"),
            
            # Quick stats
            wellPanel(
              style = "background: white;",
              fluidRow(
                column(4, 
                  div(style = "text-align: center;",
                    h2(textOutput("n_samples_display"), style = "color: #3498db; margin: 5px 0;"),
                    p("Samples", style = "margin: 0; font-weight: bold;")
                  )
                ),
                column(4,
                  div(style = "text-align: center;",
                    h2(textOutput("n_taxa_display"), style = "color: #e74c3c; margin: 5px 0;"),
                    p("Taxa", style = "margin: 0; font-weight: bold;")
                  )
                ),
                column(4,
                  div(style = "text-align: center;",
                    h2(textOutput("n_groups_display"), style = "color: #27ae60; margin: 5px 0;"),
                    p("Groups", style = "margin: 0; font-weight: bold;")
                  )
                )
              )
            ),
            
            # Study information
            wellPanel(
              h4("📚 Study Information"),
              uiOutput("study_details")
            ),
            
            # Data composition plots
            wellPanel(
              h4("📈 Data Overview"),
              tabsetPanel(
                tabPanel("Group Overview", 
                  br(),
                  plotlyOutput("group_overview_plot", height = "300px")
                ),
                tabPanel("Taxa Summary",
                  br(), 
                  plotlyOutput("taxa_summary_plot", height = "300px")
                ),
                tabPanel("Metadata",
                  br(),
                  div(style = "max-height: 350px; overflow-y: auto;",
                    DT::dataTableOutput("metadata_table")
                  )
                )
              )
            )
          ),
          
          # Dataset summary (condensed)
          conditionalPanel(
            condition = "output.data_loaded",
            wellPanel(
              h4("🔍 Technical Summary"),
              verbatimTextOutput("data_summary_condensed")
            )
          )
        )
      )
    ),
    
    # Universal Analysis tab
    tabPanel(
      "Universal Analysis",
      
      fluidRow(
        column(
          width = 4,
          
          h3("🔧 Analysis Settings"),
          
          wellPanel(
            h4("Sample Grouping", style = "margin-top: 0;"),
            selectInput(
              "group_var",
              "Group samples by:",
              choices = NULL
            ),
            tags$small("This affects the information components plot and sample-level analysis", 
                      style = "color: #7f8c8d;")
          ),
          
          wellPanel(
            h4("Advanced Options", style = "margin-top: 0;"),
            checkboxInput(
              "include_phylo",
              "Include phylogenetic components",
              value = FALSE
            ),
            tags$small("Adds phylogenetic diversity (P) calculations if tree is available", 
                      style = "color: #7f8c8d;")
          ),
          
          br(),
          
          actionButton(
            "run_universal",
            "🚀 Extract Universal Information",
            class = "btn-success btn-lg",
            style = "width: 100%; font-size: 16px; padding: 12px;"
          ),
          
          br(), br(),
          
          # Add analysis status
          conditionalPanel(
            condition = "output.universal_complete",
            wellPanel(
              style = "background: #e8f5e9; border: 1px solid #4caf50;",
              h4("✅ Analysis Complete", style = "color: #2e7d32; margin-top: 0;"),
              textOutput("analysis_summary")
            )
          )
        ),
        
        column(
          width = 8,
          
          conditionalPanel(
            condition = "output.universal_complete",
            
            h3("📊 Analysis Results"),
            
            # Quality overview
            wellPanel(
              h4("🎯 Analysis Quality", style = "margin-top: 0;"),
              verbatimTextOutput("quality_info")
            ),
            
            # Results tabs
            tabsetPanel(
              tabPanel(
                "📈 Information Components",
                br(),
                div(
                  style = "background: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                  h5("ℹ️ About Information Components", style = "margin-top: 0;"),
                  p("This plot shows how each sample's diversity breaks down into fundamental information components (R, E, P, S). The grouping variable affects how samples are colored and organized.", style = "margin-bottom: 0;")
                ),
                plotlyOutput("components_plot", height = "400px")
              ),
              
              tabPanel(
                "🔢 Universal Transformation Matrix",
                br(),
                div(
                  style = "background: #fff3cd; padding: 15px; border-radius: 8px; margin-bottom: 15px; border-left: 4px solid #ffc107;",
                  h5("ℹ️ About the Transformation Matrix", style = "margin-top: 0;"),
                  p(tags$strong("This matrix shows universal mathematical relationships"), " between diversity metrics and information components (R, E, P, S). ", 
                    "These relationships are ", tags$em("dataset-level constants"), " that don't change with grouping variables.", style = "margin-bottom: 0;")
                ),
                DT::dataTableOutput("trans_matrix")
              ),
              
              tabPanel(
                "📋 Group Summary",
                br(),
                div(
                  style = "background: #e3f2fd; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                  h5("ℹ️ Group-Specific Analysis", style = "margin-top: 0;"),
                  p("Summary statistics for each group based on the selected grouping variable.", style = "margin-bottom: 0;")
                ),
                DT::dataTableOutput("group_summary_table")
              )
            )
          )
        )
      )
    ),
    
    # Transform tab
    tabPanel(
      "Transform Metrics",
      
      fluidRow(
        column(
          width = 6,
          
          h3("Source Metrics"),
          
          selectInput(
            "source_metric",
            "Metric:",
            choices = c(
              "Shannon" = "shannon",
              "Simpson" = "simpson",
              "Observed" = "observed",
              "Chao1" = "chao1"
            )
          ),
          
          numericInput(
            "source_value",
            "Value:",
            value = 2.5,
            min = 0,
            step = 0.1
          ),
          
          h3("Target Metrics"),
          
          checkboxGroupInput(
            "target_metrics",
            "Predict:",
            choices = c(
              "Shannon" = "shannon",
              "Simpson" = "simpson",
              "Observed" = "observed", 
              "Chao1" = "chao1",
              "Pielou" = "pielou_evenness"
            ),
            selected = c("simpson", "chao1")
          ),
          
          br(),
          
          actionButton(
            "transform",
            "Transform Metrics",
            class = "btn-warning btn-lg",
            style = "width: 100%;"
          )
        ),
        
        column(
          width = 6,
          
          conditionalPanel(
            condition = "output.transform_complete",
            
            h3("Predictions"),
            
            tableOutput("predictions"),
            
            br(),
            
            plotlyOutput("prediction_plot", height = "300px")
          )
        )
      )
    ),
    
    # Enhanced Component Explorer tab
    tabPanel(
      "Component Explorer",
      
      if (exists("enhanced_component_explorer_ui")) {
        enhanced_component_explorer_ui("component_explorer")
      } else {
        # Fallback to simple version if module not loaded
        div(
          h3("Component Explorer"),
          p("Enhanced component explorer module not available. Please check installation.")
        )
      }
    ),
    
    # Visualization tab
    tabPanel(
      "Plots",
      
      conditionalPanel(
        condition = "output.universal_complete",
        
        h3("Information Space"),
        
        fluidRow(
          column(
            width = 3,
            
            selectInput(
              "x_comp",
              "X-axis:",
              choices = c(
                "Richness" = "R_component",
                "Evenness" = "E_component",
                "Phylogenetic" = "P_component",
                "Spatial" = "S_component"
              ),
              selected = "R_component"
            ),
            
            selectInput(
              "y_comp", 
              "Y-axis:",
              choices = c(
                "Richness" = "R_component",
                "Evenness" = "E_component", 
                "Phylogenetic" = "P_component",
                "Spatial" = "S_component"
              ),
              selected = "E_component"
            )
          ),
          
          column(
            width = 9,
            plotlyOutput("info_space", height = "500px")
          )
        )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  values <- reactiveValues(
    physeq = NULL,
    universal_info = NULL,
    transform_results = NULL,
    current_dataset_name = NULL
  )
  
  # Dataset browser module (if available)
  if (exists("dataset_browser_server")) {
    loaded_data <- dataset_browser_server("dataset_browser")
    
    # Watch for loaded data from browser
    observeEvent(loaded_data(), {
      if (!is.null(loaded_data())) {
        values$physeq <- loaded_data()
        
        # Get dataset name from the dataset browser's selected dataset
        browser_module <- session$moduleServer$dataset_browser
        if (!is.null(browser_module) && !is.null(browser_module$values$selected_dataset)) {
          values$current_dataset_name <- browser_module$values$selected_dataset$name
        } else {
          # Fallback: try to detect dataset type
          if (nsamples(loaded_data()) == 56 && ntaxa(loaded_data()) == 2000) {
            values$current_dataset_name <- "Soil Microbiome (Precomputed Demo)"
          } else if (nsamples(loaded_data()) == 26) {
            values$current_dataset_name <- "GlobalPatterns (Precomputed)"
          } else {
            values$current_dataset_name <- paste("Dataset (", nsamples(loaded_data()), " samples)")
          }
        }
        
        # Update group choices
        if (!is.null(values$physeq)) {
          choices <- names(sample_data(values$physeq))
          updateSelectInput(session, "group_var", choices = choices, selected = choices[1])
        }
      }
    })
  }
  
  # Enhanced Component Explorer module (if available)
  if (exists("enhanced_component_explorer_server")) {
    enhanced_component_explorer_server("component_explorer", reactive(values$universal_info))
  }
  
  # File upload detection
  output$file_uploaded <- reactive({
    !is.null(input$user_data_file)
  })
  outputOptions(output, "file_uploaded", suspendWhenHidden = FALSE)
  
  # Load user data
  observeEvent(input$load_user_data, {
    req(input$user_data_file)
    
    withProgress(message = "Processing your data...", {
      tryCatch({
        file_path <- input$user_data_file$datapath
        file_name <- input$user_data_file$name
        
        # Determine format
        format <- if (input$data_format == "auto") {
          if (!exists("detect_data_format")) {
            source("R/format_converters.R")
          }
          detect_data_format(file_path)
        } else {
          input$data_format
        }
        
        # Load based on format
        if (format == "phyloseq") {
          values$physeq <- readRDS(file_path)
          values$current_dataset_name <- paste("Custom:", gsub("\\.[^.]*$", "", file_name))
        } else {
          # Use format converters
          if (!exists("convert_to_phyloseq")) {
            source("R/format_converters.R")
          }
          values$physeq <- convert_to_phyloseq(file_path, format = format)
          values$current_dataset_name <- paste("Custom:", gsub("\\.[^.]*$", "", file_name), "(", format, ")")
        }
        
        # Update group choices
        if (!is.null(values$physeq)) {
          choices <- names(sample_data(values$physeq))
          updateSelectInput(session, "group_var", choices = choices, selected = choices[1])
        }
        
        showNotification(
          paste("Successfully loaded:", file_name),
          type = "message",
          duration = 3
        )
        
      }, error = function(e) {
        showNotification(
          paste("Error loading file:", e$message),
          type = "error",
          duration = 8
        )
      })
    })
  })
  
  output$data_loaded <- reactive({
    !is.null(values$physeq)
  })
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
  
  output$current_dataset_name <- renderText({
    if (!is.null(values$current_dataset_name)) {
      values$current_dataset_name
    } else if (!is.null(values$physeq)) {
      paste("Unknown Dataset (", nsamples(values$physeq), " samples)")
    } else {
      "No dataset loaded"
    }
  })
  
  output$data_summary <- renderPrint({
    req(values$physeq)
    values$physeq
  })
  
  # Enhanced data displays
  output$n_samples_display <- renderText({
    req(values$physeq)
    format(nsamples(values$physeq), big.mark = ",")
  })
  
  output$n_taxa_display <- renderText({
    req(values$physeq)
    format(ntaxa(values$physeq), big.mark = ",")
  })
  
  output$n_groups_display <- renderText({
    req(values$physeq, input$group_var)
    n_groups <- length(unique(sample_data(values$physeq)[[input$group_var]]))
    as.character(n_groups)
  })
  
  # Study details
  output$study_details <- renderUI({
    req(values$physeq)
    
    # Try to get study info from dataset registry
    study_info <- tryCatch({
      # Try to match with known datasets
      if (!is.null(values$current_dataset_name)) {
        if (grepl("GlobalPatterns", values$current_dataset_name)) {
          list(
            title = "Global Patterns of 16S rRNA diversity",
            description = "A comprehensive survey of microbial communities across diverse environments worldwide, from human gut to soil to ocean samples.",
            citation = "Caporaso et al. (2011) PNAS 108:4516-4522",
            environment = "Multiple environments",
            sequencing = "16S rRNA V4 region, 454 pyrosequencing"
          )
        } else if (grepl("Enterotype", values$current_dataset_name)) {
          list(
            title = "Enterotypes of the human gut microbiome",
            description = "Classification of human gut microbiomes into distinct enterotypes based on bacterial composition.",
            citation = "Arumugam et al. (2011) Nature 473:174-180", 
            environment = "Human gut microbiome",
            sequencing = "16S rRNA V3-V4 region, Illumina"
          )
        } else if (grepl("Soil", values$current_dataset_name)) {
          list(
            title = "Soil microbiome reproducibility study",
            description = "Investigation of soil microbial community responses to warming and plant clipping treatments.",
            citation = "Shade et al. (2012) ISME J 7:540-544",
            environment = "Agricultural soil",
            sequencing = "16S rRNA, 454 pyrosequencing"
          )
        } else {
          list(
            title = "Custom Dataset",
            description = "User-uploaded dataset",
            citation = "Not available",
            environment = "Unknown",
            sequencing = "Unknown"
          )
        }
      }
    }, error = function(e) NULL)
    
    if (!is.null(study_info)) {
      tagList(
        tags$table(
          class = "table table-sm",
          tags$tr(
            tags$td(tags$strong("Study:")),
            tags$td(study_info$title)
          ),
          tags$tr(
            tags$td(tags$strong("Environment:")),
            tags$td(study_info$environment)  
          ),
          tags$tr(
            tags$td(tags$strong("Sequencing:")),
            tags$td(study_info$sequencing)
          ),
          tags$tr(
            tags$td(tags$strong("Citation:")),
            tags$td(tags$em(study_info$citation))
          )
        ),
        tags$p(study_info$description, style = "margin-top: 10px; font-style: italic;")
      )
    } else {
      p("Study information not available")
    }
  })
  
  # Group overview plot - more meaningful than individual samples
  output$group_overview_plot <- renderPlotly({
    req(values$physeq, input$group_var)
    
    tryCatch({
      # Calculate basic diversity metrics for each group
      sample_df <- data.frame(sample_data(values$physeq))
      otu_table_data <- as(otu_table(values$physeq), "matrix")
      if (!taxa_are_rows(values$physeq)) {
        otu_table_data <- t(otu_table_data)
      }
      
      # Calculate richness (observed species) for each sample
      richness <- colSums(otu_table_data > 0)
      sample_df$Richness <- richness[rownames(sample_df)]
      
      # Group by the selected variable
      unique_groups <- unique(sample_df[[input$group_var]])
      
      if (length(unique_groups) <= 20) {
        # Create boxplot showing richness distribution by group
        plot_ly(
          data = sample_df,
          x = ~get(input$group_var),
          y = ~Richness,
          type = "box",
          marker = list(color = "#3498db"),
          line = list(color = "#2980b9")
        ) %>%
          layout(
            title = paste("Taxa Richness by", input$group_var),
            xaxis = list(title = input$group_var),
            yaxis = list(title = "Number of Observed Taxa"),
            margin = list(b = 100)
          )
      } else {
        # For continuous variables or too many categories, show scatter/summary
        group_summary <- aggregate(sample_df$Richness, 
                                 by = list(sample_df[[input$group_var]]), 
                                 FUN = function(x) c(mean = mean(x), sd = sd(x), n = length(x)))
        
        p("Too many categories for meaningful visualization. Consider selecting a different grouping variable.", 
          style = "text-align: center; color: #7f8c8d; margin-top: 50px;")
      }
    }, error = function(e) {
      # Fallback to simple group counts
      sample_df <- data.frame(sample_data(values$physeq))
      group_counts <- table(sample_df[[input$group_var]])
      
      if (length(group_counts) <= 20) {
        plot_ly(
          x = names(group_counts),
          y = as.numeric(group_counts),
          type = "bar",
          marker = list(color = "#3498db")
        ) %>%
          layout(
            title = paste("Sample Counts by", input$group_var),
            xaxis = list(title = input$group_var),
            yaxis = list(title = "Number of Samples")
          )
      } else {
        p("Unable to create meaningful visualization for this grouping variable.")
      }
    })
  })
  
  # Taxa summary plot - more informative abundance visualization
  output$taxa_summary_plot <- renderPlotly({
    req(values$physeq)
    
    tryCatch({
      # Get taxa abundance data
      otu_table_data <- as(otu_table(values$physeq), "matrix")
      if (!taxa_are_rows(values$physeq)) {
        otu_table_data <- t(otu_table_data)
      }
      
      # Calculate prevalence (% of samples where each taxon appears)
      prevalence <- rowSums(otu_table_data > 0) / ncol(otu_table_data) * 100
      
      # Calculate mean relative abundance for each taxon
      total_counts <- rowSums(otu_table_data)
      total_across_samples <- sum(otu_table_data)
      mean_abundance <- total_counts / total_across_samples * 100
      
      # Create scatter plot of prevalence vs abundance
      plot_ly(
        x = prevalence,
        y = mean_abundance,
        type = "scatter",
        mode = "markers",
        marker = list(
          color = "#e74c3c",
          size = 6,
          opacity = 0.6,
          line = list(color = "#c0392b", width = 1)
        ),
        hovertemplate = "Prevalence: %{x:.1f}%<br>Mean Abundance: %{y:.3f}%<extra></extra>"
      ) %>%
        layout(
          title = "Taxa Prevalence vs Abundance",
          xaxis = list(title = "Prevalence (% of samples)"),
          yaxis = list(title = "Mean Relative Abundance (%)", type = "log"),
          margin = list(l = 60)
        )
    }, error = function(e) {
      # Fallback to simpler plot
      otu_table_data <- as(otu_table(values$physeq), "matrix")
      if (!taxa_are_rows(values$physeq)) {
        otu_table_data <- t(otu_table_data)
      }
      
      taxa_per_sample <- colSums(otu_table_data > 0)
      
      plot_ly(
        x = taxa_per_sample,
        type = "histogram",
        marker = list(color = "#e74c3c"),
        nbinsx = 15
      ) %>%
        layout(
          title = "Distribution of Taxa per Sample",
          xaxis = list(title = "Number of Taxa per Sample"),
          yaxis = list(title = "Number of Samples")
        )
    })
  })
  
  # Metadata table
  output$metadata_table <- renderDataTable({
    req(values$physeq)
    
    sample_df <- data.frame(sample_data(values$physeq))
    
    datatable(
      sample_df,
      options = list(
        pageLength = 8,  # Fewer rows to fit in container
        scrollX = TRUE,
        scrollY = "250px",  # Fixed height
        dom = 'tip',
        columnDefs = list(list(width = '150px', targets = "_all"))
      ),
      rownames = TRUE,
      class = 'compact stripe'
    ) %>%
      formatStyle(columns = 1:ncol(sample_df), fontSize = '12px')
  })
  
  # Condensed summary
  output$data_summary_condensed <- renderPrint({
    req(values$physeq)
    cat("phyloseq object with:\n")
    cat("- OTU Table:   [", ntaxa(values$physeq), " taxa,", nsamples(values$physeq), "samples ]\n")
    cat("- Sample Data: [", nsamples(values$physeq), "samples,", ncol(sample_data(values$physeq)), "variables ]\n")
    if (!is.null(tax_table(values$physeq, errorIfNULL = FALSE))) {
      cat("- Taxonomy:    [", ntaxa(values$physeq), "taxa,", ncol(tax_table(values$physeq)), "ranks ]\n")
    }
    if (!is.null(phy_tree(values$physeq, errorIfNULL = FALSE))) {
      cat("- Tree:        [ Phylogenetic tree with", ntaxa(values$physeq), "tips ]\n")
    }
  })
  
  # Universal analysis
  observeEvent(input$run_universal, {
    req(values$physeq)
    
    withProgress(message = "Extracting universal information...", {
      values$universal_info <- extract_universal_information(
        values$physeq,
        groups = input$group_var,
        include_phylogenetic = input$include_phylo
      )
    })
  })
  
  output$universal_complete <- reactive({
    !is.null(values$universal_info)
  })
  outputOptions(output, "universal_complete", suspendWhenHidden = FALSE)
  
  # Analysis summary
  output$analysis_summary <- renderText({
    req(values$universal_info, input$group_var)
    
    n_samples <- nrow(values$universal_info$information_components)
    n_groups <- length(unique(values$universal_info$information_components[[input$group_var]]))
    
    paste0("Analyzed ", n_samples, " samples across ", n_groups, " groups (", input$group_var, ")")
  })
  
  output$quality_info <- renderPrint({
    req(values$universal_info)
    
    cat("Overall Quality:", values$universal_info$deconvolution_quality$overall_quality, "\n")
    cat("Mean R²:", round(values$universal_info$deconvolution_quality$mean_r_squared, 3), "\n")
    cat("Components:", paste(names(values$universal_info$deconvolution_quality$components_present)[values$universal_info$deconvolution_quality$components_present], collapse = ", "))
  })
  
  # Group summary table - this DOES change with grouping variable
  output$group_summary_table <- renderDataTable({
    req(values$universal_info, input$group_var)
    
    tryCatch({
      # Get information components
      comp_data <- values$universal_info$information_components
      
      if (input$group_var %in% colnames(comp_data)) {
        # Calculate group statistics
        group_stats <- comp_data %>%
          group_by(!!sym(input$group_var)) %>%
          summarise(
            Samples = n(),
            Mean_R = round(mean(R_proportion, na.rm = TRUE), 3),
            Mean_E = round(mean(E_proportion, na.rm = TRUE), 3),
            Mean_P = if("P_proportion" %in% colnames(comp_data)) round(mean(P_proportion, na.rm = TRUE), 3) else NA,
            Mean_S = if("S_proportion" %in% colnames(comp_data)) round(mean(S_proportion, na.rm = TRUE), 3) else NA,
            .groups = 'drop'
          )
        
        # Remove NA columns
        group_stats <- group_stats[!sapply(group_stats, function(x) all(is.na(x)))]
        
        datatable(
          group_stats,
          options = list(
            pageLength = 10,
            dom = 'tip',
            columnDefs = list(list(className = 'dt-center', targets = "_all"))
          ),
          rownames = FALSE
        ) %>%
          formatStyle(
            columns = c("Mean_R", "Mean_E", "Mean_P", "Mean_S"),
            backgroundColor = styleInterval(c(0.3, 0.7), c("#ffebee", "#fff3e0", "#e8f5e9"))
          )
      } else {
        # Fallback if grouping variable not found
        data.frame(
          Message = paste("Grouping variable", input$group_var, "not found in results")
        )
      }
    }, error = function(e) {
      data.frame(
        Error = "Unable to calculate group statistics",
        Details = e$message
      )
    })
  })
  
  output$components_plot <- renderPlotly({
    req(values$universal_info, input$group_var)
    
    tryCatch({
      # Get components data
      components <- values$universal_info$information_components
      
      # Limit to reasonable number of samples for visualization
      if (nrow(components) > 50) {
        # Sample 50 representatives, stratified by group if possible
        if (input$group_var %in% colnames(components)) {
          components <- components %>%
            group_by(!!sym(input$group_var)) %>%
            slice_sample(n = ceiling(50 / length(unique(components[[input$group_var]])))) %>%
            ungroup()
        } else {
          components <- slice_sample(components, n = 50)
        }
      }
      
      # Create color mapping for groups
      if (input$group_var %in% colnames(components)) {
        group_colors <- rainbow(length(unique(components[[input$group_var]])))
        names(group_colors) <- unique(components[[input$group_var]])
        components$color <- group_colors[components[[input$group_var]]]
      } else {
        components$color <- "#3498db"
      }
      
      # Create stacked bar plot
      p <- plot_ly() %>%
        add_trace(
          x = components$sample_id,
          y = components$R_proportion,
          name = "Richness (R)",
          type = "bar",
          marker = list(color = "#e74c3c"),
          hovertemplate = paste0("Sample: %{x}<br>",
                                "Richness: %{y:.3f}<br>",
                                "Group: ", components[[input$group_var]], "<extra></extra>")
        ) %>%
        add_trace(
          x = components$sample_id,
          y = components$E_proportion,
          name = "Evenness (E)", 
          type = "bar",
          marker = list(color = "#3498db"),
          hovertemplate = paste0("Sample: %{x}<br>",
                                "Evenness: %{y:.3f}<br>",
                                "Group: ", components[[input$group_var]], "<extra></extra>")
        )
      
      # Add P and S if available
      if ("P_proportion" %in% colnames(components)) {
        p <- p %>%
          add_trace(
            x = components$sample_id,
            y = components$P_proportion,
            name = "Phylogenetic (P)",
            type = "bar",
            marker = list(color = "#27ae60"),
            hovertemplate = paste0("Sample: %{x}<br>",
                                  "Phylogenetic: %{y:.3f}<br>",
                                  "Group: ", components[[input$group_var]], "<extra></extra>")
          )
      }
      
      if ("S_proportion" %in% colnames(components)) {
        p <- p %>%
          add_trace(
            x = components$sample_id,
            y = components$S_proportion,
            name = "Spatial (S)",
            type = "bar",
            marker = list(color = "#f39c12"),
            hovertemplate = paste0("Sample: %{x}<br>",
                                  "Spatial: %{y:.3f}<br>",
                                  "Group: ", components[[input$group_var]], "<extra></extra>")
          )
      }
      
      p %>%
        layout(
          barmode = "stack",
          title = paste("Information Components by", input$group_var),
          xaxis = list(title = "Sample ID", tickangle = -45),
          yaxis = list(title = "Proportion of Total Information"),
          legend = list(orientation = "h", y = -0.2)
        )
      
    }, error = function(e) {
      # Fallback to simple plot
      components <- values$universal_info$information_components[1:min(10, nrow(values$universal_info$information_components)), ]
      
      plot_ly() %>%
        add_trace(
          x = components$sample_id,
          y = components$R_proportion,
          name = "Richness",
          type = "bar",
          marker = list(color = "#e74c3c")
        ) %>%
        add_trace(
          x = components$sample_id,
          y = components$E_proportion,
          name = "Evenness", 
          type = "bar",
          marker = list(color = "#3498db")
        ) %>%
        layout(
          barmode = "stack",
          title = "Information Components (fallback view)",
          xaxis = list(title = "Sample"),
          yaxis = list(title = "Proportion")
        )
    })
  })
  
  output$trans_matrix <- renderDataTable({
    req(values$universal_info)
    
    datatable(
      values$universal_info$transformation_matrix,
      options = list(pageLength = 10)
    )
  })
  
  # Transform metrics
  observeEvent(input$transform, {
    req(values$universal_info)
    req(length(input$target_metrics) > 0)
    
    source_metrics <- setNames(input$source_value, input$source_metric)
    
    withProgress(message = "Transforming metrics...", {
      values$transform_results <- universal_diversity_transform(
        source_metrics = source_metrics,
        target_metrics = input$target_metrics,
        transformation_matrix = values$universal_info$transformation_matrix
      )
    })
  })
  
  output$transform_complete <- reactive({
    !is.null(values$transform_results)
  })
  outputOptions(output, "transform_complete", suspendWhenHidden = FALSE)
  
  output$predictions <- renderTable({
    req(values$transform_results)
    
    data.frame(
      Metric = names(values$transform_results$predictions),
      Value = round(unlist(values$transform_results$predictions), 3)
    )
  })
  
  output$prediction_plot <- renderPlotly({
    req(values$transform_results)
    
    pred_data <- data.frame(
      metric = names(values$transform_results$predictions),
      value = unlist(values$transform_results$predictions)
    )
    
    plot_ly(
      x = pred_data$metric,
      y = pred_data$value,
      type = "bar",
      marker = list(color = "steelblue")
    ) %>%
      layout(
        title = "Predicted Values",
        xaxis = list(title = "Metric"),
        yaxis = list(title = "Value")
      )
  })
  
  # Information space plot
  output$info_space <- renderPlotly({
    req(values$universal_info)
    
    components <- values$universal_info$information_components
    
    plot_ly(
      x = components[[input$x_comp]],
      y = components[[input$y_comp]], 
      text = components$sample_id,
      type = "scatter",
      mode = "markers",
      marker = list(size = 10)
    ) %>%
      layout(
        title = "Information Component Space",
        xaxis = list(title = input$x_comp),
        yaxis = list(title = input$y_comp)
      )
  })
  
  # Component Explorer functionality is now handled by the enhanced module
}

shinyApp(ui = ui, server = server)