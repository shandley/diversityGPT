# Dataset Browser Module for diversityGPT Shiny App

# UI function
dataset_browser_ui <- function(id) {
  ns <- NS(id)
  
  # Load dataset registry if not already loaded
  if (!exists("diversityGPT_datasets")) {
    registry_file <- system.file("R", "dataset_registry.R", package = "diversityGPT")
    if (registry_file == "" || !file.exists(registry_file)) {
      registry_file <- "../../R/dataset_registry.R"
    }
    if (file.exists(registry_file)) {
      source(registry_file)
    } else {
      # Create minimal registry if file not found
      diversityGPT_datasets <- list(
        builtin = list(),
        precomputed = list(),
        external = list()
      )
    }
  }
  
  tagList(
    # Custom CSS for dataset cards
    tags$head(
      tags$style(HTML("
        .dataset-grid {
          display: grid;
          grid-template-columns: repeat(auto-fill, minmax(350px, 1fr));
          gap: 20px;
          margin: 20px 0;
        }
        
        .dataset-card {
          background: white;
          border: 1px solid #ddd;
          border-radius: 8px;
          padding: 15px;
          cursor: pointer;
          transition: all 0.3s ease;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        
        .dataset-card:hover {
          transform: translateY(-2px);
          box-shadow: 0 4px 8px rgba(0,0,0,0.15);
          border-color: #3498db;
        }
        
        .dataset-card.selected {
          border-color: #3498db;
          border-width: 2px;
          background: #f0f8ff;
        }
        
        .dataset-card-header {
          display: flex;
          justify-content: space-between;
          align-items: start;
          margin-bottom: 10px;
        }
        
        .dataset-card h4 {
          margin: 0;
          color: #2c3e50;
          font-size: 16px;
        }
        
        .badge {
          padding: 3px 8px;
          border-radius: 4px;
          font-size: 11px;
          font-weight: bold;
          text-transform: uppercase;
        }
        
        .badge-16S {
          background: #27ae60;
          color: white;
        }
        
        .badge-shotgun {
          background: #e74c3c;
          color: white;
        }
        
        .dataset-stats {
          display: flex;
          gap: 15px;
          margin: 10px 0;
          font-size: 13px;
          color: #7f8c8d;
        }
        
        .dataset-stats span {
          display: flex;
          align-items: center;
          gap: 5px;
        }
        
        .dataset-tags {
          display: flex;
          flex-wrap: wrap;
          gap: 5px;
          margin-top: 10px;
        }
        
        .dataset-tag {
          background: #ecf0f1;
          color: #34495e;
          padding: 2px 8px;
          border-radius: 3px;
          font-size: 11px;
        }
        
        .dataset-description {
          font-size: 13px;
          color: #555;
          margin: 8px 0;
          line-height: 1.4;
        }
        
        .dataset-card-footer {
          border-top: 1px solid #eee;
          margin-top: 10px;
          padding-top: 8px;
          font-size: 11px;
          color: #95a5a6;
        }
      "))
    ),
    
    h3("📊 Dataset Browser", style = "margin-bottom: 20px;"),
    
    # Filter controls
    wellPanel(
      fluidRow(
        column(3,
          selectInput(ns("filter_type"), "Data Type:",
            choices = c("All Types" = "all", "16S Amplicon" = "16S", "Shotgun Metagenomics" = "shotgun"),
            selected = "all"
          )
        ),
        column(3,
          selectInput(ns("filter_source"), "Source:",
            choices = c("All Sources" = "all", "Built-in" = "builtin", 
                       "Precomputed" = "precomputed", "External" = "external"),
            selected = "all"
          )
        ),
        column(3,
          selectInput(ns("filter_environment"), "Environment:",
            choices = c("All" = "all", "Human Gut" = "human gut", 
                       "Soil" = "soil", "Environmental" = "environmental"),
            selected = "all"
          )
        ),
        column(3,
          textInput(ns("search_term"), "Search:", 
                   placeholder = "e.g., gut, soil, time series...")
        )
      )
    ),
    
    # Dataset count
    h4(textOutput(ns("dataset_count")), style = "color: #3498db; margin-bottom: 15px;"),
    
    # Dataset cards container
    div(
      id = ns("dataset_container"),
      class = "dataset-grid",
      uiOutput(ns("dataset_cards"))
    ),
    
    # Selected dataset details
    conditionalPanel(
      condition = paste0("output['", ns("has_selection"), "']"),
      wellPanel(
        style = "background: #f8f9fa; border: 2px solid #3498db;",
        h4("📋 Selected Dataset", style = "margin-top: 0;"),
        uiOutput(ns("selected_details")),
        br(),
        div(style = "text-align: center;",
          actionButton(ns("load_dataset"), "🚀 Load This Dataset", 
                      class = "btn-success btn-lg", 
                      icon = icon("download"),
                      style = "font-size: 18px; padding: 12px 30px;")
        )
      )
    )
  )
}

# Server function
dataset_browser_server <- function(id, parent_session = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Load dataset registry if not already loaded
    if (!exists("diversityGPT_datasets")) {
      registry_file <- system.file("R", "dataset_registry.R", package = "diversityGPT")
      if (registry_file == "" || !file.exists(registry_file)) {
        registry_file <- "../../R/dataset_registry.R"
      }
      if (file.exists(registry_file)) {
        source(registry_file)
      } else {
        # Create minimal registry if file not found
        diversityGPT_datasets <- list(
          builtin = list(),
          precomputed = list(),
          external = list()
        )
      }
    }
    
    # Reactive values
    values <- reactiveValues(
      selected_dataset = NULL,
      loaded_data = NULL
    )
    
    # Get filtered datasets
    filtered_datasets <- reactive({
      # Start with all datasets
      all_datasets <- list()
      
      # Check if registry exists
      if (!exists("diversityGPT_datasets") || length(diversityGPT_datasets) == 0) {
        return(all_datasets)
      }
      
      tryCatch({
        for (source in names(diversityGPT_datasets)) {
          if (input$filter_source != "all" && source != input$filter_source) next
          
          # Skip if source is empty
          if (is.null(diversityGPT_datasets[[source]]) || length(diversityGPT_datasets[[source]]) == 0) next
          
          for (dataset_id in names(diversityGPT_datasets[[source]])) {
            dataset <- diversityGPT_datasets[[source]][[dataset_id]]
            
            # Skip if dataset is null or not a list
            if (is.null(dataset) || !is.list(dataset)) next
            
            dataset$source_category <- source
            dataset$id <- dataset_id
            
            # Apply type filter
            if (input$filter_type != "all" && !is.null(dataset$type) && dataset$type != input$filter_type) next
            
            # Apply environment filter
            if (input$filter_environment != "all") {
              if (is.null(dataset$tags) || length(dataset$tags) == 0 || 
                  !any(grepl(input$filter_environment, tolower(dataset$tags)))) next
            }
            
            # Apply search filter
            if (!is.null(input$search_term) && nchar(input$search_term) > 0) {
              search_pattern <- tolower(input$search_term)
              found <- FALSE
              
              # Search in name, description, and tags
              if (!is.null(dataset$name) && grepl(search_pattern, tolower(dataset$name))) found <- TRUE
              if (!is.null(dataset$description) && grepl(search_pattern, tolower(dataset$description))) found <- TRUE
              if (!is.null(dataset$tags) && any(grepl(search_pattern, tolower(dataset$tags)))) found <- TRUE
              
              if (!found) next
            }
            
            all_datasets[[length(all_datasets) + 1]] <- dataset
          }
        }
      }, error = function(e) {
        showNotification(paste("Error filtering datasets:", e$message), type = "error")
      })
      
      all_datasets
    })
    
    # Dataset count
    output$dataset_count <- renderText({
      n <- length(filtered_datasets())
      paste(n, ifelse(n == 1, "dataset found", "datasets found"))
    })
    
    # Render dataset cards
    output$dataset_cards <- renderUI({
      datasets <- filtered_datasets()
      
      if (length(datasets) == 0) {
        return(div(
          class = "text-center",
          style = "padding: 40px; color: #7f8c8d;",
          icon("search", class = "fa-3x"),
          h4("No datasets found"),
          p("Try adjusting your filters or search terms")
        ))
      }
      
      cards <- lapply(seq_along(datasets), function(i) {
        dataset <- datasets[[i]]
        
        # Ensure required fields exist
        if (is.null(dataset$name)) dataset$name <- "Unnamed Dataset"
        if (is.null(dataset$type)) dataset$type <- "16S"
        if (is.null(dataset$description)) dataset$description <- "No description available"
        if (is.null(dataset$samples)) dataset$samples <- NA
        if (is.null(dataset$taxa)) dataset$taxa <- NA
        if (is.null(dataset$tags)) dataset$tags <- character(0)
        
        # Create dataset card
        div(
          class = paste("dataset-card", 
                       ifelse(!is.null(values$selected_dataset) && 
                             !is.null(values$selected_dataset$id) &&
                             !is.null(dataset$id) &&
                             values$selected_dataset$id == dataset$id, 
                             "selected", "")),
          `data-dataset-index` = i,
          onclick = paste0("Shiny.setInputValue('", session$ns("card_clicked"), "', ", i, ", {priority: 'event'})"),
          
          # Header
          div(class = "dataset-card-header",
            h4(dataset$name),
            span(class = paste0("badge badge-", dataset$type), dataset$type)
          ),
          
          # Description
          div(class = "dataset-description",
            dataset$description
          ),
          
          # Stats
          div(class = "dataset-stats",
            if (!is.na(dataset$samples)) {
              span(icon("vial"), dataset$samples, "samples")
            },
            if (!is.na(dataset$taxa)) {
              span(icon("bacteria"), format(dataset$taxa, big.mark = ","), "taxa")
            },
            if (isTRUE(dataset$has_tree)) {
              span(icon("tree"), "Phylogeny")
            }
          ),
          
          # Tags
          if (length(dataset$tags) > 0) {
            div(class = "dataset-tags",
              lapply(dataset$tags[1:min(4, length(dataset$tags))], function(tag) {
                span(class = "dataset-tag", tag)
              }),
              if (length(dataset$tags) > 4) {
                span(class = "dataset-tag", paste0("+", length(dataset$tags) - 4, " more"))
              }
            )
          },
          
          # Footer with citation and load button
          if (!is.null(dataset$citation)) {
            div(class = "dataset-card-footer",
              dataset$citation
            )
          },
          
          # Load button
          div(style = "margin-top: 10px; text-align: center;",
            actionButton(
              session$ns(paste0("load_", i)),
              "📥 Load Dataset",
              class = "btn-primary btn-sm",
              style = "width: 100%;",
              onclick = paste0("event.stopPropagation(); Shiny.setInputValue('", session$ns("load_clicked"), "', ", i, ", {priority: 'event'})")
            )
          )
        )
      })
      
      tagList(cards)
    })
    
    # Handle card clicks (for selection only)
    observeEvent(input$card_clicked, {
      datasets <- filtered_datasets()
      if (input$card_clicked > 0 && input$card_clicked <= length(datasets)) {
        values$selected_dataset <- datasets[[input$card_clicked]]
      }
    })
    
    # Handle direct load from cards
    observeEvent(input$load_clicked, {
      datasets <- filtered_datasets()
      if (input$load_clicked > 0 && input$load_clicked <= length(datasets)) {
        values$selected_dataset <- datasets[[input$load_clicked]]
        # Automatically trigger load
        load_selected_dataset()
      }
    })
    
    # Centralized load function
    load_selected_dataset <- function() {
      req(values$selected_dataset)
      
      showModal(modalDialog(
        title = "Loading Dataset",
        div(
          style = "text-align: center; padding: 20px;",
          tags$div(
            class = "spinner-border text-primary",
            role = "status",
            style = "width: 3rem; height: 3rem;"
          ),
          br(), br(),
          h4("Loading dataset, please wait..."),
          p(values$selected_dataset$name)
        ),
        footer = NULL
      ))
      
      tryCatch({
        # Load the dataset
        dataset_id <- values$selected_dataset$id
        source_cat <- values$selected_dataset$source_category
        
        if (source_cat == "builtin") {
          # Use the simple loader for built-in datasets
          data_name <- values$selected_dataset$data_name
          data(list = data_name, package = "phyloseq", envir = environment())
          physeq <- get(data_name, envir = environment())
          values$loaded_data <- physeq
        } else if (source_cat == "precomputed") {
          # Load precomputed data
          tryCatch({
            # Source the functions if not already available
            if (!exists("load_precomputed")) {
              source("../../R/dataset_loaders.R")
            }
            
            # Load the precomputed results
            result <- load_precomputed(values$selected_dataset, return_physeq = TRUE)
            
            if (inherits(result, "phyloseq")) {
              values$loaded_data <- result
            } else {
              # If we got universal_info, try to get phyloseq from the file
              file_path <- system.file("data", values$selected_dataset$file, package = "diversityGPT")
              if (file_path == "") {
                file_path <- file.path("inst/data", values$selected_dataset$file)
              }
              
              if (file.exists(file_path)) {
                full_data <- readRDS(file_path)
                if (!is.null(full_data$phyloseq)) {
                  values$loaded_data <- full_data$phyloseq
                } else {
                  showNotification("Precomputed file doesn't contain phyloseq object", 
                                 type = "warning", duration = 5)
                  removeModal()
                  return()
                }
              }
            }
          }, error = function(e) {
            showNotification(paste("Error loading precomputed data:", e$message), 
                           type = "error", duration = NULL)
            removeModal()
            return()
          })
        } else {
          # External datasets
          showNotification("External dataset loading coming soon!", 
                          type = "warning", duration = 5)
          removeModal()
          return()
        }
        
        removeModal()
        showNotification(
          paste("✅ Successfully loaded:", values$selected_dataset$name),
          type = "message",
          duration = 3
        )
        
      }, error = function(e) {
        removeModal()
        showNotification(
          paste("❌ Error loading dataset:", e$message),
          type = "error",
          duration = 8
        )
      })
    }
    
    # Has selection output
    output$has_selection <- reactive({
      !is.null(values$selected_dataset)
    })
    outputOptions(output, "has_selection", suspendWhenHidden = FALSE)
    
    # Selected dataset details
    output$selected_details <- renderUI({
      req(values$selected_dataset)
      dataset <- values$selected_dataset
      
      tagList(
        h5(dataset$name, style = "color: #2c3e50; margin-bottom: 10px;"),
        
        # Key info
        tags$table(
          class = "table table-sm",
          tags$tr(
            tags$td(tags$strong("Type:")),
            tags$td(dataset$type)
          ),
          tags$tr(
            tags$td(tags$strong("Source:")),
            tags$td(dataset$source_category)
          ),
          if (!is.na(dataset$samples)) {
            tags$tr(
              tags$td(tags$strong("Samples:")),
              tags$td(format(dataset$samples, big.mark = ","))
            )
          },
          if (!is.na(dataset$taxa)) {
            tags$tr(
              tags$td(tags$strong("Taxa:")),
              tags$td(format(dataset$taxa, big.mark = ","))
            )
          },
          if (!is.null(dataset$has_tree)) {
            tags$tr(
              tags$td(tags$strong("Phylogenetic tree:")),
              tags$td(ifelse(dataset$has_tree, "Yes", "No"))
            )
          },
          if (!is.null(dataset$metadata_vars)) {
            tags$tr(
              tags$td(tags$strong("Metadata:")),
              tags$td(paste(dataset$metadata_vars, collapse = ", "))
            )
          }
        ),
        
        # Full description
        if (!is.null(dataset$description)) {
          div(
            style = "background: white; padding: 10px; border-radius: 4px; margin-top: 10px;",
            p(dataset$description, style = "margin: 0;")
          )
        },
        
        # Citation
        if (!is.null(dataset$citation)) {
          div(
            style = "margin-top: 10px; font-size: 12px; color: #7f8c8d;",
            icon("quote-left"),
            " ",
            dataset$citation
          )
        }
      )
    })
    
    # Load dataset (from main button)
    observeEvent(input$load_dataset, {
      load_selected_dataset()
    })
    
    # Return loaded data
    return(reactive({ values$loaded_data }))
  })
}

# Helper function to create dataset browser tab
create_dataset_browser_tab <- function() {
  tabPanel(
    "Dataset Browser",
    icon = icon("database"),
    dataset_browser_ui("dataset_browser")
  )
}