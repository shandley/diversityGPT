# UI for diversityGPT Interactive Explorer

# Ensure shinydashboard is loaded
if (!requireNamespace("shinydashboard", quietly = TRUE)) {
  stop("Package 'shinydashboard' is required but not installed.")
}
library(shinydashboard)

dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = tags$span(
      tags$img(src = "logo.png", height = "30px", style = "margin-right: 10px;"),
      "diversityGPT Explorer"
    ),
    
    tags$li(
      class = "dropdown",
      tags$a(
        href = "https://github.com/shandley/diversityGPT",
        icon("github"),
        title = "View on GitHub",
        target = "_blank",
        style = "padding: 15px;"
      )
    ),
    
    tags$li(
      class = "dropdown",
      tags$a(
        href = "#",
        icon("question-circle"),
        title = "Help",
        onclick = "Shiny.setInputValue('show_help', Math.random())"
      )
    )
  ),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      
      menuItem("📊 Data Upload", tabName = "data", icon = icon("upload")),
      menuItem("🔬 Universal Analysis", tabName = "universal", icon = icon("infinity")),
      menuItem("🔄 Transform Metrics", tabName = "transform", icon = icon("exchange-alt")),
      menuItem("📈 Visualizations", tabName = "visualize", icon = icon("chart-network")),
      menuItem("🧠 AI Interpretation", tabName = "interpret", icon = icon("brain")),
      menuItem("💾 Export Results", tabName = "export", icon = icon("download"))
    ),
    
    hr(),
    
    # Dataset selection
    div(style = "padding: 0 15px;",
      h4("Quick Start", icon("rocket")),
      
      selectInput(
        "dataset_choice",
        "Example Dataset:",
        choices = c(
          "GlobalPatterns" = "global",
          "Enterotype" = "entero", 
          "Soil Microbiome" = "soil",
          "Upload Your Own" = "custom"
        ),
        selected = "global"
      ),
      
      conditionalPanel(
        condition = "input.dataset_choice != 'custom'",
        actionButton(
          "load_example",
          "Load Dataset",
          class = "btn-primary btn-block",
          icon = icon("database")
        )
      ),
      
      br(),
      
      # API status
      h5("API Status", icon("plug")),
      uiOutput("api_status")
    )
  ),
  
  dashboardBody(
    # Custom CSS and JavaScript
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      
      tags$style(HTML("
        /* Custom styles for diversityGPT */
        .content-wrapper, .right-side {
          background-color: #f8f9fa;
        }
        
        .box-header {
          border-bottom: 2px solid #e9ecef;
        }
        
        .info-box {
          cursor: pointer;
          transition: all 0.3s;
        }
        
        .info-box:hover {
          transform: translateY(-2px);
          box-shadow: 0 4px 12px rgba(0,0,0,0.15);
        }
        
        .nav-tabs-custom > .tab-content {
          padding: 20px;
          background: white;
        }
        
        .skin-blue .main-header .navbar {
          background-color: #2c3e50;
        }
        
        .skin-blue .main-header .logo {
          background-color: #34495e;
        }
        
        .btn-primary {
          background-color: #3498db;
          border-color: #2980b9;
        }
        
        .btn-success {
          background-color: #27ae60;
          border-color: #229954;
        }
        
        .btn-warning {
          background-color: #f39c12;
          border-color: #d68910;
        }
        
        .btn-info {
          background-color: #8e44ad;
          border-color: #7d3c98;
        }
        
        /* Plotly toolbar positioning */
        .js-plotly-plot .plotly .modebar {
          top: 5px !important;
          right: 5px !important;
        }
        
        /* Loading animation */
        .progress-bar {
          background-color: #3498db;
        }
        
        /* Value box icons */
        .small-box .icon {
          opacity: 0.3;
        }
        
        /* Tab panel styling */
        .nav-tabs > li.active > a {
          border-top: 3px solid #3498db;
        }
        
        /* Notification styling */
        .shiny-notification {
          border-radius: 4px;
          border-left: 4px solid;
        }
        
        .shiny-notification-error {
          border-left-color: #e74c3c;
        }
        
        .shiny-notification-warning {
          border-left-color: #f39c12;
        }
        
        .shiny-notification-message {
          border-left-color: #3498db;
        }
      "))
    ),
    
    # Add modal for help
    tags$script(HTML("
      Shiny.addCustomMessageHandler('show_help_modal', function(message) {
        Shiny.setInputValue('help_modal_trigger', Math.random());
      });
    ")),
    
    # Tab content
    tabItems(
      # Data Upload Tab
      tabItem(
        tabName = "data",
        
        h2("Data Management", icon("database")),
        br(),
        
        fluidRow(
          # Main data box
          box(
            title = span(icon("upload"), "Data Upload & Overview"),
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            
            conditionalPanel(
              condition = "input.dataset_choice == 'custom'",
              
              div(
                class = "upload-area",
                style = "border: 2px dashed #bdc3c7; border-radius: 5px; padding: 40px; text-align: center; margin-bottom: 20px;",
                
                fileInput(
                  "phyloseq_file",
                  NULL,
                  accept = c(".rds", ".RDS"),
                  buttonLabel = "Browse...",
                  placeholder = "No file selected"
                ),
                
                p("Drag and drop a phyloseq object (.RDS file) or click to browse",
                  style = "color: #7f8c8d; margin-top: 10px;")
              ),
              
              hr()
            ),
            
            # Dataset summary
            conditionalPanel(
              condition = "output.data_loaded",
              
              h4("Dataset Summary", icon("chart-bar")),
              verbatimTextOutput("data_summary"),
              
              br(),
              
              fluidRow(
                valueBoxOutput("n_samples"),
                valueBoxOutput("n_taxa"),
                valueBoxOutput("n_groups")
              )
            ),
            
            # Placeholder when no data
            conditionalPanel(
              condition = "!output.data_loaded",
              div(
                style = "text-align: center; padding: 60px;",
                icon("database", "fa-4x", style = "color: #bdc3c7;"),
                h4("No data loaded", style = "color: #7f8c8d; margin-top: 20px;"),
                p("Select an example dataset or upload your own to begin", 
                  style = "color: #95a5a6;")
              )
            )
          )
        ),
        
        # Metadata and taxonomy boxes
        conditionalPanel(
          condition = "output.data_loaded",
          
          fluidRow(
            box(
              title = span(icon("table"), "Sample Metadata"),
              status = "info",
              width = 6,
              collapsible = TRUE,
              collapsed = FALSE,
              
              DT::dataTableOutput("metadata_table")
            ),
            
            box(
              title = span(icon("bacteria"), "Taxonomic Overview"),
              status = "info",
              width = 6,
              collapsible = TRUE,
              collapsed = FALSE,
              
              plotlyOutput("taxa_plot", height = "400px")
            )
          )
        )
      ),
      
      # Universal Analysis Tab
      tabItem(
        tabName = "universal",
        
        h2("Universal Information Framework", icon("infinity")),
        p("Extract mathematical relationships between ALL diversity metrics"),
        br(),
        
        fluidRow(
          box(
            title = span(icon("cogs"), "Analysis Settings"),
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            fluidRow(
              column(
                width = 4,
                selectInput(
                  "group_var",
                  label = span(icon("users"), "Group samples by:"),
                  choices = NULL,
                  selected = NULL
                ),
                
                br(),
                
                div(
                  style = "padding: 15px; background: #ecf0f1; border-radius: 5px;",
                  checkboxInput(
                    "include_phylo",
                    span(icon("tree"), "Include phylogenetic components"),
                    value = FALSE
                  ),
                  
                  conditionalPanel(
                    condition = "input.include_phylo",
                    p("Requires phylogenetic tree in dataset", 
                      style = "color: #7f8c8d; margin: 5px 0 0 25px; font-size: 12px;")
                  )
                )
              ),
              
              column(
                width = 4,
                h5("Advanced Options", icon("sliders-h")),
                
                sliderInput(
                  "n_components",
                  "Number of components:",
                  min = 2,
                  max = 4,
                  value = 4,
                  step = 1
                ),
                
                selectInput(
                  "distance_metric",
                  "Distance metric:",
                  choices = c(
                    "Euclidean" = "euclidean",
                    "Manhattan" = "manhattan",
                    "Correlation" = "correlation"
                  ),
                  selected = "euclidean"
                )
              ),
              
              column(
                width = 4,
                div(
                  style = "text-align: center; padding-top: 20px;",
                  
                  actionButton(
                    "run_universal",
                    span(icon("play"), "Extract Universal Information"),
                    class = "btn-success btn-lg",
                    style = "width: 100%; height: 60px; font-size: 16px;"
                  ),
                  
                  br(), br(),
                  
                  p("This will decompose all diversity metrics into fundamental information components",
                    style = "color: #7f8c8d; font-size: 12px;")
                )
              )
            )
          )
        ),
        
        # Results section
        conditionalPanel(
          condition = "output.universal_complete",
          
          br(),
          
          fluidRow(
            infoBoxOutput("overall_quality"),
            infoBoxOutput("mean_r2"),
            infoBoxOutput("n_components")
          ),
          
          br(),
          
          fluidRow(
            box(
              title = "Analysis Results",
              status = "success",
              width = 12,
              
              tabsetPanel(
                type = "pills",
                
                tabPanel(
                  span(icon("layer-group"), "Information Components"),
                  br(),
                  plotlyOutput("components_plot", height = "500px"),
                  br(),
                  downloadButton("download_components", "Download Data", class = "btn-sm")
                ),
                
                tabPanel(
                  span(icon("chart-box"), "Component Distribution"),
                  br(),
                  plotlyOutput("component_dist", height = "500px")
                ),
                
                tabPanel(
                  span(icon("table"), "Transformation Matrix"),
                  br(),
                  p("This matrix shows how well each metric can be predicted from the universal components"),
                  DT::dataTableOutput("transformation_matrix")
                ),
                
                tabPanel(
                  span(icon("award"), "Quality Assessment"),
                  br(),
                  plotlyOutput("quality_plot", height = "500px"),
                  br(),
                  verbatimTextOutput("quality_summary")
                )
              )
            )
          )
        )
      ),
      
      # Transform Metrics Tab
      tabItem(
        tabName = "transform",
        
        h2("Metric Transformation", icon("exchange-alt")),
        p("Transform between ANY diversity metrics using the universal framework"),
        br(),
        
        fluidRow(
          box(
            title = "Transformation Setup",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            fluidRow(
              # Source metrics column
              column(
                width = 5,
                div(
                  style = "background: #e8f4f8; padding: 20px; border-radius: 5px;",
                  
                  h4(icon("upload"), "Source Metrics"),
                  p("Enter known metric values", style = "color: #7f8c8d;"),
                  
                  br(),
                  
                  fluidRow(
                    column(
                      width = 6,
                      selectInput(
                        "source_metric",
                        "Metric:",
                        choices = c(
                          "Shannon" = "shannon",
                          "Simpson" = "simpson",
                          "Observed" = "observed",
                          "Chao1" = "chao1",
                          "Pielou" = "pielou_evenness"
                        ),
                        selected = "shannon"
                      )
                    ),
                    
                    column(
                      width = 6,
                      numericInput(
                        "source_value",
                        "Value:",
                        value = 2.5,
                        min = 0,
                        step = 0.1
                      )
                    )
                  ),
                  
                  actionButton(
                    "add_source",
                    "Add to List",
                    icon = icon("plus"),
                    class = "btn-info btn-block"
                  ),
                  
                  br(),
                  
                  wellPanel(
                    style = "background: white;",
                    h5("Current Source Metrics:"),
                    verbatimTextOutput("source_list"),
                    
                    conditionalPanel(
                      condition = "output.has_sources",
                      actionButton(
                        "clear_sources",
                        "Clear All",
                        icon = icon("trash"),
                        class = "btn-sm btn-danger"
                      )
                    )
                  )
                )
              ),
              
              # Arrow column
              column(
                width = 2,
                div(
                  style = "text-align: center; padding-top: 100px;",
                  icon("arrow-right", "fa-3x", style = "color: #3498db;"),
                  br(), br(),
                  actionButton(
                    "transform",
                    "Transform",
                    class = "btn-warning btn-lg",
                    icon = icon("magic"),
                    style = "width: 100%;"
                  )
                )
              ),
              
              # Target metrics column
              column(
                width = 5,
                div(
                  style = "background: #fef5e7; padding: 20px; border-radius: 5px;",
                  
                  h4(icon("download"), "Target Metrics"),
                  p("Select metrics to predict", style = "color: #7f8c8d;"),
                  
                  br(),
                  
                  checkboxGroupInput(
                    "target_metrics",
                    "Predict these metrics:",
                    choices = c(
                      "Shannon Index" = "shannon",
                      "Simpson Index" = "simpson",
                      "Observed Species" = "observed",
                      "Chao1 Estimator" = "chao1",
                      "Pielou Evenness" = "pielou_evenness",
                      "Faith's PD" = "faith_pd",
                      "Fisher's Alpha" = "fisher_alpha"
                    ),
                    selected = c("simpson", "chao1")
                  ),
                  
                  br(),
                  
                  div(
                    style = "padding: 10px; background: #fff3cd; border-radius: 5px;",
                    p(icon("info-circle"), "Tip: The more source metrics you provide, the more accurate the predictions!",
                      style = "margin: 0; font-size: 12px;")
                  )
                )
              )
            )
          )
        ),
        
        # Results section
        conditionalPanel(
          condition = "output.transform_complete",
          
          br(),
          
          fluidRow(
            box(
              title = span(icon("chart-bar"), "Transformation Results"),
              status = "success",
              width = 8,
              
              plotlyOutput("transform_results", height = "400px")
            ),
            
            box(
              title = span(icon("list"), "Predicted Values"),
              status = "info",
              width = 4,
              
              tableOutput("predicted_table"),
              
              br(),
              
              h5("Transformation Quality"),
              plotlyOutput("transform_quality", height = "200px"),
              
              br(),
              
              downloadButton(
                "download_predictions",
                "Download Results",
                class = "btn-primary btn-block"
              )
            )
          )
        )
      ),
      
      # Visualizations Tab
      tabItem(
        tabName = "visualize",
        
        h2("Interactive Visualizations", icon("chart-network")),
        br(),
        
        box(
          title = "Visualization Options",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          
          tabsetPanel(
            type = "tabs",
            
            # Network tab
            tabPanel(
              span(icon("project-diagram"), "Metric Network"),
              br(),
              
              fluidRow(
                column(
                  width = 3,
                  
                  wellPanel(
                    h5("Network Settings"),
                    
                    sliderInput(
                      "min_r2",
                      "Min R² threshold:",
                      min = 0,
                      max = 1,
                      value = 0.5,
                      step = 0.1,
                      ticks = TRUE
                    ),
                    
                    radioButtons(
                      "node_size",
                      "Node size by:",
                      choices = c(
                        "Importance" = "importance",
                        "Connectivity" = "connectivity", 
                        "Uniform" = "uniform"
                      ),
                      selected = "importance"
                    ),
                    
                    selectInput(
                      "node_color",
                      "Node color by:",
                      choices = c(
                        "Component" = "component",
                        "Metric Type" = "type",
                        "Quality" = "quality"
                      ),
                      selected = "component"
                    ),
                    
                    br(),
                    
                    actionButton(
                      "update_network",
                      "Update Network",
                      icon = icon("sync"),
                      class = "btn-primary btn-block"
                    )
                  )
                ),
                
                column(
                  width = 9,
                  plotlyOutput("network_plot", height = "600px")
                )
              )
            ),
            
            # Information space tab
            tabPanel(
              span(icon("scatter-plot"), "Information Space"),
              br(),
              
              fluidRow(
                column(
                  width = 3,
                  
                  wellPanel(
                    h5("Axis Selection"),
                    
                    selectInput(
                      "x_component",
                      "X-axis:",
                      choices = c(
                        "Richness (R)" = "R_component",
                        "Evenness (E)" = "E_component",
                        "Phylogenetic (P)" = "P_component",
                        "Spatial (S)" = "S_component"
                      ),
                      selected = "R_component"
                    ),
                    
                    selectInput(
                      "y_component",
                      "Y-axis:",
                      choices = c(
                        "Richness (R)" = "R_component",
                        "Evenness (E)" = "E_component",
                        "Phylogenetic (P)" = "P_component",
                        "Spatial (S)" = "S_component"
                      ),
                      selected = "E_component"
                    ),
                    
                    selectInput(
                      "color_by",
                      "Color points by:",
                      choices = NULL
                    ),
                    
                    br(),
                    
                    checkboxInput(
                      "show_ellipses",
                      "Show group ellipses",
                      value = TRUE
                    ),
                    
                    checkboxInput(
                      "show_labels",
                      "Show sample labels",
                      value = FALSE
                    )
                  )
                ),
                
                column(
                  width = 9,
                  plotlyOutput("info_space_plot", height = "600px")
                )
              )
            ),
            
            # Dashboard tab
            tabPanel(
              span(icon("th"), "Component Dashboard"),
              br(),
              
              p("Comprehensive view of all information components and their relationships"),
              
              plotlyOutput("dashboard_plot", height = "700px"),
              
              br(),
              
              fluidRow(
                column(
                  width = 6,
                  downloadButton("download_dashboard", "Download as PNG", class = "btn-primary")
                ),
                column(
                  width = 6,
                  downloadButton("download_dashboard_data", "Download Data", class = "btn-info")
                )
              )
            )
          )
        )
      ),
      
      # AI Interpretation Tab
      tabItem(
        tabName = "interpret",
        
        h2("AI-Powered Interpretation", icon("brain")),
        p("Get ecological insights and hypothesis generation from your diversity patterns"),
        br(),
        
        fluidRow(
          box(
            title = "Interpretation Settings",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            fluidRow(
              column(
                width = 6,
                
                h4(icon("microscope"), "Study Context"),
                
                selectInput(
                  "environment",
                  "Environment type:",
                  choices = c(
                    "Human Gut" = "human_gut",
                    "Soil" = "soil",
                    "Ocean/Marine" = "ocean",
                    "Freshwater" = "freshwater",
                    "Plant-associated" = "plant",
                    "Built Environment" = "built",
                    "Other" = "other"
                  ),
                  selected = "human_gut"
                ),
                
                textInput(
                  "condition",
                  "Study condition/treatment:",
                  placeholder = "e.g., antibiotic treatment, pH gradient, seasonal variation"
                ),
                
                selectInput(
                  "organism_type",
                  "Target organisms:",
                  choices = c(
                    "Bacteria" = "bacteria",
                    "Fungi" = "fungi",
                    "Archaea" = "archaea",
                    "Viruses" = "viruses",
                    "Mixed Community" = "mixed"
                  ),
                  selected = "bacteria"
                ),
                
                textAreaInput(
                  "additional_context",
                  "Additional context (optional):",
                  placeholder = "Any other relevant information about your study...",
                  rows = 3
                )
              ),
              
              column(
                width = 6,
                
                h4(icon("tasks"), "Analysis Options"),
                
                checkboxGroupInput(
                  "interpretation_options",
                  "Include in interpretation:",
                  choices = c(
                    "Ecological mechanisms" = "mechanisms",
                    "Testable hypotheses" = "hypotheses",
                    "Literature connections" = "literature",
                    "Follow-up suggestions" = "followup",
                    "Statistical recommendations" = "statistics",
                    "Visualization suggestions" = "viz_suggestions"
                  ),
                  selected = c("mechanisms", "hypotheses", "followup")
                ),
                
                br(),
                
                radioButtons(
                  "interpretation_depth",
                  "Analysis depth:",
                  choices = c(
                    "Quick overview" = "quick",
                    "Standard analysis" = "standard",
                    "Comprehensive report" = "comprehensive"
                  ),
                  selected = "standard"
                ),
                
                br(),
                
                actionButton(
                  "interpret",
                  span(icon("brain"), "Generate Interpretation"),
                  class = "btn-info btn-lg btn-block",
                  style = "height: 50px;"
                )
              )
            )
          )
        ),
        
        # Results section
        conditionalPanel(
          condition = "output.interpretation_complete",
          
          br(),
          
          fluidRow(
            box(
              title = span(icon("lightbulb"), "AI Interpretation"),
              status = "success",
              width = 12,
              
              wellPanel(
                style = "background: #f8f9fa;",
                uiOutput("interpretation_text")
              ),
              
              br(),
              
              fluidRow(
                column(
                  width = 6,
                  
                  div(
                    class = "info-box bg-yellow",
                    span(class = "info-box-icon", icon("search")),
                    div(
                      class = "info-box-content",
                      span(class = "info-box-text", "Key Patterns"),
                      uiOutput("patterns_list")
                    )
                  )
                ),
                
                column(
                  width = 6,
                  
                  div(
                    class = "info-box bg-green",
                    span(class = "info-box-icon", icon("flask")),
                    div(
                      class = "info-box-content",
                      span(class = "info-box-text", "Suggested Follow-up"),
                      uiOutput("followup_list")
                    )
                  )
                )
              ),
              
              br(),
              
              downloadButton(
                "download_interpretation",
                "Download Full Report",
                class = "btn-success",
                icon = icon("file-pdf")
              )
            )
          )
        )
      ),
      
      # Export Tab
      tabItem(
        tabName = "export",
        
        h2("Export Results", icon("download")),
        br(),
        
        fluidRow(
          box(
            title = "Export Options",
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            
            h4("Select what to include in your export:"),
            
            br(),
            
            checkboxGroupInput(
              "export_options",
              NULL,
              choices = c(
                "Universal information components" = "universal",
                "Transformation matrices" = "matrices",
                "All visualizations" = "plots",
                "AI interpretations" = "interpretations",
                "Raw data" = "raw_data",
                "Analysis summary" = "summary",
                "R code to reproduce" = "code"
              ),
              selected = c("universal", "matrices", "plots", "summary"),
              width = "100%"
            ),
            
            br(),
            
            h4("Export format:"),
            
            radioButtons(
              "export_format",
              NULL,
              choices = c(
                "R Data Bundle (.RDS)" = "rds",
                "CSV files (zipped)" = "csv",
                "HTML Report" = "html",
                "PDF Report" = "pdf",
                "Excel Workbook" = "xlsx"
              ),
              selected = "html",
              inline = TRUE
            ),
            
            br(),
            
            conditionalPanel(
              condition = "input.export_format == 'html' || input.export_format == 'pdf'",
              
              h5("Report Options:"),
              
              checkboxInput(
                "include_header",
                "Include study information header",
                value = TRUE
              ),
              
              checkboxInput(
                "include_methods",
                "Include methods section",
                value = TRUE
              ),
              
              checkboxInput(
                "include_references",
                "Include references",
                value = TRUE
              )
            ),
            
            br(),
            
            div(
              style = "text-align: center;",
              
              downloadButton(
                "download_results",
                "Download Results",
                class = "btn-success btn-lg",
                icon = icon("download"),
                style = "width: 50%;"
              )
            )
          ),
          
          box(
            title = "Analysis Summary",
            status = "info",
            width = 4,
            
            h5("Current Analysis Status:"),
            
            verbatimTextOutput("analysis_summary"),
            
            br(),
            
            h5("Quick Actions:"),
            
            actionButton(
              "save_session",
              "Save Session",
              icon = icon("save"),
              class = "btn-primary btn-block"
            ),
            
            br(),
            
            actionButton(
              "load_session",
              "Load Previous Session",
              icon = icon("folder-open"),
              class = "btn-info btn-block"
            ),
            
            br(),
            
            actionButton(
              "reset_analysis",
              "Reset All",
              icon = icon("redo"),
              class = "btn-warning btn-block",
              onclick = "if(confirm('This will clear all results. Continue?')) Shiny.setInputValue('confirm_reset', Math.random());"
            )
          )
        )
      )
    ),
    
    # Help modal (conditional on shinyBS availability)
    if (requireNamespace("shinyBS", quietly = TRUE)) {
      shinyBS::bsModal(
        "help_modal",
        "diversityGPT Help",
        trigger = "help_modal_trigger",
        size = "large",
        
        h4("Quick Start Guide"),
        p("1. Load your data using the Data Upload tab or select an example dataset"),
        p("2. Run Universal Analysis to extract information components"),
        p("3. Use Transform Metrics to convert between any diversity metrics"),
        p("4. Explore visualizations to understand metric relationships"),
        p("5. Get AI-powered ecological interpretations"),
        p("6. Export your results in various formats"),
        
        hr(),
        
        h4("Key Concepts"),
        tags$ul(
          tags$li("R (Richness): Information about species count"),
          tags$li("E (Evenness): Information about distribution uniformity"),
          tags$li("P (Phylogenetic): Information about evolutionary diversity"),
          tags$li("S (Spatial): Information about geographic patterns")
        ),
        
        hr(),
        
        p("For more information, visit the ",
          tags$a("documentation", href = "https://github.com/shandley/diversityGPT", target = "_blank"))
      )
    }
  )
)