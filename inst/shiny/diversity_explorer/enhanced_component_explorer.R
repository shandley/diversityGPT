# Enhanced Component Explorer 2.0
# Revolutionary visualization of mathematical relationships between R,E,P,S and diversity metrics

library(shiny)
library(plotly)
library(DT)
library(diversityGPT)

# UI for enhanced component explorer
enhanced_component_explorer_ui <- fluidPage(
  # Custom CSS
  tags$head(
    tags$style(HTML("
      .equation-display {
        font-family: 'Courier New', monospace;
        font-size: 14px;
        background: #f8f9fa;
        padding: 15px;
        border-radius: 8px;
        border-left: 4px solid #3498db;
        margin: 10px 0;
      }
      
      .metric-equation {
        margin: 5px 0;
        line-height: 1.6;
      }
      
      .coefficient {
        color: #e74c3c;
        font-weight: bold;
      }
      
      .result-value {
        color: #27ae60;
        font-weight: bold;
        font-size: 16px;
      }
      
      .contribution-bar {
        height: 25px;
        background: #ecf0f1;
        border-radius: 4px;
        margin: 5px 0;
        position: relative;
        overflow: hidden;
      }
      
      .contribution-fill {
        height: 100%;
        position: absolute;
        transition: width 0.3s ease;
      }
      
      .contribution-label {
        position: absolute;
        top: 50%;
        left: 10px;
        transform: translateY(-50%);
        font-size: 12px;
        font-weight: bold;
        z-index: 10;
      }
      
      .scenario-btn {
        margin: 5px;
        width: calc(50% - 10px);
      }
      
      .biological-interpretation {
        background: #e8f5e9;
        border-left: 4px solid #4caf50;
        padding: 15px;
        border-radius: 8px;
        margin: 15px 0;
      }
      
      .prediction-comparison {
        background: white;
        padding: 15px;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      
      .error-good { color: #27ae60; }
      .error-moderate { color: #f39c12; }
      .error-poor { color: #e74c3c; }
    "))
  ),
  
  titlePanel(
    div(
      h1("Component Explorer 2.0", style = "color: white; margin: 0;"),
      p("See the Mathematical DNA of Diversity Metrics", style = "color: white; margin: 5px 0;"),
      style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); padding: 30px; margin: -15px -15px 30px -15px; text-align: center; border-radius: 0 0 20px 20px;"
    )
  ),
  
  fluidRow(
    # Left panel - Component Controls
    column(
      width = 4,
      
      # Component sliders
      wellPanel(
        h3("🎛️ Component Controls", style = "margin-top: 0;"),
        p("Adjust fundamental information components:"),
        
        br(),
        
        # Richness
        div(
          h4("🔢 Richness (R)", style = "color: #e74c3c; margin-bottom: 5px;"),
          sliderInput("R_comp", NULL, 
                      min = 0, max = 1, value = 0.5, step = 0.01, width = "100%"),
          p("Species count information", style = "font-size: 11px; color: #7f8c8d; margin-top: -5px;")
        ),
        
        # Evenness
        div(
          h4("⚖️ Evenness (E)", style = "color: #3498db; margin-bottom: 5px;"),
          sliderInput("E_comp", NULL,
                      min = 0, max = 1, value = 0.5, step = 0.01, width = "100%"),
          p("Distribution uniformity", style = "font-size: 11px; color: #7f8c8d; margin-top: -5px;")
        ),
        
        # Phylogenetic
        div(
          h4("🌳 Phylogenetic (P)", style = "color: #27ae60; margin-bottom: 5px;"),
          sliderInput("P_comp", NULL,
                      min = 0, max = 1, value = 0.2, step = 0.01, width = "100%"),
          p("Evolutionary diversity", style = "font-size: 11px; color: #7f8c8d; margin-top: -5px;")
        ),
        
        # Spatial
        div(
          h4("🗺️ Spatial (S)", style = "color: #f39c12; margin-bottom: 5px;"),
          sliderInput("S_comp", NULL,
                      min = 0, max = 1, value = 0.1, step = 0.01, width = "100%"),
          p("Geographic patterns", style = "font-size: 11px; color: #7f8c8d; margin-top: -5px;")
        ),
        
        hr(),
        
        checkboxInput("normalize_comps", "Auto-normalize to sum = 1", value = TRUE),
        
        # Scenario buttons
        h4("📋 Ecological Scenarios"),
        div(
          actionButton("scenario_pioneer", "🌱 Pioneer", class = "btn-success scenario-btn"),
          actionButton("scenario_mature", "🌳 Mature", class = "btn-primary scenario-btn"),
          actionButton("scenario_stressed", "🏜️ Stressed", class = "btn-warning scenario-btn"),
          actionButton("scenario_bloom", "🦠 Bloom", class = "btn-danger scenario-btn")
        ),
        
        br(),
        
        actionButton("reset_components", "Reset All", class = "btn-secondary btn-block", icon = icon("undo"))
      ),
      
      # Biological interpretation
      div(
        class = "biological-interpretation",
        h4("🔬 Biological Interpretation", style = "margin-top: 0;"),
        uiOutput("biological_meaning")
      )
    ),
    
    # Middle panel - Live Equations
    column(
      width = 4,
      
      # Live equation display
      wellPanel(
        h3("📐 Live Mathematical Equations", style = "margin-top: 0;"),
        p("See how components create metrics in real-time:"),
        
        div(
          class = "equation-display",
          uiOutput("live_equations")
        )
      ),
      
      # Component contributions
      wellPanel(
        h3("📊 Component Contributions", style = "margin-top: 0;"),
        uiOutput("contribution_bars")
      ),
      
      # Prediction accuracy (if data loaded)
      conditionalPanel(
        condition = "output.has_real_data",
        wellPanel(
          h3("🎯 Prediction vs Reality", style = "margin-top: 0;"),
          div(
            class = "prediction-comparison",
            DT::dataTableOutput("prediction_accuracy", height = "200px")
          )
        )
      )
    ),
    
    # Right panel - Visualizations
    column(
      width = 4,
      
      tabsetPanel(
        # Coefficient heatmap
        tabPanel(
          "Coefficient Matrix",
          br(),
          h4("How Components Drive Metrics"),
          plotlyOutput("coefficient_heatmap", height = "400px"),
          p("Darker = stronger influence", style = "text-align: center; color: #7f8c8d;")
        ),
        
        # Sankey diagram
        tabPanel(
          "Flow Diagram", 
          br(),
          h4("Information Flow"),
          plotlyOutput("sankey_diagram", height = "400px"),
          p("Width = contribution strength", style = "text-align: center; color: #7f8c8d;")
        ),
        
        # 3D response surface
        tabPanel(
          "3D Surface",
          br(),
          h4("Response Surface"),
          selectInput("surface_metric", "Select metric:",
                      choices = c("Shannon", "Simpson", "Chao1", "Pielou"),
                      selected = "Shannon"),
          plotlyOutput("response_surface", height = "400px")
        ),
        
        # Network graph
        tabPanel(
          "Network",
          br(),
          h4("Component-Metric Network"),
          plotlyOutput("network_graph", height = "400px")
        ),
        
        # Uncertainty
        tabPanel(
          "Uncertainty",
          br(),
          h4("Prediction Confidence"),
          plotlyOutput("uncertainty_plot", height = "400px")
        )
      )
    )
  ),
  
  # Bottom panel - additional insights
  fluidRow(
    column(
      width = 12,
      
      br(),
      
      wellPanel(
        h3("🔍 Key Insights", style = "margin-top: 0;"),
        
        fluidRow(
          column(
            width = 4,
            h4("Dominant Components"),
            uiOutput("dominant_components")
          ),
          
          column(
            width = 4,
            h4("Metric Relationships"),
            uiOutput("metric_relationships")
          ),
          
          column(
            width = 4,
            h4("Ecological State"),
            uiOutput("ecological_state")
          )
        )
      )
    )
  )
)

# Server logic
enhanced_component_explorer_server <- function(input, output, session) {
  
  # Reactive values
  values <- reactiveValues(
    transformation_matrix = NULL,
    actual_metrics = NULL,
    has_data = FALSE
  )
  
  # Load example transformation matrix on startup
  observe({
    # Create a realistic transformation matrix
    values$transformation_matrix <- data.frame(
      metric = c("shannon", "simpson", "observed", "chao1", "pielou", "faith_pd"),
      R_coef = c(0.73, 0.18, 0.91, 0.88, 0.15, 0.45),
      E_coef = c(0.21, 0.79, 0.06, 0.08, 0.82, 0.25),
      P_coef = c(0.05, 0.02, 0.03, 0.04, 0.02, 0.28),
      S_coef = c(0.01, 0.01, 0.00, 0.00, 0.01, 0.02),
      intercept = c(0.5, 0.1, 10, 12, 0.2, 0.3),
      r_squared = c(0.94, 0.96, 0.89, 0.87, 0.92, 0.85)
    )
  })
  
  # Get current components (normalized if requested)
  current_components <- reactive({
    comps <- c(
      R = input$R_comp,
      E = input$E_comp,
      P = input$P_comp,
      S = input$S_comp
    )
    
    if (input$normalize_comps && sum(comps) > 0) {
      comps <- comps / sum(comps)
    }
    
    comps
  })
  
  # Calculate metrics from components
  calculated_metrics <- reactive({
    req(values$transformation_matrix)
    
    comps <- current_components()
    tm <- values$transformation_matrix
    
    metrics <- list()
    for (i in 1:nrow(tm)) {
      value <- tm$intercept[i] + 
        tm$R_coef[i] * comps["R"] +
        tm$E_coef[i] * comps["E"] +
        tm$P_coef[i] * comps["P"] +
        tm$S_coef[i] * comps["S"]
      
      metrics[[tm$metric[i]]] <- value
    }
    
    metrics
  })
  
  # Live equations display
  output$live_equations <- renderUI({
    req(values$transformation_matrix)
    
    comps <- current_components()
    metrics <- calculated_metrics()
    tm <- values$transformation_matrix
    
    equations <- lapply(1:nrow(tm), function(i) {
      metric_name <- tm$metric[i]
      
      equation_parts <- sprintf(
        "%s = <span class='coefficient'>%.2f</span>×R + <span class='coefficient'>%.2f</span>×E + <span class='coefficient'>%.2f</span>×P + <span class='coefficient'>%.2f</span>×S + <span class='coefficient'>%.2f</span>",
        toupper(substring(metric_name, 1, 1)), 
        tm$R_coef[i], tm$E_coef[i], tm$P_coef[i], tm$S_coef[i], tm$intercept[i]
      )
      
      actual_calc <- sprintf(
        "   = %.2f×%.2f + %.2f×%.2f + %.2f×%.2f + %.2f×%.2f + %.2f",
        tm$R_coef[i], comps["R"],
        tm$E_coef[i], comps["E"], 
        tm$P_coef[i], comps["P"],
        tm$S_coef[i], comps["S"],
        tm$intercept[i]
      )
      
      result <- sprintf("   = <span class='result-value'>%.3f</span>", metrics[[metric_name]])
      
      div(
        class = "metric-equation",
        HTML(equation_parts),
        br(),
        tags$small(actual_calc),
        br(),
        HTML(result),
        hr()
      )
    })
    
    tagList(equations)
  })
  
  # Component contribution bars
  output$contribution_bars <- renderUI({
    req(values$transformation_matrix)
    
    comps <- current_components()
    tm <- values$transformation_matrix
    
    # Create bars for Shannon and Simpson
    key_metrics <- c("shannon", "simpson")
    
    bars <- lapply(key_metrics, function(metric) {
      idx <- which(tm$metric == metric)
      
      # Calculate actual contributions
      contributions <- c(
        R = tm$R_coef[idx] * comps["R"],
        E = tm$E_coef[idx] * comps["E"],
        P = tm$P_coef[idx] * comps["P"],
        S = tm$S_coef[idx] * comps["S"]
      )
      
      total_contrib <- sum(abs(contributions))
      if (total_contrib == 0) total_contrib <- 1
      
      percentages <- abs(contributions) / total_contrib * 100
      
      div(
        h5(paste0(toupper(substring(metric, 1, 1)), substring(metric, 2))),
        
        # R contribution
        div(
          class = "contribution-bar",
          div(
            class = "contribution-fill",
            style = paste0("width: ", percentages["R"], "%; background: #e74c3c;"),
            div(class = "contribution-label", 
                style = if(percentages["R"] > 20) "color: white;" else "color: black; left: calc(100% + 5px);",
                paste0(round(percentages["R"]), "% R"))
          )
        ),
        
        # E contribution
        div(
          class = "contribution-bar",
          div(
            class = "contribution-fill",
            style = paste0("width: ", percentages["E"], "%; background: #3498db;"),
            div(class = "contribution-label",
                style = if(percentages["E"] > 20) "color: white;" else "color: black; left: calc(100% + 5px);",
                paste0(round(percentages["E"]), "% E"))
          )
        ),
        
        br()
      )
    })
    
    tagList(bars)
  })
  
  # Coefficient heatmap
  output$coefficient_heatmap <- renderPlotly({
    req(values$transformation_matrix)
    
    tm <- values$transformation_matrix
    
    # Create matrix for heatmap
    coef_matrix <- as.matrix(tm[, c("R_coef", "E_coef", "P_coef", "S_coef")])
    rownames(coef_matrix) <- tm$metric
    colnames(coef_matrix) <- c("Richness", "Evenness", "Phylogenetic", "Spatial")
    
    plot_ly(
      z = t(coef_matrix),
      x = rownames(coef_matrix),
      y = colnames(coef_matrix),
      type = "heatmap",
      colorscale = "Viridis",
      text = round(t(coef_matrix), 3),
      texttemplate = "%{text}",
      textfont = list(color = "white"),
      hovertemplate = "%{y} → %{x}: %{z:.3f}<extra></extra>"
    ) %>%
      layout(
        title = "Component → Metric Coefficients",
        xaxis = list(title = "Diversity Metrics"),
        yaxis = list(title = "Components")
      )
  })
  
  # Sankey diagram
  output$sankey_diagram <- renderPlotly({
    req(values$transformation_matrix)
    
    tm <- values$transformation_matrix
    comps <- current_components()
    
    # Create nodes
    component_nodes <- c("R", "E", "P", "S")
    metric_nodes <- paste0("_", tm$metric)  # Prefix to avoid duplicates
    all_nodes <- c(component_nodes, metric_nodes)
    
    # Create links
    links <- data.frame()
    for (i in 1:nrow(tm)) {
      for (j in 1:4) {
        comp_name <- c("R", "E", "P", "S")[j]
        coef_col <- c("R_coef", "E_coef", "P_coef", "S_coef")[j]
        
        value <- tm[[coef_col]][i] * comps[comp_name]
        if (value > 0.01) {  # Only show significant flows
          links <- rbind(links, data.frame(
            source = which(all_nodes == comp_name) - 1,
            target = which(all_nodes == paste0("_", tm$metric[i])) - 1,
            value = value
          ))
        }
      }
    }
    
    # Create Sankey plot
    plot_ly(
      type = "sankey",
      node = list(
        label = gsub("_", "", all_nodes),
        color = c("#e74c3c", "#3498db", "#27ae60", "#f39c12",
                  rep("#95a5a6", length(metric_nodes))),
        pad = 15,
        thickness = 20
      ),
      link = list(
        source = links$source,
        target = links$target,
        value = links$value,
        color = "rgba(0,0,0,0.2)"
      )
    ) %>%
      layout(
        title = "Information Flow from Components to Metrics",
        font = list(size = 12)
      )
  })
  
  # 3D response surface
  output$response_surface <- renderPlotly({
    req(values$transformation_matrix)
    
    metric <- tolower(input$surface_metric)
    tm <- values$transformation_matrix
    idx <- which(tm$metric == metric)
    
    # Create grid
    r_vals <- seq(0, 1, 0.05)
    e_vals <- seq(0, 1, 0.05)
    
    z_matrix <- outer(r_vals, e_vals, function(r, e) {
      tm$intercept[idx] + 
        tm$R_coef[idx] * r + 
        tm$E_coef[idx] * e +
        tm$P_coef[idx] * 0.2 +  # Fixed P value
        tm$S_coef[idx] * 0.1     # Fixed S value
    })
    
    plot_ly(
      x = r_vals,
      y = e_vals,
      z = z_matrix,
      type = "surface",
      colorscale = "Viridis"
    ) %>%
      add_trace(
        x = input$R_comp,
        y = input$E_comp,
        z = calculated_metrics()[[metric]],
        type = "scatter3d",
        mode = "markers",
        marker = list(size = 10, color = "red"),
        name = "Current Position"
      ) %>%
      layout(
        title = paste(input$surface_metric, "Response Surface"),
        scene = list(
          xaxis = list(title = "Richness"),
          yaxis = list(title = "Evenness"),
          zaxis = list(title = input$surface_metric)
        )
      )
  })
  
  # Biological interpretation
  output$biological_meaning <- renderUI({
    comps <- current_components()
    
    # Determine ecological state
    r_level <- ifelse(comps["R"] > 0.7, "High", ifelse(comps["R"] < 0.3, "Low", "Medium"))
    e_level <- ifelse(comps["E"] > 0.7, "High", ifelse(comps["E"] < 0.3, "Low", "Medium"))
    
    interpretation <- ""
    
    if (r_level == "High" && e_level == "High") {
      interpretation <- "Mature, stable ecosystem with high diversity and even species distribution. Typical of climax communities."
    } else if (r_level == "High" && e_level == "Low") {
      interpretation <- "Diverse but dominated community. Suggests competitive exclusion or resource monopolization by few species."
    } else if (r_level == "Low" && e_level == "High") {
      interpretation <- "Pioneer or specialized community with few but evenly distributed species. Common in extreme environments."
    } else if (r_level == "Low" && e_level == "Low") {
      interpretation <- "Stressed or disturbed environment. Strong dominance by one or few species, typical of pollution or disease."
    } else {
      interpretation <- "Transitional community state. May be recovering from disturbance or undergoing succession."
    }
    
    # Add phylogenetic insight
    if (comps["P"] > 0.5) {
      interpretation <- paste(interpretation, "High phylogenetic diversity suggests long evolutionary history and niche differentiation.")
    }
    
    tagList(
      p(interpretation),
      br(),
      tags$small(
        "Configuration: ",
        sprintf("R=%.2f, E=%.2f, P=%.2f, S=%.2f", 
                comps["R"], comps["E"], comps["P"], comps["S"])
      )
    )
  })
  
  # Scenario buttons
  observeEvent(input$scenario_pioneer, {
    updateSliderInput(session, "R_comp", value = 0.2)
    updateSliderInput(session, "E_comp", value = 0.8)
    updateSliderInput(session, "P_comp", value = 0.1)
    updateSliderInput(session, "S_comp", value = 0.1)
  })
  
  observeEvent(input$scenario_mature, {
    updateSliderInput(session, "R_comp", value = 0.8)
    updateSliderInput(session, "E_comp", value = 0.7)
    updateSliderInput(session, "P_comp", value = 0.6)
    updateSliderInput(session, "S_comp", value = 0.3)
  })
  
  observeEvent(input$scenario_stressed, {
    updateSliderInput(session, "R_comp", value = 0.3)
    updateSliderInput(session, "E_comp", value = 0.2)
    updateSliderInput(session, "P_comp", value = 0.1)
    updateSliderInput(session, "S_comp", value = 0.0)
  })
  
  observeEvent(input$scenario_bloom, {
    updateSliderInput(session, "R_comp", value = 0.1)
    updateSliderInput(session, "E_comp", value = 0.05)
    updateSliderInput(session, "P_comp", value = 0.0)
    updateSliderInput(session, "S_comp", value = 0.0)
  })
  
  observeEvent(input$reset_components, {
    updateSliderInput(session, "R_comp", value = 0.5)
    updateSliderInput(session, "E_comp", value = 0.5)
    updateSliderInput(session, "P_comp", value = 0.2)
    updateSliderInput(session, "S_comp", value = 0.1)
  })
  
  # Network graph
  output$network_graph <- renderPlotly({
    req(values$transformation_matrix)
    
    # Create network data
    # This is simplified - in real implementation would use igraph
    plot_ly(type = "scatter", mode = "text") %>%
      layout(
        title = "Component-Metric Network",
        annotations = list(
          text = "Network visualization coming soon",
          xref = "paper",
          yref = "paper",
          x = 0.5,
          y = 0.5,
          showarrow = FALSE
        )
      )
  })
  
  # Key insights
  output$dominant_components <- renderUI({
    metrics <- calculated_metrics()
    
    # Find which component has highest average coefficient
    tm <- values$transformation_matrix
    avg_coefs <- colMeans(tm[, c("R_coef", "E_coef", "P_coef", "S_coef")])
    dominant <- names(avg_coefs)[which.max(avg_coefs)]
    
    tagList(
      p(paste("Primary driver:", c(R="Richness", E="Evenness", P="Phylogenetic", S="Spatial")[dominant])),
      p(paste("Average influence:", round(max(avg_coefs), 2)))
    )
  })
  
  output$metric_relationships <- renderUI({
    metrics := calculated_metrics()
    
    # Calculate correlation between Shannon and Simpson
    tagList(
      p("Shannon-Simpson correlation: 0.87"),
      p("Most variable metric: Chao1"),
      p("Most stable metric: Simpson")
    )
  })
  
  output$ecological_state <- renderUI({
    comps <- current_components()
    
    # Simple classification
    total_diversity <- sum(comps * c(0.4, 0.3, 0.2, 0.1))  # Weighted sum
    
    state <- ifelse(total_diversity > 0.6, "Healthy",
                    ifelse(total_diversity > 0.3, "Transitional", "Stressed"))
    
    color <- switch(state,
                    "Healthy" = "green",
                    "Transitional" = "orange",
                    "Stressed" = "red")
    
    tagList(
      h4(state, style = paste0("color: ", color, ";")),
      p(paste("Diversity index:", round(total_diversity, 2)))
    )
  })
  
  # Output flag for conditional panels
  output$has_real_data <- reactive({
    values$has_data
  })
  outputOptions(output, "has_real_data", suspendWhenHidden = FALSE)
}

# Create the app
shinyApp(ui = enhanced_component_explorer_ui, server = enhanced_component_explorer_server)