# Interactive Component Explorer Widget
# Real-time exploration of R, E, P, S parameter space with live metric calculations

library(shiny)
library(plotly)
library(diversityGPT)

# UI for component explorer
component_explorer_ui <- fluidPage(
  titlePanel(
    div(
      h2("Universal Component Explorer"),
      p("Explore how R, E, P, S components create ALL diversity metrics"),
      style = "text-align: center; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; padding: 20px; margin: -15px -15px 20px -15px; border-radius: 10px;"
    )
  ),
  
  fluidRow(
    # Left panel - Sliders
    column(
      width = 4,
      
      div(
        style = "background: #f8f9fa; padding: 20px; border-radius: 10px; box-shadow: 0 2px 10px rgba(0,0,0,0.1);",
        
        h3("Component Controls", icon("sliders-h")),
        p("Adjust the fundamental information components:", style = "color: #666;"),
        
        br(),
        
        # Richness component
        div(
          style = "margin-bottom: 25px;",
          h4("🔢 Richness (R)", style = "color: #e74c3c;"),
          sliderInput(
            "R_value",
            "Species count information:",
            min = 0,
            max = 1,
            value = 0.5,
            step = 0.01,
            width = "100%"
          ),
          p("Controls: Observed species, Chao1, ACE", style = "font-size: 12px; color: #7f8c8d;")
        ),
        
        # Evenness component
        div(
          style = "margin-bottom: 25px;",
          h4("⚖️ Evenness (E)", style = "color: #3498db;"),
          sliderInput(
            "E_value",
            "Distribution uniformity:",
            min = 0,
            max = 1,
            value = 0.5,
            step = 0.01,
            width = "100%"
          ),
          p("Controls: Shannon, Simpson, Pielou", style = "font-size: 12px; color: #7f8c8d;")
        ),
        
        # Phylogenetic component
        div(
          style = "margin-bottom: 25px;",
          h4("🌳 Phylogenetic (P)", style = "color: #27ae60;"),
          sliderInput(
            "P_value",
            "Evolutionary diversity:",
            min = 0,
            max = 1,
            value = 0.2,
            step = 0.01,
            width = "100%"
          ),
          p("Controls: Faith's PD, Phylogenetic Shannon", style = "font-size: 12px; color: #7f8c8d;")
        ),
        
        # Spatial component
        div(
          style = "margin-bottom: 25px;",
          h4("🗺️ Spatial (S)", style = "color: #f39c12;"),
          sliderInput(
            "S_value",
            "Geographic patterns:",
            min = 0,
            max = 1,
            value = 0.1,
            step = 0.01,
            width = "100%"
          ),
          p("Controls: Beta diversity, distance decay", style = "font-size: 12px; color: #7f8c8d;")
        ),
        
        hr(),
        
        # Normalization
        checkboxInput(
          "normalize",
          "Auto-normalize to sum = 1",
          value = TRUE
        ),
        
        actionButton(
          "random_combo",
          "Random Combination",
          icon = icon("dice"),
          class = "btn-info btn-block"
        ),
        
        br(),
        
        actionButton(
          "reset_sliders",
          "Reset All",
          icon = icon("undo"),
          class = "btn-secondary btn-block"
        )
      )
    ),
    
    # Right panel - Visualizations
    column(
      width = 8,
      
      # Live metrics display
      div(
        style = "background: white; padding: 15px; border-radius: 10px; box-shadow: 0 2px 10px rgba(0,0,0,0.1); margin-bottom: 20px;",
        
        h3("🔢 Live Diversity Metrics", style = "margin-top: 0;"),
        
        fluidRow(
          column(width = 6,
            h4("Alpha Diversity"),
            tableOutput("alpha_metrics")
          ),
          column(width = 6,
            h4("Metric Relationships"),
            tableOutput("metric_relationships")
          )
        )
      ),
      
      # Component space visualization
      div(
        style = "background: white; padding: 15px; border-radius: 10px; box-shadow: 0 2px 10px rgba(0,0,0,0.1); margin-bottom: 20px;",
        
        h3("📊 Component Space Visualization"),
        
        tabsetPanel(
          tabPanel(
            "2D Component Space",
            br(),
            fluidRow(
              column(width = 6,
                selectInput("x_axis", "X-axis:", 
                           choices = c("R" = "R", "E" = "E", "P" = "P", "S" = "S"),
                           selected = "R")
              ),
              column(width = 6,
                selectInput("y_axis", "Y-axis:",
                           choices = c("R" = "R", "E" = "E", "P" = "P", "S" = "S"), 
                           selected = "E")
              )
            ),
            plotlyOutput("component_2d", height = "400px")
          ),
          
          tabPanel(
            "3D Component Space",
            br(),
            plotlyOutput("component_3d", height = "500px")
          ),
          
          tabPanel(
            "Component Radar",
            br(),
            plotlyOutput("component_radar", height = "400px")
          )
        )
      ),
      
      # Metric evolution over time
      div(
        style = "background: white; padding: 15px; border-radius: 10px; box-shadow: 0 2px 10px rgba(0,0,0,0.1);",
        
        h3("📈 Metric Evolution"),
        p("Watch how diversity metrics change as you adjust components"),
        
        tabsetPanel(
          tabPanel(
            "Time Series",
            br(),
            plotlyOutput("metric_evolution", height = "300px")
          ),
          
          tabPanel(
            "Component Composition",
            br(),
            plotlyOutput("component_composition", height = "300px")
          ),
          
          tabPanel(
            "Metric Heatmap",
            br(),
            plotlyOutput("metric_heatmap", height = "300px")
          )
        )
      )
    )
  )
)

# Server logic
component_explorer_server <- function(input, output, session) {
  
  # Reactive values to store history
  values <- reactiveValues(
    history = data.frame(
      time = numeric(0),
      R = numeric(0),
      E = numeric(0), 
      P = numeric(0),
      S = numeric(0),
      shannon = numeric(0),
      simpson = numeric(0),
      observed = numeric(0),
      chao1 = numeric(0),
      pielou = numeric(0),
      faith_pd = numeric(0)
    ),
    counter = 0
  )
  
  # Get current normalized components
  current_components <- reactive({
    components <- c(
      R = input$R_value,
      E = input$E_value,
      P = input$P_value,
      S = input$S_value
    )
    
    if (input$normalize && sum(components) > 0) {
      components <- components / sum(components)
    }
    
    components
  })
  
  # Calculate metrics from components
  current_metrics <- reactive({
    comp <- current_components()
    
    # Universal transformation equations (simplified for demo)
    # In reality, these would use the actual transformation matrix
    
    list(
      shannon = 2.5 * comp["R"] + 1.8 * comp["E"] + 0.5 * comp["P"] + 0.2 * comp["S"],
      simpson = 0.8 * comp["R"] + 0.9 * comp["E"] + 0.3 * comp["P"] + 0.1 * comp["S"],
      observed = 50 * comp["R"] + 10 * comp["E"] + 15 * comp["P"] + 5 * comp["S"],
      chao1 = 65 * comp["R"] + 15 * comp["E"] + 20 * comp["P"] + 8 * comp["S"],
      pielou = 0.3 * comp["R"] + 0.85 * comp["E"] + 0.2 * comp["P"] + 0.05 * comp["S"],
      faith_pd = 0.5 * comp["R"] + 0.3 * comp["E"] + 2.0 * comp["P"] + 0.4 * comp["S"]
    )
  })
  
  # Update history when components change
  observeEvent({
    input$R_value
    input$E_value
    input$P_value
    input$S_value
  }, {
    comp <- current_components()
    metrics <- current_metrics()
    
    values$counter <- values$counter + 1
    
    new_row <- data.frame(
      time = values$counter,
      R = comp["R"],
      E = comp["E"],
      P = comp["P"],
      S = comp["S"],
      shannon = metrics$shannon,
      simpson = metrics$simpson,
      observed = metrics$observed,
      chao1 = metrics$chao1,
      pielou = metrics$pielou,
      faith_pd = metrics$faith_pd
    )
    
    values$history <- rbind(values$history, new_row)
    
    # Keep only last 100 points
    if (nrow(values$history) > 100) {
      values$history <- tail(values$history, 100)
    }
  })
  
  # Alpha metrics table
  output$alpha_metrics <- renderTable({
    metrics <- current_metrics()
    
    data.frame(
      Metric = c("Shannon", "Simpson", "Observed", "Chao1", "Pielou", "Faith's PD"),
      Value = round(c(
        metrics$shannon,
        metrics$simpson,
        metrics$observed,
        metrics$chao1,
        metrics$pielou,
        metrics$faith_pd
      ), 3),
      stringsAsFactors = FALSE
    )
  }, bordered = TRUE, hover = TRUE)
  
  # Metric relationships
  output$metric_relationships <- renderTable({
    comp <- current_components()
    
    data.frame(
      Component = c("Richness", "Evenness", "Phylogenetic", "Spatial"),
      Value = round(comp, 3),
      Percentage = paste0(round(comp * 100, 1), "%"),
      stringsAsFactors = FALSE
    )
  }, bordered = TRUE, hover = TRUE)
  
  # 2D component space
  output$component_2d <- renderPlotly({
    comp <- current_components()
    
    # Create background grid showing all possible combinations
    grid_points <- expand.grid(
      x = seq(0, 1, 0.1),
      y = seq(0, 1, 0.1)
    )
    
    plot_ly() %>%
      add_trace(
        data = grid_points,
        x = ~x,
        y = ~y,
        type = "scatter",
        mode = "markers",
        marker = list(size = 4, color = "lightgray", opacity = 0.3),
        name = "Grid",
        hoverinfo = "none"
      ) %>%
      add_trace(
        x = comp[input$x_axis],
        y = comp[input$y_axis],
        type = "scatter",
        mode = "markers",
        marker = list(size = 20, color = "red", symbol = "star"),
        name = "Current Position",
        text = paste0(
          input$x_axis, ": ", round(comp[input$x_axis], 3), "<br>",
          input$y_axis, ": ", round(comp[input$y_axis], 3)
        ),
        hovertemplate = "%{text}<extra></extra>"
      ) %>%
      layout(
        title = paste("Component Space:", input$x_axis, "vs", input$y_axis),
        xaxis = list(title = paste0(input$x_axis, "_component"), range = c(0, 1)),
        yaxis = list(title = paste0(input$y_axis, "_component"), range = c(0, 1)),
        showlegend = FALSE
      )
  })
  
  # 3D component space
  output$component_3d <- renderPlotly({
    comp <- current_components()
    
    plot_ly() %>%
      add_trace(
        x = comp["R"],
        y = comp["E"], 
        z = comp["P"],
        type = "scatter3d",
        mode = "markers",
        marker = list(size = 10, color = "red"),
        name = "Current Position",
        text = paste0(
          "R: ", round(comp["R"], 3), "<br>",
          "E: ", round(comp["E"], 3), "<br>",
          "P: ", round(comp["P"], 3), "<br>",
          "S: ", round(comp["S"], 3)
        )
      ) %>%
      layout(
        title = "3D Component Space (R-E-P)",
        scene = list(
          xaxis = list(title = "Richness", range = c(0, 1)),
          yaxis = list(title = "Evenness", range = c(0, 1)),
          zaxis = list(title = "Phylogenetic", range = c(0, 1))
        )
      )
  })
  
  # Component radar chart
  output$component_radar <- renderPlotly({
    comp <- current_components()
    
    # Prepare data for radar chart
    categories <- c("Richness", "Evenness", "Phylogenetic", "Spatial", "Richness")
    values <- c(comp["R"], comp["E"], comp["P"], comp["S"], comp["R"])
    
    plot_ly(
      type = "scatterpolar",
      r = values,
      theta = categories,
      fill = "toself",
      name = "Components",
      line = list(color = "#1f77b4"),
      fillcolor = "rgba(31, 119, 180, 0.3)"
    ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(0, 1)
          )
        ),
        title = "Component Radar Chart"
      )
  })
  
  # Metric evolution over time
  output$metric_evolution <- renderPlotly({
    req(nrow(values$history) > 0)
    
    history <- values$history
    
    plot_ly(history) %>%
      add_trace(y = ~shannon, x = ~time, name = "Shannon", type = "scatter", mode = "lines") %>%
      add_trace(y = ~simpson, x = ~time, name = "Simpson", type = "scatter", mode = "lines") %>%
      add_trace(y = ~pielou, x = ~time, name = "Pielou", type = "scatter", mode = "lines") %>%
      layout(
        title = "Metric Evolution Over Time",
        xaxis = list(title = "Time"),
        yaxis = list(title = "Metric Value"),
        hovermode = "x unified"
      )
  })
  
  # Component composition over time
  output$component_composition <- renderPlotly({
    req(nrow(values$history) > 0)
    
    history <- values$history
    
    plot_ly(history) %>%
      add_trace(y = ~R, x = ~time, name = "Richness", type = "scatter", mode = "lines", 
                line = list(color = "#e74c3c")) %>%
      add_trace(y = ~E, x = ~time, name = "Evenness", type = "scatter", mode = "lines",
                line = list(color = "#3498db")) %>%
      add_trace(y = ~P, x = ~time, name = "Phylogenetic", type = "scatter", mode = "lines",
                line = list(color = "#27ae60")) %>%
      add_trace(y = ~S, x = ~time, name = "Spatial", type = "scatter", mode = "lines",
                line = list(color = "#f39c12")) %>%
      layout(
        title = "Component Evolution",
        xaxis = list(title = "Time"),
        yaxis = list(title = "Component Value", range = c(0, 1)),
        hovermode = "x unified"
      )
  })
  
  # Random combination
  observeEvent(input$random_combo, {
    updateSliderInput(session, "R_value", value = runif(1, 0, 1))
    updateSliderInput(session, "E_value", value = runif(1, 0, 1))
    updateSliderInput(session, "P_value", value = runif(1, 0, 1))
    updateSliderInput(session, "S_value", value = runif(1, 0, 1))
  })
  
  # Reset sliders
  observeEvent(input$reset_sliders, {
    updateSliderInput(session, "R_value", value = 0.5)
    updateSliderInput(session, "E_value", value = 0.5)
    updateSliderInput(session, "P_value", value = 0.2)
    updateSliderInput(session, "S_value", value = 0.1)
    
    # Clear history
    values$history <- data.frame(
      time = numeric(0),
      R = numeric(0),
      E = numeric(0),
      P = numeric(0),
      S = numeric(0),
      shannon = numeric(0),
      simpson = numeric(0),
      observed = numeric(0),
      chao1 = numeric(0),
      pielou = numeric(0),
      faith_pd = numeric(0)
    )
    values$counter <- 0
  })
}

# Standalone app
shinyApp(ui = component_explorer_ui, server = component_explorer_server)