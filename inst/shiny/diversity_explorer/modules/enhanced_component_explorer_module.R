# Enhanced Component Explorer Module
# Modular version for integration into main app

# UI function
enhanced_component_explorer_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
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
          left: 0;
          top: 0;
          transition: width 0.3s ease;
        }
        
        .biological-interpretation {
          background: #e8f6f3;
          padding: 15px;
          border-radius: 8px;
          margin-top: 20px;
          border: 1px solid #27ae60;
        }
        
        .scenario-btn {
          margin: 2px;
          width: 48%;
        }
      "))
    ),
    
    h3("🧬 Enhanced Component Explorer"),
    p("Discover the mathematical relationships between information components and diversity metrics"),
    
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
            sliderInput(ns("R_comp"), NULL, 
                        min = 0, max = 1, value = 0.5, step = 0.01, width = "100%"),
            p("Species count information", style = "font-size: 11px; color: #7f8c8d; margin-top: -5px;")
          ),
          
          # Evenness
          div(
            h4("⚖️ Evenness (E)", style = "color: #3498db; margin-bottom: 5px;"),
            sliderInput(ns("E_comp"), NULL,
                        min = 0, max = 1, value = 0.5, step = 0.01, width = "100%"),
            p("Distribution uniformity", style = "font-size: 11px; color: #7f8c8d; margin-top: -5px;")
          ),
          
          # Phylogenetic
          div(
            h4("🌳 Phylogenetic (P)", style = "color: #27ae60; margin-bottom: 5px;"),
            sliderInput(ns("P_comp"), NULL,
                        min = 0, max = 1, value = 0.2, step = 0.01, width = "100%"),
            p("Evolutionary diversity", style = "font-size: 11px; color: #7f8c8d; margin-top: -5px;")
          ),
          
          # Spatial
          div(
            h4("🗺️ Spatial (S)", style = "color: #f39c12; margin-bottom: 5px;"),
            sliderInput(ns("S_comp"), NULL,
                        min = 0, max = 1, value = 0.1, step = 0.01, width = "100%"),
            p("Geographic patterns", style = "font-size: 11px; color: #7f8c8d; margin-top: -5px;")
          ),
          
          hr(),
          
          checkboxInput(ns("normalize_comps"), "Auto-normalize to sum = 1", value = TRUE),
          
          # Scenario buttons
          h4("📋 Ecological Scenarios"),
          div(
            actionButton(ns("scenario_pioneer"), "🌱 Pioneer", class = "btn-success scenario-btn"),
            actionButton(ns("scenario_mature"), "🌳 Mature", class = "btn-primary scenario-btn"),
            actionButton(ns("scenario_stressed"), "🏜️ Stressed", class = "btn-warning scenario-btn"),
            actionButton(ns("scenario_bloom"), "🦠 Bloom", class = "btn-danger scenario-btn")
          ),
          
          br(),
          
          actionButton(ns("reset_components"), "Reset All", class = "btn-secondary btn-block", icon = icon("undo"))
        ),
        
        # Biological interpretation
        div(
          class = "biological-interpretation",
          h4("🔬 Biological Interpretation", style = "margin-top: 0;"),
          uiOutput(ns("biological_meaning"))
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
            uiOutput(ns("live_equations"))
          )
        ),
        
        # Component contributions
        wellPanel(
          h3("📊 Component Contributions", style = "margin-top: 0;"),
          uiOutput(ns("contribution_bars"))
        ),
        
        # Prediction accuracy (if data loaded)
        conditionalPanel(
          condition = paste0("output['", ns("has_real_data"), "']"),
          wellPanel(
            h3("🎯 Prediction vs Reality", style = "margin-top: 0;"),
            div(
              class = "prediction-comparison",
              DT::dataTableOutput(ns("prediction_accuracy"), height = "200px")
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
            plotlyOutput(ns("coefficient_heatmap"), height = "400px"),
            p("Darker = stronger influence", style = "text-align: center; color: #7f8c8d;")
          ),
          
          # Sankey diagram
          tabPanel(
            "Flow Diagram", 
            br(),
            h4("Information Flow"),
            plotlyOutput(ns("sankey_diagram"), height = "400px"),
            p("Width = contribution strength", style = "text-align: center; color: #7f8c8d;")
          ),
          
          # 3D response surface
          tabPanel(
            "3D Surface",
            br(),
            h4("Response Surface"),
            selectInput(ns("surface_metric"), "Select metric:",
                        choices = c("Shannon", "Simpson", "Chao1", "Pielou"),
                        selected = "Shannon"),
            plotlyOutput(ns("response_surface"), height = "400px")
          ),
          
          # Network graph
          tabPanel(
            "Network",
            br(),
            h4("Component-Metric Network"),
            plotlyOutput(ns("network_graph"), height = "400px")
          ),
          
          # Uncertainty
          tabPanel(
            "Uncertainty",
            br(),
            h4("Prediction Confidence"),
            plotlyOutput(ns("uncertainty_plot"), height = "400px")
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
              uiOutput(ns("dominant_components"))
            ),
            
            column(
              width = 4,
              h4("Metric Relationships"),
              uiOutput(ns("metric_relationships"))
            ),
            
            column(
              width = 4,
              h4("Ecological State"),
              uiOutput(ns("ecological_state"))
            )
          )
        )
      )
    )
  )
}

# Server function
enhanced_component_explorer_server <- function(id, universal_info = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    
    # Get normalized components
    current_components <- reactive({
      components <- c(
        R = input$R_comp,
        E = input$E_comp, 
        P = input$P_comp,
        S = input$S_comp
      )
      
      if (input$normalize_comps && sum(components) > 0) {
        components <- components / sum(components)
      }
      
      components
    })
    
    # Get transformation matrix (use default or from data)
    transformation_matrix <- reactive({
      if (!is.null(universal_info()) && !is.null(universal_info()$transformation_matrix)) {
        tm <- universal_info()$transformation_matrix
        # Ensure column names match expected format
        if ("R_coef" %in% colnames(tm)) {
          names(tm)[names(tm) == "R_coef"] <- "R_component"
          names(tm)[names(tm) == "E_coef"] <- "E_component"
          names(tm)[names(tm) == "P_coef"] <- "P_component"
          names(tm)[names(tm) == "S_coef"] <- "S_component"
        }
        tm
      } else {
        # Default transformation matrix for demonstration
        data.frame(
          metric = c("shannon", "simpson", "observed", "chao1", "pielou", "faith_pd"),
          R_component = c(0.3, 0.1, 0.9, 0.85, 0.05, 0.4),
          E_component = c(0.6, 0.8, 0.05, 0.1, 0.9, 0.2),
          P_component = c(0.08, 0.05, 0.03, 0.03, 0.03, 0.35),
          S_component = c(0.02, 0.05, 0.02, 0.02, 0.02, 0.05),
          intercept = c(0.1, 0.05, 10, 15, 0.02, 0.5)
        )
      }
    })
    
    # Calculate predicted metrics
    predicted_metrics <- reactive({
      comp <- current_components()
      tm <- transformation_matrix()
      
      metrics <- list()
      for (i in 1:nrow(tm)) {
        metric_name <- tm$metric[i]
        value <- tm$R_component[i] * comp["R"] +
                 tm$E_component[i] * comp["E"] +
                 tm$P_component[i] * comp["P"] +
                 tm$S_component[i] * comp["S"] +
                 tm$intercept[i]
        metrics[[metric_name]] <- round(value, 3)
      }
      
      metrics
    })
    
    # Live equations output
    output$live_equations <- renderUI({
      comp <- current_components()
      tm <- transformation_matrix()
      metrics <- predicted_metrics()
      
      equations <- lapply(1:nrow(tm), function(i) {
        metric_name <- tm$metric[i]
        
        # Build equation string
        equation_parts <- sprintf(
          "%s = <span class='coefficient'>%.2f</span>×R + <span class='coefficient'>%.2f</span>×E + <span class='coefficient'>%.2f</span>×P + <span class='coefficient'>%.2f</span>×S + <span class='coefficient'>%.2f</span>",
          toupper(substring(metric_name, 1, 1)), 
          tm$R_component[i], tm$E_component[i], tm$P_component[i], tm$S_component[i], tm$intercept[i]
        )
        
        # Add current values
        current_calc <- sprintf(
          " = <span class='coefficient'>%.2f</span>×%.2f + <span class='coefficient'>%.2f</span>×%.2f + <span class='coefficient'>%.2f</span>×%.2f + <span class='coefficient'>%.2f</span>×%.2f + <span class='coefficient'>%.2f</span>",
          tm$R_component[i], comp["R"],
          tm$E_component[i], comp["E"],
          tm$P_component[i], comp["P"],
          tm$S_component[i], comp["S"],
          tm$intercept[i]
        )
        
        # Final result
        result <- sprintf(" = <span class='result-value'>%.3f</span>", metrics[[metric_name]])
        
        div(
          class = "metric-equation",
          HTML(paste0(equation_parts, "<br>", current_calc, result))
        )
      })
      
      tagList(equations)
    })
    
    # Component contribution bars
    output$contribution_bars <- renderUI({
      comp <- current_components()
      tm <- transformation_matrix()
      
      bars <- lapply(1:nrow(tm), function(i) {
        metric_name <- tm$metric[i]
        
        # Calculate contributions with NA handling
        r_contrib <- if (!is.na(tm$R_component[i]) && !is.na(comp["R"])) tm$R_component[i] * comp["R"] else 0
        e_contrib <- if (!is.na(tm$E_component[i]) && !is.na(comp["E"])) tm$E_component[i] * comp["E"] else 0
        p_contrib <- if (!is.na(tm$P_component[i]) && !is.na(comp["P"])) tm$P_component[i] * comp["P"] else 0
        s_contrib <- if (!is.na(tm$S_component[i]) && !is.na(comp["S"])) tm$S_component[i] * comp["S"] else 0
        
        total_contrib <- r_contrib + e_contrib + p_contrib + s_contrib
        
        # Ensure we have valid values
        if (!is.na(total_contrib) && total_contrib > 0) {
          r_width <- (r_contrib / total_contrib) * 100
          e_width <- (e_contrib / total_contrib) * 100
          p_width <- (p_contrib / total_contrib) * 100
          s_width <- (s_contrib / total_contrib) * 100
        } else {
          r_width <- e_width <- p_width <- s_width <- 0
        }
        
        div(
          h5(toupper(metric_name)),
          div(
            class = "contribution-bar",
            div(class = "contribution-fill", style = paste0("width: ", r_width, "%; background: #e74c3c;")),
            div(class = "contribution-fill", style = paste0("width: ", e_width, "%; background: #3498db; left: ", r_width, "%;")),
            div(class = "contribution-fill", style = paste0("width: ", p_width, "%; background: #27ae60; left: ", r_width + e_width, "%;")),
            div(class = "contribution-fill", style = paste0("width: ", s_width, "%; background: #f39c12; left: ", r_width + e_width + p_width, "%;"))
          )
        )
      })
      
      tagList(bars)
    })
    
    # Biological interpretation
    output$biological_meaning <- renderUI({
      comp <- current_components()
      
      # Determine dominant components
      dominant <- names(comp)[which.max(comp)]
      
      interpretation <- switch(dominant,
        "R" = list(
          state = "Species-rich community",
          description = "High richness dominance suggests a diverse community with many species. Common in stable, resource-rich environments or early succession.",
          mechanisms = c("Low disturbance", "High resource availability", "Multiple niches")
        ),
        "E" = list(
          state = "Well-balanced community",
          description = "High evenness indicates species abundances are similar. Suggests competitive equilibrium or uniform resource distribution.",
          mechanisms = c("Competitive exclusion", "Resource partitioning", "Stable conditions")
        ),
        "P" = list(
          state = "Phylogenetically diverse",
          description = "High phylogenetic diversity indicates evolutionary breadth. Common in ancient or stable environments.",
          mechanisms = c("Long evolutionary history", "Multiple colonization events", "Niche conservatism")
        ),
        "S" = list(
          state = "Spatially structured",
          description = "High spatial component suggests geographic patterns dominate. Common in patchy environments.",
          mechanisms = c("Dispersal limitation", "Environmental gradients", "Spatial heterogeneity")
        )
      )
      
      tagList(
        h5(interpretation$state, style = "color: #27ae60;"),
        p(interpretation$description),
        h6("Likely mechanisms:"),
        tags$ul(
          lapply(interpretation$mechanisms, function(m) tags$li(m))
        )
      )
    })
    
    # Coefficient heatmap
    output$coefficient_heatmap <- renderPlotly({
      tm <- transformation_matrix()
      
      # Prepare matrix for heatmap
      coef_matrix <- as.matrix(tm[, c("R_component", "E_component", "P_component", "S_component")])
      rownames(coef_matrix) <- tm$metric
      
      plot_ly(
        z = t(coef_matrix),
        x = tm$metric,
        y = c("R", "E", "P", "S"),
        type = "heatmap",
        colorscale = "Blues",
        hovertemplate = "Metric: %{x}<br>Component: %{y}<br>Coefficient: %{z}<extra></extra>"
      ) %>%
        layout(
          xaxis = list(title = "Diversity Metrics"),
          yaxis = list(title = "Components")
        )
    })
    
    # Sankey diagram
    output$sankey_diagram <- renderPlotly({
      comp <- current_components()
      tm <- transformation_matrix()
      
      # Prepare data for Sankey
      sources <- c(rep(0:3, each = nrow(tm)))  # R, E, P, S
      targets <- c(rep(4:(3 + nrow(tm)), 4))   # Metrics
      
      values <- c()
      for (component in c("R", "E", "P", "S")) {
        col_name <- paste0(component, "_component")
        values <- c(values, tm[[col_name]] * comp[component])
      }
      
      labels <- c("R", "E", "P", "S", tm$metric)
      
      plot_ly(
        type = "sankey",
        node = list(
          label = labels,
          color = c("#e74c3c", "#3498db", "#27ae60", "#f39c12", 
                   rep("#95a5a6", nrow(tm))),
          pad = 15,
          thickness = 20
        ),
        link = list(
          source = sources,
          target = targets,
          value = values,
          color = "rgba(0,0,0,0.2)"
        )
      ) %>%
        layout(
          title = "Information Flow from Components to Metrics"
        )
    })
    
    # 3D response surface
    output$response_surface <- renderPlotly({
      req(input$surface_metric)
      metric_idx <- which(transformation_matrix()$metric == tolower(input$surface_metric))
      
      if (length(metric_idx) == 0 || length(metric_idx) > 1) {
        metric_idx <- metric_idx[1]  # Take first match if multiple
      }
      if (length(metric_idx) == 0) return(NULL)
      
      tm <- transformation_matrix()
      
      # Create grid
      r_seq <- seq(0, 1, length.out = 20)
      e_seq <- seq(0, 1, length.out = 20)
      
      z_matrix <- outer(r_seq, e_seq, function(r, e) {
        # Fix P and S at current values
        p_fixed <- current_components()["P"]
        s_fixed <- current_components()["S"]
        
        # Normalize if needed
        if (input$normalize_comps) {
          total <- r + e + p_fixed + s_fixed
          if (total > 0) {
            r <- r / total
            e <- e / total
            p_fixed <- p_fixed / total
            s_fixed <- s_fixed / total
          }
        }
        
        # Calculate metric value
        tm$R_component[metric_idx] * r +
        tm$E_component[metric_idx] * e +
        tm$P_component[metric_idx] * p_fixed +
        tm$S_component[metric_idx] * s_fixed +
        tm$intercept[metric_idx]
      })
      
      plot_ly(
        x = r_seq,
        y = e_seq,
        z = z_matrix,
        type = "surface",
        colorscale = "Viridis"
      ) %>%
        layout(
          title = paste(input$surface_metric, "Response Surface"),
          scene = list(
            xaxis = list(title = "Richness (R)"),
            yaxis = list(title = "Evenness (E)"),
            zaxis = list(title = input$surface_metric)
          )
        )
    })
    
    # Network graph
    output$network_graph <- renderPlotly({
      comp <- current_components()
      tm <- transformation_matrix()
      
      # Create nodes
      component_nodes <- data.frame(
        name = c("R", "E", "P", "S"),
        x = c(0, 0, 0, 0),
        y = c(3, 2, 1, 0),
        color = c("#e74c3c", "#3498db", "#27ae60", "#f39c12"),
        size = comp * 30 + 10
      )
      
      metric_nodes <- data.frame(
        name = tm$metric,
        x = rep(2, nrow(tm)),
        y = seq(3.5, -0.5, length.out = nrow(tm)),
        color = "#95a5a6",
        size = 20
      )
      
      all_nodes <- rbind(component_nodes, metric_nodes)
      
      # Create edges (only show significant connections)
      edges <- list()
      for (i in 1:4) {
        component <- c("R", "E", "P", "S")[i]
        col_name <- paste0(component, "_component")
        
        # Check if column exists
        if (col_name %in% colnames(tm)) {
          for (j in 1:nrow(tm)) {
            # Ensure value exists and is not NA
            if (!is.na(tm[[col_name]][j]) && tm[[col_name]][j] > 0.1) {  # Only show significant connections
              edges[[length(edges) + 1]] <- list(
                x = c(0, 2),
                y = c(component_nodes$y[i], metric_nodes$y[j]),
                width = tm[[col_name]][j] * 10
              )
            }
          }
        }
      }
      
      # Plot nodes
      p <- plot_ly() %>%
        add_trace(
          data = all_nodes,
          x = ~x,
          y = ~y,
          type = "scatter",
          mode = "markers+text",
          text = ~name,
          textposition = "middle center",
          marker = list(
            size = ~size,
            color = ~color
          ),
          hovertemplate = "%{text}<extra></extra>"
        )
      
      # Add edges
      for (edge in edges) {
        p <- p %>%
          add_trace(
            x = edge$x,
            y = edge$y,
            type = "scatter",
            mode = "lines",
            line = list(width = edge$width, color = "rgba(0,0,0,0.2)"),
            showlegend = FALSE,
            hoverinfo = "none"
          )
      }
      
      p %>%
        layout(
          title = "Component-Metric Network",
          showlegend = FALSE,
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
        )
    })
    
    # Uncertainty plot
    output$uncertainty_plot <- renderPlotly({
      metrics <- predicted_metrics()
      
      # Simulate uncertainty (in real app, this would come from the model)
      metric_names <- names(metrics)
      values <- unlist(metrics)
      
      # Create confidence intervals (simulated)
      lower <- values * 0.9
      upper <- values * 1.1
      
      plot_ly() %>%
        add_trace(
          x = metric_names,
          y = values,
          type = "scatter",
          mode = "markers",
          marker = list(size = 10, color = "#3498db"),
          name = "Predicted",
          error_y = list(
            type = "data",
            symmetric = FALSE,
            array = upper - values,
            arrayminus = values - lower,
            color = "#3498db"
          )
        ) %>%
        layout(
          title = "Prediction Confidence Intervals",
          xaxis = list(title = "Metric"),
          yaxis = list(title = "Value"),
          showlegend = FALSE
        )
    })
    
    # Key insights
    output$dominant_components <- renderUI({
      comp <- current_components()
      sorted_comp <- sort(comp, decreasing = TRUE)
      
      tagList(
        lapply(1:2, function(i) {
          name <- names(sorted_comp)[i]
          value <- sorted_comp[i]
          color <- c(R = "#e74c3c", E = "#3498db", P = "#27ae60", S = "#f39c12")[name]
          
          div(
            style = paste0("color: ", color, ";"),
            strong(name), ": ", sprintf("%.1f%%", value * 100)
          )
        })
      )
    })
    
    output$metric_relationships <- renderUI({
      metrics <- predicted_metrics()
      
      # Find correlated metrics (simplified)
      if (!is.null(metrics$shannon) && !is.null(metrics$simpson) &&
          !is.na(metrics$shannon) && !is.na(metrics$simpson)) {
        if (metrics$shannon > 2 && metrics$simpson > 0.7) {
          p("High diversity detected across multiple metrics")
        } else if (metrics$shannon < 1 && metrics$simpson < 0.5) {
          p("Low diversity - dominated system")
        } else {
          p("Moderate diversity levels")
        }
      } else {
        p("Calculating metric relationships...")
      }
    })
    
    output$ecological_state <- renderUI({
      comp <- current_components()
      
      if (comp["R"] > 0.6) {
        state <- "Pioneer/Colonization"
        color <- "#27ae60"
      } else if (comp["E"] > 0.6) {
        state <- "Mature/Stable"
        color <- "#3498db"
      } else if (comp["P"] > 0.4) {
        state = "Ancient/Established"
        color <- "#9b59b6"
      } else {
        state <- "Transitional"
        color <- "#f39c12"
      }
      
      h4(state, style = paste0("color: ", color, ";"))
    })
    
    # Scenario buttons
    observeEvent(input$scenario_pioneer, {
      updateSliderInput(session, "R_comp", value = 0.7)
      updateSliderInput(session, "E_comp", value = 0.2)
      updateSliderInput(session, "P_comp", value = 0.08)
      updateSliderInput(session, "S_comp", value = 0.02)
    })
    
    observeEvent(input$scenario_mature, {
      updateSliderInput(session, "R_comp", value = 0.3)
      updateSliderInput(session, "E_comp", value = 0.6)
      updateSliderInput(session, "P_comp", value = 0.08)
      updateSliderInput(session, "S_comp", value = 0.02)
    })
    
    observeEvent(input$scenario_stressed, {
      updateSliderInput(session, "R_comp", value = 0.2)
      updateSliderInput(session, "E_comp", value = 0.1)
      updateSliderInput(session, "P_comp", value = 0.6)
      updateSliderInput(session, "S_comp", value = 0.1)
    })
    
    observeEvent(input$scenario_bloom, {
      updateSliderInput(session, "R_comp", value = 0.05)
      updateSliderInput(session, "E_comp", value = 0.05)
      updateSliderInput(session, "P_comp", value = 0.05)
      updateSliderInput(session, "S_comp", value = 0.85)
    })
    
    observeEvent(input$reset_components, {
      updateSliderInput(session, "R_comp", value = 0.5)
      updateSliderInput(session, "E_comp", value = 0.5)
      updateSliderInput(session, "P_comp", value = 0.2)
      updateSliderInput(session, "S_comp", value = 0.1)
    })
    
    # Prediction accuracy table
    output$prediction_accuracy <- renderDataTable({
      if (!is.null(universal_info()) && !is.null(universal_info()$actual_metrics)) {
        # Get actual metrics from the universal_info
        actual <- universal_info()$actual_metrics
        predicted <- predicted_metrics()
        
        # Create comparison table
        comparison <- data.frame(
          Metric = names(predicted),
          Predicted = round(unlist(predicted), 3),
          Actual = NA,
          Error = NA,
          stringsAsFactors = FALSE
        )
        
        # Match actual values
        for (i in 1:nrow(comparison)) {
          metric_name <- comparison$Metric[i]
          if (metric_name %in% names(actual)) {
            comparison$Actual[i] <- round(actual[[metric_name]], 3)
            comparison$Error[i] <- round(abs(comparison$Predicted[i] - comparison$Actual[i]), 3)
          }
        }
        
        # Remove rows with NA actual values
        comparison <- comparison[!is.na(comparison$Actual), ]
        
        datatable(comparison, 
                  options = list(
                    pageLength = 5,
                    dom = 't',
                    ordering = FALSE
                  ),
                  rownames = FALSE) %>%
          formatStyle('Error',
                      backgroundColor = styleInterval(c(0.1, 0.5), 
                                                    c('lightgreen', 'lightyellow', 'lightcoral')))
      } else {
        # Return empty table
        data.frame(
          Note = "Load data with universal analysis to see predictions vs actual values"
        )
      }
    })
    
    # Output for conditional panel
    output$has_real_data <- reactive({
      !is.null(universal_info()) && !is.null(universal_info()$transformation_matrix)
    })
    outputOptions(output, "has_real_data", suspendWhenHidden = FALSE)
  })
}