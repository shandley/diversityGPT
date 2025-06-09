# diversityGPT Interactive Explorer
# Shiny application for real-time diversity metric transformation and exploration

# Load required libraries
library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(diversityGPT)
library(phyloseq)

# Load example datasets
data(GlobalPatterns)
data(enterotype)
data(soilrep)

# Check if pre-loaded example is set
if (!is.null(getOption("diversityGPT.example"))) {
  message("Pre-loading example dataset: ", getOption("diversityGPT.example"))
}

# Source UI and server
ui <- source("ui.R", local = TRUE)$value
server <- source("server.R", local = TRUE)$value

# Run the application
shinyApp(ui = ui, server = server)