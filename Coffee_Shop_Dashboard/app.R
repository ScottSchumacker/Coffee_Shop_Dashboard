# Scott Schumacker
# Coffee Shop Dashboard

library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(ggplot2)
library(plotly)
library(readr)

# Define UI for application that draws a histogram
ui <- page_sidebar(
  # App title ----
  title = "Coffee Sales",
  # Sidebar panel for inputs ----
  sidebar = sidebar(
  ),
  
  layout_columns(
    fill = FALSE,
    value_box(
      title = "Total Revenue",
      value = textOutput("total")
    ),
    value_box(
      title = "Total Sales",
      value = textOutput("consume2023")
    ),
    value_box(
      title = "Most Popular Item",
      value = textOutput("change")
    )
  ),
  card(),
)

# Define server logic required to draw a histogram
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
