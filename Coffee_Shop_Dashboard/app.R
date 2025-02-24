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
  card(
    plotlyOutput("totalRevPlot")
  ),
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Creating total revenue data frame
  total_revenueDF <- coffee_sales %>% 
    group_by(Date) %>% 
    summarise(total_rev = sum(Price))
  
  # Creating total revenue plot
  output$totalRevPlot <- renderPlotly({
    ggplotly(
      ggplot(total_revenueDF, aes(Date, total_rev)) +
        geom_point() +
        geom_line() +
        theme_minimal() +
        xlab("Date") +
        ylab("Total Sales Revenue ($)")
    )
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
