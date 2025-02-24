# Scott Schumacker
# Coffee Shop Dashboard

# Loading libraries
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
  title = "Sales Dashboard",
  # Sidebar panel for inputs ----
  sidebar = sidebar(
    selectInput( 
      "select", 
      "Select options below:", 
      list("October" = "1A", "September" = "1B", "August" = "1C") 
    ),
  ),
  
  layout_columns(
    fill = FALSE,
    value_box(
      title = "Total Revenue",
      value = textOutput("totalRevVal")
    ),
    value_box(
      title = "Total Sales",
      value = textOutput("totalSalesNum")
    ),
    value_box(
      title = "Most Popular Item",
      value = textOutput("change")
    )
  ),
  card(
    plotlyOutput("totalRevPlot")
  ),
  card(
    plotlyOutput("totalNumberSales")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Creating total revenue data frame
  total_revenueDF <- coffee_sales %>% 
    group_by(Date) %>% 
    summarise(total_rev = sum(Price))
  
  # Creating total number of sales data frame
  number_sales <- coffee_sales %>% 
    group_by(Date) %>% 
    summarise(count = n())
  
  totalRevNumber <- sum(total_revenueDF$total_rev)
  
  totalSalesNumber <- sum(number_sales$count)
  
  # Creating KPI metrics
  output$totalRevVal <- renderText({
    paste0("$ ", round(totalRevNumber,2))
  })
  
  output$totalSalesNum <- renderText({
    totalSalesNumber
  })
  
  # Creating total revenue plot
  output$totalRevPlot <- renderPlotly({
    ggplotly(
      ggplot(total_revenueDF, aes(Date, total_rev)) +
        geom_point() +
        geom_line() +
        theme_minimal() +
        xlab("Date") +
        ylab("Total Sales Revenue ($)") +
        ggtitle("Total Sales Revenue")
    )
  })
  
  # Creating total number of sales plot
  output$totalNumberSales <- renderPlotly({
    ggplotly(
      ggplot(number_sales, aes(Date, count)) +
        geom_point() +
        geom_line() +
        theme_minimal() +
        xlab("Date") +
        ylab("Number of Sales") +
        ggtitle("Number of Sales")
    )
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
