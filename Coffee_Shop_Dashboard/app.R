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
      value = textOutput("mostPopular")
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
  
  ##########
  
  # Creating data set
  # Number of transactions
  set.seed(487)
  n_transactions <- 1000
  
  # Dates (for a month)
  start_date <- as.Date("2023-10-01")
  dates <- sample(seq(start_date, start_date + 30, by = "day"), n_transactions, replace = TRUE)
  
  # Time of day (morning, afternoon, evening)
  times <- sample(c("Morning", "Afternoon", "Evening"), n_transactions, replace = TRUE)
  
  # Coffee shop items
  items <- sample(c("Latte", "Cappuccino", "Americano", "Mocha", "Espresso", "Iced Coffee", "Pastry", "Muffin", "Sandwich", "Tea"), n_transactions, replace = TRUE)
  
  # Prices (adjust as needed)
  prices <- numeric(n_transactions)
  prices[items == "Latte"] <- round(runif(sum(items == "Latte"), 3.5, 5.5), 2)
  prices[items == "Cappuccino"] <- round(runif(sum(items == "Cappuccino"), 3.5, 5.5), 2)
  prices[items == "Americano"] <- round(runif(sum(items == "Americano"), 2.5, 4.5), 2)
  prices[items == "Mocha"] <- round(runif(sum(items == "Mocha"), 4, 6), 2)
  prices[items == "Espresso"] <- round(runif(sum(items == "Espresso"), 2, 3.5), 2)
  prices[items == "Iced Coffee"] <- round(runif(sum(items == "Iced Coffee"), 3, 5), 2)
  prices[items == "Pastry"] <- round(runif(sum(items == "Pastry"), 2, 4), 2)
  prices[items == "Muffin"] <- round(runif(sum(items == "Muffin"), 2.5, 4.5), 2)
  prices[items == "Sandwich"] <- round(runif(sum(items == "Sandwich"), 5, 8), 2)
  prices[items == "Tea"] <- round(runif(sum(items == "Tea"), 2, 4), 2)
  
  # Customer ID (optional, for repeat customers)
  customer_ids <- sample(1:200, n_transactions, replace = TRUE)
  
  # Create the data frame
  coffee_sales <- data.frame(
    TransactionID = 1:n_transactions,
    Date = dates,
    Time = times,
    ItemID = items,
    Price = prices,
    CustomerID = customer_ids
  )
  
  ##########
  
  # Creating popular item
  item_count <- coffee_sales %>% 
    group_by(ItemID) %>% 
    summarise(itemCount = n())
  
  maxItem <- subset(item_count, itemCount == max(item_count$itemCount),
                    select = ItemID)
  
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
  
  output$mostPopular <- renderText({
    maxItem[[1]]
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
