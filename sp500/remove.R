# Title: S&P 500 Data
# Description: visualizes historical closing values and multi-year returns of the S&P 500 market index
# Author: Jeffrey Ding
# Date: 11-1-2024


# =======================================================
# Packages 
# =======================================================
library(shiny)
library(tidyverse)  # for data manipulation and graphics
library(lubridate)  # for working with dates
library(tidyquant)  # financial & quant functions
library(plotly)     # for web interactive graphics


# ======================================================
# Auxiliary objects/functions
# ======================================================

sp500 = tq_get("^GSPC", from = "1928-01-01", to = "2023-12-31")

calculate_returns = function(data, range) {
  filtered_data = data |>
    select(date, close) |>
    mutate(year = year(date))
  
  results = tibble(start_year = numeric(), return = numeric())
  
  for (i in min(filtered_data$year):(max(filtered_data$year) - range + 1)) {
    period_return = filtered_data |>
      filter(year %in% i:(i + range - 1)) |>
      summarise(return = (last(close) - first(close)) / first(close)) |>
      pull(return)
    
    results = results |>
      add_row(start_year = i, return = period_return)
  }
  
  return(results)
}


# =======================================================
# Define UI for application
# =======================================================
ui <- fluidPage(
  
  # Application title
  titlePanel("S&P 500 Market Index"),
  
  # -------------------------------------------------------
  # Sidebar with input widgets
  # -------------------------------------------------------
  sidebarLayout(
    sidebarPanel(
      # inputs
      sliderInput(inputId = "time_period",
                  label = "Time Period:",
                  min = 1928,
                  max = 2023,
                  value = c(1950, 2000),
                  step = 1),
      radioButtons(inputId = "scale",
                   label = "Scale of Closing Values Timeline:",
                   choices = c("Linear", "Logarithmic" = "log"),
                   selected = "Linear"),
      numericInput(inputId = "years",
                   label = "Number of Years for Multi-Year Returns:",
                   value = 3,
                   min = 1),
      checkboxGroupInput(inputId = "stats",
                   label = "Summary Statistics of Returns:",
                   choices = c("Mean Return" = "mean", "Median Return" = "median", "Standard Deviation" = "sd"),
                   selected = NULL),
    ),  # closes sidebarPanel of inputs
    
    # -------------------------------------------------------
    # Main Panel with outputs: plots and table
    # -------------------------------------------------------
    mainPanel(
      h3("Daily Closing Values of S&P 500"),
      plotlyOutput(outputId = "plot1"),
      hr(),
      h3("Multi-Year Returns of S&P 500"),
      plotlyOutput(outputId = "plot2"),
      hr(),
      h3("Summary Statistics of Multi-Year Returns"),
      tableOutput(outputId = "table"),
    ) # closes mainPanel of outputs
    
  ) # closes sidebarLayout
) # closes fluidPage (UI)


# ======================================================
# Define server logic
# ======================================================
server <- function(input, output) {
  
  # ------------------------------------------------------------
  # Auxiliary table (note that this is a reactive conductor)
  # ------------------------------------------------------------
  sp500_data = reactive({
    sp500 |>
      filter(year(date) >= input$time_period[1] & year(date) <= input$time_period[2])
  })
  
  multi_year_data = reactive({
    calculate_returns(sp500_data(), input$years)
  })
  
  # ------------------------------------------------------------
  # Plot (timeline of daily closing values)
  # ------------------------------------------------------------
  output$plot1 <- renderPlotly({
    y_label <- if (input$scale == "Linear") "Closing Value ($)" else "Log10 of Closing Value"
    
    plot1 = ggplot(data = sp500_data(),
                   mapping = aes(x = date, y = close)) +
      geom_line(color = "#11AA66") +
      labs(title = paste("S&P 500 (", input$time_period[1], "-", input$time_period[2], ")"),
           x = "Date",
           y = y_label) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust=1))
    
    if (input$scale == "log") {
      plot1 = plot1 + scale_y_log10()
    }
    
    ggplotly(plot1)
  })


  # ------------------------------------------------------------
  # Plot (bar-chart of multi-year returns)
  # ------------------------------------------------------------
  output$plot2 <- renderPlotly({
    plot2 = ggplot(data = multi_year_data(),
                   mapping = aes(x = start_year, y = return)) +
      geom_col(fill = "#11AA66") +
      labs(title = paste(input$years, "- Year Returns for S&P 500 (", input$time_period[1], "-", input$time_period[2], ")"),
           x = paste("Starting Year of", input$years, "Year Period"),
           y = "Percent Return") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust=1))
    
    if ("mean" %in% input$stats) {
      mean_return = mean(multi_year_data()$return)
      plot2 = plot2 + geom_hline(yintercept = mean_return, color = "mediumblue") +
        annotate(geom = "text", x = min(multi_year_data()$start_year), y = mean_return + 0.05, label = "Mean", 
                 vjust = -5, color = "mediumblue")
    }
    if ("median" %in% input$stats) {
      median_return = median(multi_year_data()$return)
      plot2 = plot2 + geom_hline(yintercept = median_return, color = "red") +
        annotate(geom = "text", x = max(multi_year_data()$start_year), y = median_return + 0.05, label = "Median",
                 vjust = -5, color = "red")
    }
    if ("sd" %in% input$stats) {
      sd_return = sd(multi_year_data()$return)
      plot2 = plot2 + geom_hline(yintercept = mean_return + sd_return, linetype = "dashed", color = "mediumblue") +
        annotate(geom = "text", x = min(multi_year_data()$start_year) + 2.5, y = mean_return + sd_return + 0.05, 
                 label = "Mean + SD", vjust = -5, color = "mediumblue") + 
        geom_hline(yintercept = mean_return - sd_return, linetype = "dashed", color = "mediumblue") +
        annotate(geom = "text", x = min(multi_year_data()$start_year) + 2.5, y = mean_return - sd_return + 0.05, 
                 label = "Mean - SD", vjust = -5, color = "mediumblue")
    }
    
    ggplotly(plot2)
  })
  
    
  # ------------------------------------------------------------
  # Table
  # ------------------------------------------------------------
  output$table <- renderTable({
    summary_table <- tibble(
      Statistic = c("Mean", "Standard Deviation", "10th Percentile", "25th Percentile", "Median", "75th Percentile",
                    "90th Percentile", "IQR"),
      Value = c(mean(multi_year_data()$return, na.rm = TRUE),
                sd(multi_year_data()$return, na.rm = TRUE),
                quantile(multi_year_data()$return, 0.1, na.rm = TRUE),
                quantile(multi_year_data()$return, 0.25, na.rm = TRUE),
                median(multi_year_data()$return, na.rm = TRUE),
                quantile(multi_year_data()$return, 0.75, na.rm = TRUE),
                quantile(multi_year_data()$return, 0.9, na.rm = TRUE),
                IQR(multi_year_data()$return, na.rm = TRUE))
    )
    
    summary_table
  })
 
} # closes server


# Run the application 
shinyApp(ui = ui, server = server)
