# Title: Text Analysis of Harry Potter Books
# Description: visualizes results from a text analysis on J.K. Rowling's Harry Potter books
# Details: sentiment analysis, word frequency analysis
# Author: Jeffrey Ding
# Date: 11-15-2024


# ===============================================
# Packages
# ===============================================
library(shiny)
library(tidyverse)  # for data manipulation and graphics
library(tidytext)   # for text mining
library(DT)         # to render Data Tables nicely
library(plotly)     # if you are interested in using ggplotly()


# ===============================================
# Import data
# ===============================================
hp_books = read_csv("harry_potter_books.csv", col_types = "ccc")
nrc <- read_csv("nrc.csv")

book_names = unique(hp_books$book)

tbl_tokens = hp_books |> 
  unnest_tokens(word, text)


# ===============================================
# Define "ui" for application
# ===============================================

ui <- fluidPage(
  
  # Application title
  titlePanel("Text Analysis of Harry Potter Books"),
  
  # -------------------------------------------------------
  # Input widgets 
  # -------------------------------------------------------
  fluidRow(
    # widgets of column 1
    column(4,
           p(em("Analysis 1 & 2")),
           selectInput(inputId = "selected_book", 
                       label = "Select a book", 
                       choices = c(book_names, "All books"),
                       selected = book_names[1])
    ), # closes column 1
    
    # widgets of column 2
    column(2,
           p(em("Analysis 1 & 2")),
           checkboxInput(inputId = "remove_stopwords", 
                         label = "Remove stopwords", 
                         value = FALSE)
    ), # closes column 2
    
    # widgets of column 3
    column(3,
           p(em("Analysis 1")),
           radioButtons(inputId = "sort_order", 
                        label = "Sort bars by:",
                        choices = c("Alphabetical" = "alpha",
                                    "Proportion (decreasing)" = "desc",
                                    "Proportion (increasing)" = "asc"),
                        selected = "alpha")
    ), # closes column 3
    
    # widgets of column 4
    column(3,
           p(em("Analysis 2")),
           sliderInput(inputId = "top_n", 
                       label = "Top n words",
                       min = 1,
                       max = 50,
                       value = 10)
    ), # closes column 4
    
    # widgets of column 5
    column(3,
           p(em("Analysis 2")),
           selectInput(inputId = "selected_sentiment", 
                       label = "Select Sentiment", 
                       choices = unique(nrc$sentiment),
                       selected = "positive")
    ) # closes column 5
    
  ), # closes fluidRow
  
  hr(), # horizontal rule
  
  
  # -------------------------------------------------------
  # Tabset Panel of outputs
  # -------------------------------------------------------
  tabsetPanel(type = "tabs",
              # First tab
              tabPanel(title = "Analysis 1",
                       h3("Word Sentiment Analysis"),
                       plotlyOutput("plot1"),
                       hr(),
                       dataTableOutput("table1")),
              # Second tab
              tabPanel(title = "Analysis 2", 
                       h3("Word Frequency Analysis by Associated Sentiment"),
                       plotlyOutput("plot2"),
                       hr(),
                       dataTableOutput("table2"))
  ) # closes tabsetPanel
  
) # closes ui


# ===============================================
# Define Server "server" logic
# ===============================================

server <- function(input, output) {
  
  # ===============================================
  # Reactive conductor table for Tabs 1 & 2
  # ===============================================
  # reactive conductor
  selected_tokens <- reactive({
    # Removes stopwords (e.g. a, the, of)
    if (input$remove_stopwords) {
      selected_tokens = tbl_tokens |>
        anti_join(stop_words, by = "word")
    } else {
      selected_tokens = tbl_tokens
    }
    
    # Selects books to be analyzed based on user input
    if (input$selected_book != "All books") {
      selected_tokens = selected_tokens |>
        filter(book == input$selected_book)
    }
    
    selected_tokens
  })
  
  
  # ===============================================
  # Outputs for TAB-1: Word Sentiment Analysis
  # ===============================================
  
  # auxiliary reactive conductor for Tab 1
  tbl_nrc_sentiment <- reactive({
    sentiment_counts <- selected_tokens() |>
      inner_join(nrc, by = "word") |>
      count(sentiment, name = "count")
    
    total_count <- sum(sentiment_counts$count)
    
    sentiment_counts <- sentiment_counts |>
      mutate(proportion = count / total_count)
  })
  
  
  # plot1: Bar chart of sentiment proportions
  output$plot1 <- renderPlotly({
    
    # User input determines the order sentiments are displayed in
    if (input$sort_order == "alpha") {
      p1 <- tbl_nrc_sentiment() |>
        ggplot(aes(x = sentiment, 
                   y = proportion, fill = sentiment))
    } else if (input$sort_order == "desc") {
      p1 <- tbl_nrc_sentiment() |>
        ggplot(aes(x = reorder(sentiment, -proportion), 
                   y = proportion, fill = sentiment))
    } else if (input$sort_order == "asc") {
      p1 <- tbl_nrc_sentiment() |>
        ggplot(aes(x = reorder(sentiment, +proportion), 
                   y = proportion, fill = sentiment))
    }
    
    p1 <- p1 + geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = paste("Proportion of Words by Associated Sentiments"),
           x = "Sentiment",
           y = "Proportion") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p1)
  })
  
  
  # table1: word frequencies
  output$table1 <- renderDataTable({
    tbl_nrc_sentiment()
  })
  
  
  
  # ===============================================
  # Outputs for TAB-2: Word Frequency Analysis
  # ===============================================
  
  # auxiliary reactive table for Tab 2
  top_n_sentiments <- reactive({
    # Count words, filter by sentiment, and select top n rows
    selected_tokens() |>
      inner_join(nrc, by = "word") |> 
      filter(sentiment == input$selected_sentiment) |>
      count(word, sentiment, name = "count", sort = TRUE) |>
      slice_head(n = input$top_n)
  })

  
  # plot2: Bar chart of word frequencies for selected book and sentiment
  output$plot2 <- renderPlotly({

      p2 <- top_n_sentiments() |>
      ggplot(mapping = aes(x = reorder(word, -count), 
                           y = count)) +
      geom_bar(stat = "identity",
               fill = "skyblue") + 
      facet_wrap(~ sentiment, 
                 scales = "free_y") + 
      theme_minimal() +
      labs(title = paste(input$top_n, "Most Common Words Associated with:", input$selected_sentiment),
           x = "Word", 
           y = "Frequency") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none") 
    
    ggplotly(p2)
  })
  
  
  # table2: word freqs and their sentiments
  output$table2 <- renderDataTable({
    top_n_sentiments() |>
      select(word, count)
  })
  
}



# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

