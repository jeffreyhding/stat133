# Title: Car Accidents in California
# Description: graphical analysis and interactive map of car accidents in California from 2019 to 2021
# Author: Jeffrey Ding
# Date: 12-6-2024


# =======================================================
# Packages (you can use other packages if you want)
# =======================================================
library(shiny)
library(plotly)       # web interactive graphics
library(tidyverse)    # data wrangling and graphics
library(lubridate)    # for working with dates
library(leaflet)      # web interactive maps

# =======================================================
# Import data
# =======================================================
crashes = read_csv(
  file = "crashes_california_2019_2021.csv",
  col_types = list(
    col_double(),    #  1) CASE_ID
    col_double(),    #  2) ACCIDENT_YEAR
    col_date(),      #  3) COLLISION_DATE
    col_double(),    #  4) COLLISION_TIME
    col_double(),    #  5) HOUR
    col_integer(),   #  6) DAY_OF_WEEK
    col_character(), #  7) WEATHER_1
    col_character(), #  8) WEATHER_2
    col_character(), #  9) STATE_HWY_IND
    col_character(), # 10) COLLISION_SEVERITY
    col_integer(),   # 11) NUMBER_KILLED 
    col_integer(),   # 12) NUMBER_INJURED
    col_integer(),   # 13) PARTY_COUNT
    col_character(), # 14) PCF_VIOL_CATEGORY
    col_character(), # 15) TYPE_OF_COLLISION
    col_character(), # 16) ROAD_SURFACE
    col_character(), # 17) ROAD_COND_1
    col_character(), # 18) ROAD_COND_2
    col_character(), # 19) LIGHTING
    col_character(), # 20) PEDESTRIAN_ACCIDENT
    col_character(), # 21) BICYCLE_ACCIDENT
    col_character(), # 22) MOTORCYCLE_ACCIDENT
    col_character(), # 23) TRUCK_ACCIDENT
    col_character(), # 24) NOT_PRIVATE_PROPERTY
    col_character(), # 25) ALCOHOL_INVOLVED
    col_character(), # 26) COUNTY
    col_character(), # 27) CITY
    col_character(), # 28) PO_NAME
    col_double(),    # 29) ZIP_CODE
    col_double(),    # 30) POINT_X
    col_double()     # 31) POINT_Y
  )) |>
  sample_n(20000)

crashes = crashes |>
  # Timeline plot variables
  mutate(ACCIDENT_MONTH_YEAR = format(COLLISION_DATE, "%Y-%m")) |>
  mutate(ACCIDENT_QUARTER = case_when(
    month(COLLISION_DATE) %in% c(1, 2, 3) ~ 1,
    month(COLLISION_DATE) %in% c(4, 5, 6) ~ 2,
    month(COLLISION_DATE) %in% c(7, 8, 9) ~ 3,
    month(COLLISION_DATE) %in% c(10, 11, 12) ~ 4)) |>
  mutate(ACCIDENT_QUARTER_YEAR = paste0(ACCIDENT_YEAR, "-Q", ACCIDENT_QUARTER)) |>
  
  # Time of day plot variable
  mutate(COLLISION_HOUR = COLLISION_TIME %/% 100) |>
  
  # Condensed variable for vehicles involved
  mutate(VEHICLES = case_when(
    PEDESTRIAN_ACCIDENT == "yes" ~ "Pedestrian-Car",
    BICYCLE_ACCIDENT == "yes" ~ "Bicycle-Car",
    MOTORCYCLE_ACCIDENT == "yes" ~ "Motorcycle-Car",
    TRUCK_ACCIDENT == "yes" ~ "Truck-Car",
    TRUE ~ "Car-Car/Other"))

  # Palettes
  vehicles = c("Car-Car/Other", "Truck-Car", "Motorcycle-Car", "Bicycle-Car", "Pedestrian-Car")
  severity = c("possible injury", "minor injury", "severe injury", "fatal injury")
  alcohol = c("yes", "no")
  weather = c("clear", "cloudy", "fog", "raining", "snowing", "wind", "other", "unknown")

  palettes = list(
    "VEHICLES" = colorFactor(
      palette = c("skyblue", "coral", "gold", "seagreen3", "violet"),
      domain = factor(vehicles, levels = vehicles)
      ),
    "COLLISION_SEVERITY" = colorFactor(
      palette = c("green3", "gold", "orange", "red3"),
      domain = factor(severity, levels = severity)
      ),
    "ALCOHOL_INVOLVED" = colorFactor(
      palette = c("skyblue", "salmon"),
      domain = factor(alcohol, levels = alcohol)
      ),
    "WEATHER_1" = colorFactor(
      c("skyblue", "gray60", "turquoise4", "slateblue", "snow3", "aquamarine", "coral3", "coral4"),
      domain = factor(weather, levels = weather)))
  

# ===============================================
# Define "ui" for application
# ===============================================
ui <- fluidPage(
  
  # Application title
  titlePanel("Car Accidents in California"),
  
  # -------------------------------------------------------
  # Input widgets 
  # -------------------------------------------------------
  sidebarLayout(
    sidebarPanel(
      # ---------------------------------------------
      # Tab 1: Input Widgets
      # ---------------------------------------------
      conditionalPanel(
        condition = "input.tabselected==1",
        h4("Exploratory Analysis"),
        
        # Plot 1, 2, 3
        sliderInput(inputId = "time_period",
                    label = "Select time period",
                    min = 2019,
                    max = 2021,
                    value = c(2019, 2021),
                    step = 1),
        
        # Plot 1, 2, 3
        selectInput(inputId = "time_stat",
                    label = "Select statistic",
                    choices = c("All accidents" = "All",
                                "Car/Other accidents" = "Car-Car/Other",
                                "Pedestrian accidents" = "Pedestrian-Car",
                                "Bicycle accidents" = "Bicycle-Car",
                                "Motocycle accidents" = "Motorcycle-Car",
                                "Truck accidents" = "Truck-Car",
                                "Deaths" = "Killed",
                                "Injuries" = "Injured"),
                    selected = "all"),
        
        # Plot 1
        radioButtons(inputId = "time_scale",
                     label = "Select timeline scale",
                     choices = c("Year" = "ACCIDENT_YEAR",
                                 "Quarter-Year" = "ACCIDENT_QUARTER_YEAR",
                                 "Month-Year" = "ACCIDENT_MONTH_YEAR"),
                     selected = "ACCIDENT_YEAR"),
        
        # Plot 1, 2
        p(strong("Overlay")),
        checkboxInput(inputId = "alcohol",
                      label = HTML("Alcohol involvement <span style='color:salmon;'>&#9632;</span>"),
                      value = FALSE)
      ), # closes 1st panel
      
      # ---------------------------------------------
      # Tab 2: Input Widgets
      # ---------------------------------------------
      conditionalPanel(
        condition = "input.tabselected==2",
        h4("Map"),
        
        sliderInput(inputId = "time_period",
                    label = "Select time period",
                    min = 2019,
                    max = 2021,
                    value = c(2019, 2021),
                    step = 1),
        
        # Filter by county
        selectInput(inputId = "county",
                    label = "Select county",
                    choices = c("All counties" = "All", sort(unique(crashes$COUNTY))),
                    selected = "All"),
        
        # Color code points
        radioButtons(inputId = "color",
                     label = "Color by:",
                     choices = c("None",
                                 "Vehicles involved" = "VEHICLES",
                                 "Collision severity" = "COLLISION_SEVERITY",
                                 "Alcohol involvement" = "ALCOHOL_INVOLVED",
                                 "Weather" = "WEATHER_1"),
                     selected = "None"),
      ) # closes 2nd panel
    ), # closes sidebarPanel
    
    
    # -------------------------------------------------------
    # Main Panel with 2 Tabs: 
    # Tab 1: Exploratory Analysis
    # Tab 2: Map of Crashes
    # -------------------------------------------------------
    mainPanel(
      tabsetPanel(
        type = "tabs",
        # Tab 1
        tabPanel(title = "Explore",
                 value = 1,
                 plotlyOutput(outputId = "plot1"),
                 hr(),
                 plotlyOutput(outputId = "plot2"),
                 hr(),
                 plotlyOutput(outputId = "plot3")),
        
        # Tab 2
        tabPanel(title = "Map",
                 value = 2,
                 leafletOutput("map", height = 600)),
        
        # Selected tab
        id = "tabselected"
      ) # closes tabsetPanel
    ) # closes mainPanel
    
  ) # closes sidebarLayout
) # closes fluidPage (UI)



# ===============================================
# Define server logic
# ===============================================
server <- function(input, output) {
  # ------------------------------------------------
  # Auxiliary table
  # ------------------------------------------------
  crashes_data = reactive({
    # Filter for Time Period
    crashes_data = crashes |>
      filter(ACCIDENT_YEAR >= input$time_period[1] & ACCIDENT_YEAR <= input$time_period[2])
    
    # Filter for Timeline Statistic (Tab 1)
    if (input$time_stat %in% unique(crashes$VEHICLES)) {
      crashes_data = crashes_data |>
        filter(VEHICLES == input$time_stat)
    }
    
    # Filter for Alcohol Involvement (Tab 1)
    alcohol_data = crashes_data |>
      filter(ALCOHOL_INVOLVED == "yes")
    
    list(all = crashes_data,
         alcohol = alcohol_data)
  })
  
  
  # ------------------------------------------------
  # Tab 1: Output
  # ------------------------------------------------
  output$plot1 <- renderPlotly({
    # Plot 1: Statistics by Year/Quarter/Month
    
    # Summarized data for timeline plot
    timeline_data = crashes_data()$all |>
      group_by(!!sym(input$time_scale)) |>
      summarize(NUM_CRASHES = n(),
                TOTAL_KILLED = sum(NUMBER_KILLED, na.rm = TRUE),
                TOTAL_INJURED = sum(NUMBER_INJURED, na.rm = TRUE),
                .groups = "drop") |>
      arrange(!!sym(input$time_scale))
    
    # Data filtered for alcohol involvement
    alcohol_timeline_data = crashes_data()$alcohol |>
      group_by(!!sym(input$time_scale)) |>
      summarize(NUM_CRASHES = n(),
                TOTAL_KILLED = sum(NUMBER_KILLED, na.rm = TRUE),
                TOTAL_INJURED = sum(NUMBER_INJURED, na.rm = TRUE),
                .groups = "drop") |>
      arrange(!!sym(input$time_scale))
    
    
    # ggplot components determined by user input
    stat = ifelse (input$time_stat == "Killed", "Car Accident Deaths",
                   ifelse (input$time_stat == "Injured", "Car Accident Injuries",
                           paste(input$time_stat, "Accidents")))
    
    y_variable = ifelse (input$time_stat == "Killed", "TOTAL_KILLED",
                         ifelse (input$time_stat == "Injured", "TOTAL_INJURED",       
                                 "NUM_CRASHES"))
    
    y_label = ifelse (input$time_stat == "Killed", "Number of Deaths",
                      ifelse (input$time_stat == "Injured", "Number of Injuries",       
                              "Number of Accidents"))
    
    x_label = switch(input$time_scale,
                     "ACCIDENT_YEAR" = "Year",
                     "ACCIDENT_QUARTER_YEAR" = "Quarter-Year",
                     "ACCIDENT_MONTH_YEAR" = "Month-Year")
    
    num_years = max(1, input$time_period[2] - input$time_period[1])
    axis_scale = switch(input$time_scale,
                        "ACCIDENT_YEAR" = 10,
                        "ACCIDENT_QUARTER_YEAR" = max(4, 11 - num_years),
                        "ACCIDENT_MONTH_YEAR" = max(2, 9 - num_years))
    
    # ggplot code
    p1 = ggplot() +
      geom_col(data = timeline_data, 
               aes(x = !!sym(input$time_scale), y = !!sym(y_variable)),
               fill = "skyblue") +
      labs(title = paste(stat, "by", x_label),
           x = x_label,
           y = y_label) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1, size = axis_scale),
            plot.title = element_text(size = 12))
    
    if (input$alcohol) {
      p1 = p1 + geom_col(data = alcohol_timeline_data, 
                         aes(x = !!sym(input$time_scale), y = !!sym(y_variable)),
                         fill = "salmon")
    }
    
    # Ensures all year labels show up on x-axis    
    if (input$time_scale == "ACCIDENT_YEAR") {
      p1 = p1 + scale_x_continuous(breaks = 2014:2023)
    }
    
    ggplotly(p1)
  })
  
  
  output$plot2 <- renderPlotly({
    # Plot 2: Statistics by Hour
    
    # Summarized data for time of day plot
    hourly_data = crashes_data()$all |>
      filter(COLLISION_TIME >= 0 & COLLISION_TIME <= 2359) |>
      group_by(COLLISION_HOUR) |>
      summarize(NUM_CRASHES = n(),
                TOTAL_KILLED = sum(NUMBER_KILLED, na.rm = TRUE),
                TOTAL_INJURED = sum(NUMBER_INJURED, na.rm = TRUE)) |>
      arrange(COLLISION_HOUR)
    
    # Data filtered for alcohol involvement
    alcohol_hourly_data = crashes_data()$alcohol |>
      filter(COLLISION_TIME >= 0 & COLLISION_TIME <= 2359) |>
      group_by(COLLISION_HOUR) |>
      summarize(NUM_CRASHES = n(),
                TOTAL_KILLED = sum(NUMBER_KILLED, na.rm = TRUE),
                TOTAL_INJURED = sum(NUMBER_INJURED, na.rm = TRUE)) |>
      arrange(COLLISION_HOUR)
    
    
    # ggplot components determined by user input
    stat = ifelse (input$time_stat == "Killed", "Car Accident Deaths",
                   ifelse (input$time_stat == "Injured", "Car Accident Injuries",
                           paste(input$time_stat, "Accidents")))
    
    y_variable = ifelse (input$time_stat == "Killed", "TOTAL_KILLED",
                         ifelse (input$time_stat == "Injured", "TOTAL_INJURED",       
                                 "NUM_CRASHES"))
    
    y_label = ifelse (input$time_stat == "Killed", "Number of Deaths",
                      ifelse (input$time_stat == "Injured", "Number of Injuries",       
                              "Number of Accidents"))
    
    year_range = if (input$time_period[1] == input$time_period[2]) input$time_period[1] 
    else paste0(input$time_period[1], "-", input$time_period[2])
    
    # ggplot code
    p2 = ggplot() +
      geom_col(data = hourly_data,
               aes(x = COLLISION_HOUR, y = !!sym(y_variable)),
               fill = "skyblue") +
      scale_x_continuous(breaks = 0:23,
                         labels = paste0(0:23, ":00")) +
      labs(title = paste0(stat, " by Time of Day (", year_range, ")"),
           x = "Hour Start",
           y = y_label) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1, size = 8),
            plot.title = element_text(size = 12))
    
    # Overlaid plot for alcohol-involved accidents
    if (input$alcohol) {
      p2 = p2 + geom_col(data = alcohol_hourly_data,
                         aes(x = COLLISION_HOUR, y = !!sym(y_variable)),
                         fill = "salmon")
    }
    
    ggplotly(p2)
  })
  
  
  output$plot3 <- renderPlotly({
    # Plot 3: Statistics by Hour (Alcohol Involved)
    
    # Summarized data for time of day plot (alcohol involved)
    alcohol_hourly_data = crashes_data()$alcohol |>
      filter(COLLISION_TIME >= 0 & COLLISION_TIME <= 2359) |>
      group_by(COLLISION_HOUR) |>
      summarize(NUM_CRASHES = n(),
                TOTAL_KILLED = sum(NUMBER_KILLED, na.rm = TRUE),
                TOTAL_INJURED = sum(NUMBER_INJURED, na.rm = TRUE)) |>
      arrange(COLLISION_HOUR)
    
    
    # ggplot components determined by user input
    stat = ifelse (input$time_stat == "Killed", "Alcohol-Involved Car Accident Deaths",
                   ifelse (input$time_stat == "Injured", "Alcohol-Involved Car Accident Injuries",
                           ifelse (input$time_stat == "All", "All Alcohol-Involved Accidents",
                                   paste("Alcohol-Involved", input$time_stat, "Accidents"))))
    
    y_variable = ifelse (input$time_stat == "Killed", "TOTAL_KILLED",
                         ifelse (input$time_stat == "Injured", "TOTAL_INJURED",       
                                 "NUM_CRASHES"))
    
    y_label = ifelse (input$time_stat == "Killed", "Number of Deaths",
                      ifelse (input$time_stat == "Injured", "Number of Injuries",       
                              "Number of Accidents"))
    
    year_range = if (input$time_period[1] == input$time_period[2]) input$time_period[1] 
    else paste0(input$time_period[1], "-", input$time_period[2])
    
    # ggplot code
    p3 = ggplot() +
      geom_col(data = alcohol_hourly_data,
               aes(x = COLLISION_HOUR, y = !!sym(y_variable)),
               fill = "salmon") +
      scale_x_continuous(breaks = 0:23,
                         labels = paste0(0:23, ":00")) +
      labs(title = paste0(stat, " by Time of Day (", year_range, ")"),
           x = "Hour Start",
           y = y_label) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1, size = 8),
            plot.title = element_text(size = 10))
    
    ggplotly(p3)
  })
  
  
  # -----------------------------------------------
  # Tab 2: Output
  # -----------------------------------------------
  output$map <- renderLeaflet({
    map_data = crashes_data()$all |>
      mutate(CASUALTIES = NUMBER_KILLED + NUMBER_INJURED)
    
    if (input$county != "All") {
      map_data = map_data |> 
        filter(COUNTY == input$county)
    }
    
    palette = if (input$color == "None") NULL else palettes[[input$color]]
    
    legend_values = switch(input$color,
                           "VEHICLES" = vehicles,
                           "COLLISION_SEVERITY" = severity,
                           "ALCOHOL_INVOLVED" = alcohol,
                           "WEATHER_1" = weather)
    
    map = map_data |>
      leaflet() |>
      addProviderTiles(provider = "CartoDB.Positron") |>
      addCircles(lng = ~POINT_X,
                 lat = ~POINT_Y,
                 popup = ~paste0(COLLISION_DATE, ", ", VEHICLES,
                                ", Type: ", TYPE_OF_COLLISION,
                                ", Severity: ", COLLISION_SEVERITY,
                                ", Casualties: ", CASUALTIES),
                 color = if (is.null(palette)) "skyblue" else ~palette(map_data[[input$color]]),
                 fillOpacity = 0.8) 
    
    if (input$color != "None") {
      map = map |>
        addLegend(position = "bottomleft",
                  pal = palette,
                  values = legend_values,
                  title = "Legend",
                  opacity = 0.8)
    }
    
    map
  })
  
  
} # closes server



# ===============================================
# Run the application
# ===============================================
shinyApp(ui = ui, server = server)
