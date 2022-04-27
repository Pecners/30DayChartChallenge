library(shiny)
library(tidyverse)
library(curl)
library(jsonlite)
library(lubridate)
library(glue)
library(showtext)

# Help function to expand period returned by API ----

expand_period <- function(df) {
  temp_df <- map_df(1:nrow(df), function(i) {
    t <- df[i,]
    
    if (str_detect(t$validTime, "H")) {
      p_hours <- as.numeric(str_extract(t$validTime, "\\d*(?=H)"))
    } else {
      p_hours <- 0
    }
    
    if (str_detect(t$validTime, "D")) {
      p_days <- as.numeric(str_extract(t$validTime, "\\d*(?=D)"))
    } else {
      p_days <- 0
    }
    
    p_full <- p_days * 24 + p_hours
    
    start_time <- ymd_hms(str_sub(t$validTime, start = 1, end = 25),
                          tz = "America/Chicago", quiet = TRUE)
    
    hours <- seq.POSIXt(from = start_time, length.out = p_full, by = 60*60)
    
    return(
      tibble(
        hours = hours,
        value = t$value
      )
    )
  })
}

# Define UI ----

ui <- fluidPage(
  
  # Import font, set it as main body font
  
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Merienda&display=swap');
          body {
            font-family: 'Merienda';
          }
                    ")
    )
  ),
  
  # App title, aligned center ----
  titlePanel(h1("Spencer's Very Specific Kayaking Forecast", align = "center"),
             windowTitle = "Spencer Kayaks"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for allowable kayaking conditions ----
      sliderInput(inputId = "waves",
                  label = "Set your wave height limit (ft):",
                  min = 1,
                  max = 10,
                  value = 5),
      sliderInput(inputId = "wind",
                  label = "Set your wind speed limit (mph):",
                  min = 1,
                  max = 20,
                  value = 10),
      # Basic info on the API used
      tags$div(class="header", checked=NA,
               tags$p(
                 glue("Data is sourced via the US National Weather Service API (see link below). ",
                      "Location is set near South Shore Marina in Milwaukee, Wisconsin, ",
                      "where Spencer most often launches his kayak.")
               ),
               tags$a(href="https://api.weather.gov/gridpoints/MKX/90,62", 
                      "https://api.weather.gov/gridpoints/MKX/90,62")
      ),
      # Legend
      h2("Color Coding", style='text-align:center;'),
      h3("Good conditions", style='background:#2dc937;color:white;text-align:center;padding:5px;'),
      h3("Okay conditions", style='background:#efb700;color:white;text-align:center;padding:5px;'),
      h3("Bad conditions", style='background:#CC3232;color:white;text-align:center;padding:5px;'),
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      plotOutput(outputId = "windPlot"),
      plotOutput(outputId = "wavePlot")
      
    )
  )
)

# Define server logic to query api and draw plots ----
server <- function(input, output) {
  
  # Query API
  # Note that I have hard coded my location here
  
  url <- "https://api.weather.gov/gridpoints/MKX/90,62"
  
  req <- curl_fetch_memory(url)
  
  x <- prettify(rawToChar(req$content)) %>%
    fromJSON()
  
  # Access elements with values of interest,
  # convert to US units
  
  waves <- x$properties$waveHeight$values %>%
    mutate(value = value * 3.281) # convert from m to ft
  
  temp <- x$properties$temperature$values %>%
    mutate(value = value * 1.8 + 32) # convert from C to F
  
  wind <- x$properties$windSpeed$values %>%
    mutate(value / 1.609) # convert from kph to mph
  
  # Set stoplight colors
  
  green <- "#2dc937"
  yellow <- "#efb700"
  red <- "#CC3232"
  
  # Add font again, not sure how to access font added by CSS 
  # in UI element
  
  font_add_google("Merienda", "m")
  showtext_auto()
  
  
  output$windPlot <- renderPlot({
    wave_max <- input$waves
    waves_df <- expand_period(waves)
    
    waves_df %>%
      mutate(gonogo = case_when(value >= wave_max ~ red,
                                value >= wave_max * .6 ~ yellow,
                                TRUE ~ green)) %>%
      ggplot(aes(hours, value, fill = gonogo, color = gonogo)) +
      geom_col() +
      scale_fill_identity() +
      scale_color_identity() +
      scale_y_continuous(breaks = c(1:floor(max(waves_df$value))),
                         labels = c(1:(floor(max(waves_df$value)) - 1),
                                    paste0(floor(max(waves_df$value)), " ft"))) +
      scale_x_datetime(expand = c(0, 0)) +
      theme_minimal() +
      theme(text = element_text(family = "m"),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            plot.title.position = "plot") +
      labs(x = "", y = "", 
           title = "Wave Height Forecast")
  })
  
  output$wavePlot <- renderPlot({
    wind_max <- input$wind
    wind_df <- expand_period(wind)
    
    wind_df %>%
      mutate(value = value / 1.609,
             gonogo = case_when(value >= wind_max ~ red,
                                value >= wind_max * .6 ~ yellow,
                                TRUE ~ green)) %>%
      ggplot(aes(hours, value, fill = gonogo, color = gonogo)) +
      geom_col() +
      scale_fill_identity() +
      scale_color_identity() +
      scale_y_continuous(breaks = c(1:floor(max(wind_df$value))),
                         labels = c(1:(floor(max(wind_df$value)) - 1),
                                    paste0(floor(max(wind_df$value)), " mph"))) +
      scale_x_datetime(expand = c(0, 0)) +
      theme_minimal() +
      theme(text = element_text(family = "m"),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            plot.title.position = "plot") +
      labs(x = "", y = "", 
           title = "Wind Speed Forecast")
  })
  
  
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
