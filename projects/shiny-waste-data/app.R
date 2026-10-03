#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)
library(leaflet)
library(sf)
library(rnaturalearth)

# Load data
waw <- read_csv("data/WaW3.csv")

# Create global totals by year
global_msw <- waw %>%
  filter(
    INDICATOR_LABEL == "Municipal Solid Waste (MSW) Generation",
    UNIT_MEASURE_LABEL == "Tonnes per year"
  ) %>%
  summarise(
    across(
      matches("^\\d{4}$"),
      ~ sum(as.numeric(.), na.rm = TRUE)
    )
  )

# Convert to Year/Value format for charting
global_msw_long <- global_msw %>%
  pivot_longer(
    everything(),
    names_to = "Year",
    values_to = "Value"
  ) %>%
  mutate(
    Year = as.numeric(Year)
  ) %>%
  arrange(Year)

# Build a simple forecast trend from trusted points
trusted_points <- global_msw_long %>%
  filter(Year %in% c(2022, 2030, 2040, 2050))

forecast_model <- lm(Value ~ Year, data = trusted_points)

forecast_line <- data.frame(
  Year = seq(
    min(global_msw_long$Year),
    max(global_msw_long$Year),
    by = 1
  )
)

forecast_line$Value <- predict(
  forecast_model,
  newdata = forecast_line
)

# Natural Earth countries
world <- ne_countries(
  scale = "medium",
  returnclass = "sf"
)

# Country centroids
centroids <- st_centroid(world)

coords <- st_coordinates(centroids)

centroids$lon <- coords[,1]
centroids$lat <- coords[,2]

# 2050 MSW by country
msw_2050 <- waw %>%
  filter(
    INDICATOR_LABEL == "Municipal Solid Waste (MSW) Generation",
    UNIT_MEASURE_LABEL == "Tonnes per year"
  ) %>%
  select(
    REF_AREA_LABEL,
    `2050`
  )

# Join WaW to Natural Earth
map_data <- centroids %>%
  left_join(
    msw_2050,
    by = c("name" = "REF_AREA_LABEL")
  )

# User Interface
ui <- fluidPage(
  
  titlePanel("What a Waste 3.0"),
  
  tabsetPanel(
    
    tabPanel(
      "Data Explorer",
      
      h2("Municipal Solid Waste Generation"),
      
      selectInput(
        "area",
        "Country / Region",
        choices = sort(unique(waw$REF_AREA_LABEL)),
        selected = "Global"
      ),
      
      plotOutput("trendChart", height = "500px"),
      
      hr(),
      
      DTOutput("dataTable")
    ),
    
    tabPanel(
      "Map",
      
      h2("Map Explorer"),
      
      leafletOutput(
        "map",
        height = "700px"
      )
      
    )
    
  )
  
)

# Server

server <- function(input, output, session) {

  filtered_msw <- reactive({

    waw %>%
      filter(
        REF_AREA_LABEL == input$area,
        INDICATOR_LABEL == "Municipal Solid Waste (MSW) Generation",
        UNIT_MEASURE_LABEL == "Tonnes per year"
      ) %>%
      summarise(
        across(
          matches("^\\d{4}$"),
          ~ sum(as.numeric(.), na.rm = TRUE)
        )
      ) %>%
      pivot_longer(
        everything(),
        names_to = "Year",
        values_to = "Value"
      ) %>%
      mutate(
        Year = as.numeric(Year)
      ) %>%
      arrange(Year)

  })

  output$trendChart <- renderPlot({

    # Build forecast line for selected area
    trusted_points <- filtered_msw() %>%
      filter(Year %in% c(2022, 2030, 2040, 2050))

    forecast_model <- lm(
      Value ~ Year,
      data = trusted_points
    )

    forecast_line <- data.frame(
      Year = seq(
        min(filtered_msw()$Year),
        max(filtered_msw()$Year),
        by = 1
      )
    )

    forecast_line$Value <- predict(
      forecast_model,
      newdata = forecast_line
    )

    ggplot() +

      geom_line(
        data = filtered_msw(),
        aes(
          x = Year,
          y = Value
        ),
        linewidth = 1.2,
        colour = "#0072B2"
      ) +

      geom_point(
        data = filtered_msw(),
        aes(
          x = Year,
          y = Value
        ),
        colour = "#0072B2"
      ) +

      geom_line(
        data = forecast_line,
        aes(
          x = Year,
          y = Value
        ),
        colour = "#D55E00",
        linewidth = 1.5,
        linetype = "dashed"
      ) +

      labs(
        x = "Year",
        y = "Tonnes per year",
        title = paste(
          input$area,
          "Municipal Solid Waste Generation"
        )
      ) +

      theme_minimal()

  })

  output$dataTable <- renderDT({

    datatable(
      filtered_msw(),
      options = list(
        pageLength = 20,
        order = list(list(1, 'desc'))
      )
    )

  })
  
  output$map <- renderLeaflet({
    
    leaflet(map_data) %>%
      
      addTiles() %>%
      
      addCircleMarkers(
        lng = ~lon,
        lat = ~lat,
        
        radius = ~sqrt(as.numeric(`2050`)) / 1000,
        
        popup = ~paste(
          name,
          "<br>",
          format(
            round(as.numeric(`2050`)),
            big.mark = ","
          ),
          "tonnes"
        ),
        
        stroke = FALSE,
        fillOpacity = 0.7
      ) %>%
      
      setView(
        lng = 0,
        lat = 20,
        zoom = 2
      )
    
  })

}

shinyApp(ui, server)