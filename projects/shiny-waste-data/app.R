############################################################
# WHAT A WASTE 3.0
#
# PURPOSE
# -------
# Main Shiny application entry point.
#
# RESPONSIBILITIES
# ----------------
# - Load libraries
# - Load datasets and reference data
# - Load tab definitions
# - Define UI
# - Define server logic
#
# RELATED FILES
# -------------
# R/data.R
# R/lookups.R
# R/geography.R
# R/race.R
# R/policy.R
#
# ui/home_tab.R
# ui/data_explorer_tab.R
# ui/map_tab.R
# ui/cartogram_tab.R
# ui/race_tab.R
# ui/policy_tab.R
#
# AI NOTES
# --------
# UI tab definitions should live in ui/*.
#
# Country cleaning should live in R/lookups.R.
#
# Natural Earth and sf spatial logic should live in
# R/geography.R.
#
# Avoid adding large data preparation logic directly
# into app.R.
############################################################

library(shiny)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)
library(leaflet)
library(sf)
library(rnaturalearth)
library(cartogram)
library(plotly)

# Load files
# Data first
source("R/data.R")
source("R/lookups.R")
source("R/geography.R")
source("R/race.R")
source("R/policy.R")

# Then load tabs
source("ui/home_tab.R")
source("ui/data_explorer_tab.R")
source("ui/map_tab.R")
source("ui/cartogram_tab.R")
source("ui/race_tab.R")
source("ui/policy_tab.R")


# User Interface
ui <- fluidPage(
  
  titlePanel("What a Waste 3.0"),
  
  tabsetPanel(
    
    homeTab,
    dataExplorerTab,
    mapTab,
    cartogramTab,
    raceTab,
    policyTab
    
  )
  
)

# Server

server <- function(input, output, session) {

  # Build country-level map data for the selected year.
  #
  # Users select:
  #   2021
  #   2022
  #   2030
  #   2040
  #   2050
  #
  # The selected year is converted into a generic 'value'
  # column so that the mapping code remains independent
  # of the underlying year.
  
  map_data <- reactive({
    
    msw <- waw_clean %>%
      filter(
        INDICATOR_LABEL == "Municipal Solid Waste (MSW) Generation",
        UNIT_MEASURE_LABEL == "Tonnes per year"
      ) %>%
      mutate(
        value = as.numeric(.data[[input$mapYear]])
      ) %>%
      filter(
        !is.na(value)
      ) %>%
      group_by(map_code) %>%
      summarise(
        value = first(value),
        .groups = "drop"
      )
    
    world %>%
      left_join(
        msw,
        by = c("adm0_a3" = "map_code")
      )
    
  })
  
  
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
    
    map_df <- map_data()
    
    print(input$mapScale)
    
    values <- map_df$value
    
    if (input$mapScale == "Natural") {
      
      map_df$display_value <- map_df$value
      
      pal <- colorNumeric(
        palette = input$mapPalette,
        domain = map_df$display_value,
        na.color = "#d3d3d3"
      )
      
    } else if (input$mapScale == "Logarithmic") {
      
      map_df$display_value <- ifelse(
        map_df$value > 0,
        log10(map_df$value),
        NA
      )
      
      pal <- colorNumeric(
        palette = input$mapPalette,
        domain = map_df$display_value,
        na.color = "#d3d3d3"
      )
      
    } else if (input$mapScale == "Quantiles") {
      
      map_df$display_value <- map_df$value
      
      pal <- colorQuantile(
        palette = input$mapPalette,
        domain = map_df$display_value,
        n = 7,
        na.color = "#d3d3d3"
      )
      
    } else if (input$mapScale == "Percentile") {
      
      map_df$display_value <- percent_rank(map_df$value)
      
      pal <- colorNumeric(
        palette = input$mapPalette,
        domain = c(0, 1),
        na.color = "#d3d3d3"
      )
      
    }
    
    leaflet(map_df) %>%
      
      addTiles() %>%
      
      addPolygons(
        
        fillColor = ~pal(display_value),
        
        fillOpacity = 0.8,
        
        color = "#666666",
        
        weight = 0.5,
        
        popup = ~paste(
          "<b>", name, "</b>",
          "<br>",
          input$mapYear,
          ": ",
          format(
            round(as.numeric(value)),
            big.mark = ","
          ),
          " tonnes"
        )
      ) %>%
      
      addLegend(
        position = "bottomright",
        pal = pal,
        values = map_df$display_value,
        title = paste(
          input$mapScale,
          "-",
          input$mapYear
        )
      ) %>%
      
      setView(
        lng = 0,
        lat = 20,
        zoom = 2
      )
    
  })

  ############################################################
  # DORLING CARTOGRAM
  #
  # PURPOSE
  # -------
  # Visualise waste metrics using circle sizes rather
  # than geographic area.
  #
  # PROCESS
  # -------
  # WaW Data
  #   -> Country Normalisation
  #   -> Natural Earth Join
  #   -> Projection
  #   -> Cartogram Generation
  #
  # AI NOTES
  # --------
  # Cartograms require projected geometries.
  #
  # If errors mention longitude/latitude, check
  # st_transform().
  #
  # Missing bubbles are usually caused by failed country
  # name joins or missing values.
  ############################################################
  
  output$dorlingPlot <- renderPlot({
    
    msw <- waw_clean %>%
      filter(
        INDICATOR_LABEL == "Municipal Solid Waste (MSW) Generation",
        UNIT_MEASURE_LABEL == input$cartogramUnit
      ) %>%
      mutate(
        value = as.numeric(`2030`)
      ) %>%
      filter(
        !is.na(value)
      ) %>%
      select(
        map_code,
        value
      )

    # Join on country code, not name (see R/lookups.R)
    world_msw <- world %>%
      left_join(
        msw,
        by = c("adm0_a3" = "map_code")
      )
    
    world_msw_proj <- st_transform(
      world_msw,
      3857
    )
    
    world_msw_dorling <- world_msw_proj %>%
      filter(
        !is.na(value),
        value > 0
      )
    
    dorling <- cartogram_dorling(
      world_msw_dorling,
      weight = "value"
    )
    
    plot(
      dorling["value"],
      main = "Municipal Solid Waste Generation (2030)"
    )
    
  })
  
  ############################################################
  # COUNTRY RACE
  #
  # The chart is built in R/race.R.
  ############################################################

  race_unit_data <- reactive({

    df <- race_frames %>%
      filter(unit == input$raceUnit)

    # Per person: skip small territories (see R/race.R)
    if (input$raceUnit == "Kilograms per person per day") {
      df <- df %>% filter(population >= 1e6)
    }

    df

  })

  output$racePlot <- renderPlotly({

    race_plot(
      race_unit_data(),
      top_n = as.numeric(input$raceTop),
      is_tonnes = input$raceUnit == "Tonnes per year"
    )

  })

  ############################################################
  # POLICY
  #
  # Charts are built in R/policy.R; the map key is in
  # ui/policy_tab.R.
  ############################################################

  output$eprKey <- renderUI(epr_key(input$policyMetric))

  output$eprMap <- renderPlotly(epr_map(input$policyMetric))

  output$eprGap <- renderPlotly(epr_gap_chart(input$policyMetric))

}

shinyApp(ui, server)