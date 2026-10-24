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
  
  animation_progress <- reactiveVal(0)
  
  animation_running <- reactiveVal(FALSE)
  
  observe({
    animation_progress(input$animationProgress)
  })
  
  observeEvent(input$animateDorling, {
    
    updateSliderInput(
      session,
      "animationProgress",
      value = 0
    )
    
    animation_running(TRUE)
    
  })
  
  observe({
    
    req(animation_running())
    
    invalidateLater(50)
    
    current <- input$animationProgress
    
    if (current >= 1) {
      
      animation_running(FALSE)
      
    } else {
      
      updateSliderInput(
        session,
        "animationProgress",
        value = min(1, current + 0.04)
      )
      
    }
    
  })
  
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
    print(nrow(map_df))
    
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
      group_by(map_code) %>%
      summarise(
        value = first(na.omit(value)),
        .groups = "drop"
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
    
    ########################################################
    # ANIMATION RESEARCH
    #
    # PURPOSE
    # -------
    # Build equivalent Dorling cartograms for both:
    #
    # - Tonnes per year
    # - Kilograms per person per day
    #
    # This allows country locations to be compared between
    # states and is the foundation for future cartogram
    # animation.
    #
    # AI NOTES
    # --------
    # Both cartograms currently contain 215 countries and
    # share the same ISO3 identifiers, making country-level
    # interpolation possible.
    ########################################################
    
    msw_tonnes <- waw_clean %>%
      filter(
        INDICATOR_LABEL == "Municipal Solid Waste (MSW) Generation",
        UNIT_MEASURE_LABEL == "Tonnes per year"
      ) %>%
      mutate(
        value = as.numeric(`2030`)
      ) %>%
      filter(
        !is.na(value)
      ) %>%
      group_by(map_code) %>%
      summarise(
        value = first(na.omit(value)),
        .groups = "drop"
      )
    
    msw_per_capita <- waw_clean %>%
      filter(
        INDICATOR_LABEL == "Municipal Solid Waste (MSW) Generation",
        UNIT_MEASURE_LABEL == "Kilograms per person per day"
      ) %>%
      mutate(
        value = as.numeric(`2030`)
      ) %>%
      filter(
        !is.na(value)
      ) %>%
      group_by(map_code) %>%
      summarise(
        value = first(na.omit(value)),
        .groups = "drop"
      )
    
    world_tonnes <- world %>%
      left_join(
        msw_tonnes,
        by = c("adm0_a3" = "map_code")
      )
    
    world_per_capita <- world %>%
      left_join(
        msw_per_capita,
        by = c("adm0_a3" = "map_code")
      )
    
    dorling_tonnes <- cartogram_dorling(
      st_transform(
        world_tonnes %>% filter(!is.na(value), value > 0),
        3857
      ),
      weight = "value"
    )
    
    dorling_per_capita <- cartogram_dorling(
      st_transform(
        world_per_capita %>% filter(!is.na(value), value > 0),
        3857
      ),
      weight = "value"
    )
    
    ########################################################
    # CARTOGRAM INTERPOLATION DATA
    #
    # PURPOSE
    # -------
    # Extract cartogram centre points from both states and
    # calculate intermediary positions.
    #
    # Example:
    #
    # Start (Tonnes)
    #        ↓
    # Midpoint
    #        ↓
    # End (Per Capita)
    #
    # Future animation can increase progress from:
    #
    # 0.0 -> 1.0
    #
    # to smoothly morph the cartogram.
    ########################################################
    
    
    map_centres <- st_coordinates(
      st_centroid(world_msw_dorling)
    )
    
    top5_tonnes <- msw_tonnes %>%
      filter(!is.na(value)) %>%
      arrange(desc(value)) %>%
      head(5)
    
    top5_per_capita <- msw_per_capita %>%
      arrange(desc(value)) %>%
      head(5)
    
    animation_df <- data.frame(
      country = world_msw_dorling$name,
      iso3 = world_msw_dorling$adm0_a3,
      x = map_centres[,1],
      y = map_centres[,2],
      
      value_start =
        dorling_tonnes$value /
        max(dorling_tonnes$value),
      
      value_end =
        dorling_per_capita$value /
        max(dorling_per_capita$value)
    )
    
    progress <- animation_progress()
  
    if (progress < 0.1) {
      
      plot(
        st_geometry(world_msw),
        col = "lightgrey",
        border = "white",
        main = "Global Municipal Waste Generation (2030)"
      )
      
      return()
      
    }
    
    bubble_progress <- max(
      0,
      min(
        1,
        (progress - 0.1) / 0.9
      )
    )
    
    animation_df$value_current <-
      animation_df$value_start +
      (animation_df$value_end - animation_df$value_start) * bubble_progress
    
    animation_df$radius_current <-
      animation_df$value_current * 3000000
    
    animation_df$fill_colour <- "lightgrey"
    
    animation_df$fill_colour[
      animation_df$iso3 %in% top5_tonnes$map_code
    ] <- "#4E79A7"
    
    animation_df$fill_colour[
      animation_df$iso3 %in% top5_per_capita$map_code
    ] <- "#F28E2B"
    
    symbols(
      animation_df$x,
      animation_df$y,
      circles = animation_df$radius_current,
      inches = FALSE,
      bg = animation_df$fill_colour,
      fg = "black",
      asp = 1,
      main = paste(
        "From Tonnes per Year to Kilograms per Person per Day\n",
        round(progress * 100),
        "% Complete"
      )
      
    )
    
    label_df <- animation_df %>%
      filter(fill_colour != "lightgrey")
    
    text(
      label_df$x,
      label_df$y,
      labels = label_df$iso3,
      cex = 0.7,
      col = "black"
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