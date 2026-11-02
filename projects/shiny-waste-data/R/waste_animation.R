############################################################
# WASTE ANIMATION MAP
############################################################

library(plotly)
library(dplyr)
library(scales)
library(shiny)

waste_animation_frame <- function(animation_df, progress) {
  
  # Bubble animation starts after 10%.
  bubble_progress <- max(
    0,
    min(
      1,
      (progress - 0.1) / 0.9
    )
  )
  
  
  bubble_progress <- bubble_progress^0.75

  animation_df$value_current <-
    animation_df$value_start +
    (animation_df$value_end - animation_df$value_start) * bubble_progress
  
  colour_index <- round(
    1 + (animation_df$value_current * 99)
  )
  
  colour_index <- pmin(
    100,
    pmax(
      1,
      colour_index
    )
  )
  
  bubble_colour <- viridisLite::viridis(100)[colour_index]
  
  animation_df$size_current <- pmax(3, animation_df$value_current^2 * 600)
  animation_df$colour_current <- bubble_colour

  animation_df
}

waste_animation_title <- function(progress) {
  paste(
    "Transition:",
    "Total Waste → Waste Per Person",
    "(",
    round(progress * 100),
    "% )"
  )
}

waste_animation_map <- function(animation_df, progress) {

  animation_df <- waste_animation_frame(animation_df, progress)

  animation_df$hover <- sprintf(
    paste0(
      "<b>%s</b><br>",
      "Total waste: %s tonnes per year<br>",
      "Waste per person: %.2f kg per person per day"
    ),
    animation_df$country,
    format(
      round(animation_df$tonnes_per_year),
      big.mark = ","
    ),
    animation_df$kg_per_person_day
  )
  
  fallback_rows <- !is.na(animation_df$lon) &
    !is.na(animation_df$lat)
  iso_rows <- !fallback_rows

  p <- plot_geo() %>%
    add_trace(
      data = animation_df[iso_rows, , drop = FALSE],
      type = "scattergeo",
      mode = "markers",
      locations = ~iso3,
      locationmode = "ISO-3",
      hoverinfo = "text",
      text = ~hover,
      marker = list(
        size = animation_df$size_current[iso_rows],
        sizemode = "area",
        sizeref = 0.5,
        sizemin = 3,
        color = animation_df$colour_current[iso_rows],
        opacity = 0.8,
        line = list(
          color = "white",
          width = 1
        )
      )
    )

  if (any(fallback_rows)) {
    p <- p %>%
      add_trace(
        data = animation_df[fallback_rows, , drop = FALSE],
        type = "scattergeo",
        mode = "markers",
        lon = ~lon,
        lat = ~lat,
        hoverinfo = "text",
        text = ~hover,
        marker = list(
          size = animation_df$size_current[fallback_rows],
          sizemode = "area",
          sizeref = 0.5,
          sizemin = 3,
          color = animation_df$colour_current[fallback_rows],
          opacity = 0.8,
          line = list(
            color = "white",
            width = 1
          )
        )
      )
  }

  p %>%
    layout(
      showlegend = FALSE,
      geo = list(
        uirevision = "keep",
        showframe = FALSE,
        showcoastlines = FALSE,
        showcountries = TRUE,
        countrycolor = "#c3c2b7",
        showland = TRUE,
        landcolor = "#F7F6F2",
        projection = list(
          type = "natural earth"
        ),
        lataxis = list(
          range = c(-58, 85)
        )
      ),
      margin = list(
        l = 0,
        r = 0,
        t = 40,
        b = 0
      ),
      title = waste_animation_title(progress)
      
    ) %>%
    config(
      displayModeBar = FALSE
    )

  
}

waste_transition_legend <- function() {
  
  swatch <- function(colour) {
    span(style = sprintf(
      "display: inline-block; width: 12px; height: 12px;
       margin-right: 6px; vertical-align: -1px;
       background: %s; border-radius: 50%%;",
      colour
    ))
  }
  
  div(
    
    strong("Bubble size"),
    
    div(
      style = "margin-top: 6px;",
      "Larger bubbles represent countries with greater prominence in the current view."
    ),
    
    div(
      style = "margin-top: 10px;",
      strong("Bubble colour")
    ),
    
    div(
      swatch("#440154"),
      "Lower ranking"
    ),
    
    div(
      swatch("#21908C"),
      "Middle ranking"
    ),
    
    div(
      swatch("#FDE725"),
      "Higher ranking"
    ),
    
    div(
      style = "margin-top: 8px;",
      class = "text-muted",
      "Yellow countries are the most prominent at each stage of the transition."
    )
    
  )
  
}