############################################################
# WASTE ANIMATION MAP
############################################################

library(plotly)
library(dplyr)

waste_animation_map <- function(animation_df, progress) {
  
  # Bubble animation starts after 10%
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
  
  # Scale to sensible marker sizes
  animation_df$size_current <-
    sqrt(animation_df$value_current) * 15
  
  animation_df$hover <- sprintf(
    "<b>%s</b><br>Total Waste: %.3f<br>Per Capita: %.3f",
    animation_df$country,
    animation_df$value_start,
    animation_df$value_end
  )
  
  top_labels <- animation_df %>%
    arrange(desc(value_current)) %>%
    head(10)
  
  max_size <- max(animation_df$size_current, na.rm = TRUE)
  
  plot_geo() %>%
    add_trace(
      data = animation_df,
      type = "scattergeo",
      mode = "markers",
      locations = ~iso3,
      locationmode = "ISO-3",
      hoverinfo = "text",
      text = ~hover,
      marker = list(
        size = animation_df$size_current,
        sizemode = "area",
        sizeref = 2 * max_size / (80^2),
        sizemin = 3,
        color = "#4E79A7",
        opacity = 0.8,
        line = list(
          color = "white",
          width = 1
        )
      )
    ) %>%
    layout(
      showlegend = FALSE,
      geo = list(
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
      title = paste(
        "Total Waste → Waste Per Person",
        "|",
        round(progress * 100),
        "%"
      )
    ) %>%
    config(
      displayModeBar = FALSE
    )
  
}