############################################################
# WASTE TRANSITION ANIMATION
#
# PURPOSE
# -------
# Prepare country-level total or estimated material values
# and render the animated transition from total tonnes to
# per-person rankings.
#
# DATA NOTE
# ---------
# Material values are estimates: each country's latest
# reported composition share is applied to its 2030 MSW
# projections. This is not a reported material projection.
#
# USED BY
# -------
# ui/cartogram_tab.R and the matching server logic in app.R.
############################################################

library(plotly)
library(dplyr)
library(scales)
library(shiny)

waste_animation_choices <- function(waw_data) {

  products <- waw_data %>%
    filter(
      INDICATOR == "WM_MSW_COMP",
      UNIT_MEASURE_LABEL == "Percentage of Municipal Solid Waste (MSW)",
      WASTE_PRODUCT != "_Z"
    ) %>%
    distinct(WASTE_PRODUCT, WASTE_PRODUCT_LABEL) %>%
    arrange(WASTE_PRODUCT_LABEL)

  c(
    "Total municipal waste" = "total",
    stats::setNames(products$WASTE_PRODUCT, products$WASTE_PRODUCT_LABEL)
  )

}

waste_animation_data <- function(waw_data, world, waste_type = "total") {

  year_columns <- names(waw_data)[grepl("^\\d{4}$", names(waw_data))]
  is_estimate <- waste_type != "total"

  projected_tonnes <- waw_data %>%
    filter(
      INDICATOR_LABEL == "Municipal Solid Waste (MSW) Generation",
      UNIT_MEASURE_LABEL == "Tonnes per year"
    ) %>%
    transmute(
      map_code,
      value = as.numeric(.data[["2030"]])
    ) %>%
    filter(!is.na(value)) %>%
    group_by(map_code) %>%
    summarise(
      tonnes_per_year = first(value),
      .groups = "drop"
    )

  projected_per_capita <- waw_data %>%
    filter(
      INDICATOR_LABEL == "Municipal Solid Waste (MSW) Generation",
      UNIT_MEASURE_LABEL == "Kilograms per person per day"
    ) %>%
    transmute(
      map_code,
      value = as.numeric(.data[["2030"]])
    ) %>%
    filter(!is.na(value)) %>%
    group_by(map_code) %>%
    summarise(
      kg_per_person_day = first(value),
      .groups = "drop"
    )

  animation_df <- world %>%
    sf::st_drop_geometry() %>%
    left_join(projected_tonnes, by = c("adm0_a3" = "map_code")) %>%
    left_join(projected_per_capita, by = c("adm0_a3" = "map_code"))

  if (is_estimate) {

    product <- waw_data %>%
      filter(
        INDICATOR == "WM_MSW_COMP",
        UNIT_MEASURE_LABEL == "Percentage of Municipal Solid Waste (MSW)",
        WASTE_PRODUCT == waste_type
      ) %>%
      distinct(WASTE_PRODUCT_LABEL)

    if (nrow(product) == 0) {
      stop("No MSW composition data found for waste type: ", waste_type)
    }

    # Keep the latest valid observation by year; this is still a reported
    # historical share, not a composition value observed in the 2030 projection.
    composition <- waw_data %>%
      filter(
        INDICATOR == "WM_MSW_COMP",
        UNIT_MEASURE_LABEL == "Percentage of Municipal Solid Waste (MSW)",
        WASTE_PRODUCT == waste_type
      ) %>%
      select(map_code, all_of(year_columns)) %>%
      tidyr::pivot_longer(
        cols = all_of(year_columns),
        names_to = "composition_year",
        values_to = "composition_share_pct",
        values_transform = list(composition_share_pct = as.character)
      ) %>%
      mutate(
        composition_year = as.integer(composition_year),
        composition_share_pct = dplyr::recode(
          composition_share_pct,
          "TRUE" = "1",
          "FALSE" = "0",
          .default = composition_share_pct
        ),
        composition_share_pct = as.numeric(composition_share_pct)
      ) %>%
      filter(
        !is.na(composition_share_pct),
        composition_share_pct >= 0,
        composition_share_pct <= 100
      ) %>%
      group_by(map_code) %>%
      slice_max(composition_year, n = 1, with_ties = FALSE) %>%
      ungroup()

    animation_df <- animation_df %>%
      left_join(composition, by = c("adm0_a3" = "map_code")) %>%
      # Composition shares are percentages of MSW; apply the same fraction
      # to both projected total-waste measures.
      mutate(
        tonnes_per_year = tonnes_per_year * composition_share_pct / 100,
        kg_per_person_day = kg_per_person_day * composition_share_pct / 100
      )

    waste_type_label <- product$WASTE_PRODUCT_LABEL[[1]]

  } else {
    waste_type_label <- "Total municipal waste"
  }

  if (is_estimate) {
    animation_df <- animation_df %>%
      filter(
        !is.na(tonnes_per_year),
        !is.na(kg_per_person_day),
        tonnes_per_year >= 0,
        kg_per_person_day >= 0,
        !is.na(composition_share_pct)
      )
  } else {
    animation_df <- animation_df %>%
      filter(
        !is.na(tonnes_per_year),
        !is.na(kg_per_person_day),
        tonnes_per_year > 0,
        kg_per_person_day > 0
      )
  }

  if (nrow(animation_df) == 0) {
    stop("No mappable countries found for waste type: ", waste_type)
  }

  animation_df %>%
    transmute(
      country = name,
      iso3 = adm0_a3,
      tonnes_per_year = as.numeric(tonnes_per_year),
      kg_per_person_day = as.numeric(kg_per_person_day),
      composition_share_pct = if (is_estimate) composition_share_pct else NA_real_,
      composition_year = if (is_estimate) composition_year else NA_integer_,
      waste_type_label = waste_type_label,
      is_estimate = is_estimate,
      # Compare each country by rank at each end of the animation, rather
      # than interpolating between unlike units (tonnes and kg/person/day).
      value_start = percent_rank(tonnes_per_year),
      value_end = percent_rank(kg_per_person_day)
    ) %>%
    left_join(
      plotly_geo_fallback_coords,
      by = "iso3"
    )

}

waste_animation_frame <- function(animation_df, progress) {
  
  # Delay the bubbles for the first 10% of the slider, then ease their
  # rank-based size and colour toward the per-person ranking.
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
  
  animation_df$size_current <- pmax(
    3,
    2 * animation_df$value_current * sqrt(600)
  )
  animation_df$colour_current <- bubble_colour

  animation_df
}

waste_animation_title <- function(progress, waste_type_label, is_estimate) {

  from <- if (is_estimate) {
    paste("Estimated", waste_type_label, "waste (tonnes/year)")
  } else {
    "Total waste (tonnes/year)"
  }

  to <- if (is_estimate) {
    paste("Estimated", waste_type_label, "waste per person (kg/person/day)")
  } else {
    "Waste per person (kg/person/day)"
  }

  paste0(
    "Transition: ",
    from,
    " to ",
    to,
    " (",
    round(progress * 100),
    "%)"
  )
}

waste_animation_map <- function(animation_df, progress) {

  animation_df <- waste_animation_frame(animation_df, progress)

  if (animation_df$is_estimate[[1]]) {
    animation_df$hover <- sprintf(
      paste0(
        "<b>%s</b><br>",
        "Estimated %s waste: %s tonnes per year<br>",
        "Estimated %s waste per person: %.2f kg per person per day<br>",
        "Reported MSW composition: %.2f%% (%d)"
      ),
      animation_df$country,
      animation_df$waste_type_label,
      format(round(animation_df$tonnes_per_year), big.mark = ","),
      animation_df$waste_type_label,
      animation_df$kg_per_person_day,
      animation_df$composition_share_pct,
      animation_df$composition_year
    )
  } else {
    animation_df$hover <- sprintf(
      paste0(
        "<b>%s</b><br>",
        "Total waste: %s tonnes per year<br>",
        "Waste per person: %.2f kg per person per day"
      ),
      animation_df$country,
      format(round(animation_df$tonnes_per_year), big.mark = ","),
      animation_df$kg_per_person_day
    )
  }
  
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
        sizemode = "diameter",
        sizeref = 1,
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
          sizemode = "diameter",
          sizeref = 1,
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
      title = waste_animation_title(
        progress,
        animation_df$waste_type_label[[1]],
        animation_df$is_estimate[[1]]
      )
      
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