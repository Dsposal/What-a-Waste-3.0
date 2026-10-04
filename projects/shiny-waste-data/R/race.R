############################################################
# COUNTRY RACE
#
# PURPOSE
# -------
# Data and chart for the "Country race" tab: an animated
# bar chart of the countries generating the most municipal
# solid waste, 2022 to 2050.
#
# AI NOTES
# --------
# Only 2022, 2030, 2040 and 2050 exist for every country,
# so the race starts in 2022. Years in between are
# interpolated for the animation only.
############################################################

library(dplyr)
library(tidyr)
library(plotly)

# The four years every country has a value for
race_data_years <- c(2022, 2030, 2040, 2050)

# Full region names for the colour key
race_regions <- c(
  "E. Asia & Pacific"        = "East Asia & Pacific",
  "Eur. & Cent. Asia"        = "Europe & Central Asia",
  "L. Amer. & the Caribbean" = "Latin America & Caribbean",
  "Mid. East & N. Africa"    = "Middle East & North Africa",
  "N. Amer."                 = "North America",
  "S. Asia"                  = "South Asia",
  "Sub-Saharan Africa"       = "Sub-Saharan Africa"
)

# Colour-blind safe palette (Okabe-Ito), one per region
race_colours <- c(
  "East Asia & Pacific"        = "#E69F00",
  "Europe & Central Asia"      = "#56B4E9",
  "Latin America & Caribbean"  = "#009E73",
  "Middle East & North Africa" = "#CC79A7",
  "North America"              = "#0072B2",
  "South Asia"                 = "#D55E00",
  "Sub-Saharan Africa"         = "#999999"
)

# One row per country, unit and data year
race_points <- waw %>%
  filter(
    INDICATOR == "WM_MSW_GEN",
    UNIT_MEASURE_LABEL %in% c(
      "Tonnes per year",
      "Kilograms per person per day"
    )
  ) %>%
  select(
    REF_AREA,
    country = REF_AREA_LABEL,
    REGION_WB,
    unit = UNIT_MEASURE_LABEL,
    all_of(as.character(race_data_years))
  ) %>%
  pivot_longer(
    all_of(as.character(race_data_years)),
    names_to = "year",
    values_to = "value"
  ) %>%
  mutate(
    year = as.numeric(year),
    value = as.numeric(value)
  ) %>%
  filter(!is.na(value)) %>%
  group_by(REF_AREA, unit, year) %>%
  summarise(
    country = first(country),
    region = race_regions[first(REGION_WB)],
    value = first(value),
    .groups = "drop"
  )

# Population is implied by the two units:
# tonnes / (kg per person per day * 365). Used only to keep
# small territories out of the per person ranking.
race_population <- race_points %>%
  filter(year == 2022) %>%
  select(REF_AREA, unit, value) %>%
  pivot_wider(names_from = unit, values_from = value) %>%
  mutate(
    population = `Tonnes per year` * 1000 /
      (`Kilograms per person per day` * 365)
  ) %>%
  select(REF_AREA, population)

# Every year from 2022 to 2050, interpolated in between
race_years <- seq(2022, 2050)

race_frames <- race_points %>%
  group_by(REF_AREA, country, region, unit) %>%
  # 'value' comes first: approx() needs the original years
  reframe(
    value = approx(year, value, xout = race_years)$y,
    year = race_years
  ) %>%
  left_join(race_population, by = "REF_AREA")


# Animated chart. Each year is a plotly frame and the y axis
# is the rank, so bars slide when countries swap places.
# Try it in the console:
#   race_plot(filter(race_frames, unit == "Tonnes per year"))
race_plot <- function(df, top_n = 15, is_tonnes = TRUE) {

  df <- df %>%
    group_by(year) %>%
    mutate(rank = rank(-value, ties.method = "first")) %>%
    ungroup()

  # Countries that reach the top N in some year. The rest of
  # the time they wait at rank N + 1, just out of view, so
  # they slide in and out rather than pop up.
  racers <- df %>%
    filter(rank <= top_n) %>%
    pull(country) %>%
    unique()

  df <- df %>%
    filter(country %in% racers) %>%
    mutate(
      rank = pmin(rank, top_n + 1),
      x = if (is_tonnes) value / 1e6 else value,
      label = if (is_tonnes) {
        paste0(country, "  ", round(x), " Mt")
      } else {
        paste0(country, "  ", sprintf("%.2f", x))
      }
    )

  axis_max <- max(df$x)

  # Tonnes start at 0. Per person values sit close together
  # (about 2 to 3 kg), so that axis starts just below the
  # smallest value shown, rounded down to 0.5 kg.
  if (is_tonnes) {
    axis_min <- 0
  } else {
    shown_min <- min(df$x[df$rank <= top_n])
    axis_min <- floor((shown_min - 0.1 * (axis_max - shown_min)) * 2) / 2
  }

  # Extra room on the right for the bar labels
  axis_end <- axis_max + 0.3 * (axis_max - axis_min)

  axis_title <- if (is_tonnes) {
    "Million tonnes per year"
  } else {
    "Kilograms per person per day"
  }

  # Big grey year, bottom right
  year_stamp <- data.frame(year = race_years)

  p <- plot_ly(
    df,
    x = ~x,
    y = ~rank,
    frame = ~year,
    ids = ~country,
    type = "bar",
    orientation = "h",
    color = ~region,
    colors = race_colours,
    text = ~label,
    textposition = "outside",
    textfont = list(size = 13, color = "#333333"),
    hoverinfo = "none"
  ) %>%

    add_text(
      data = year_stamp,
      x = axis_end - 0.04 * (axis_end - axis_min),
      y = top_n + 0.5,
      text = ~year,
      frame = ~year,
      inherit = FALSE,
      textposition = "top left",
      textfont = list(size = 72, color = "#d9d9d9"),
      hoverinfo = "none"
    ) %>%

    layout(
      showlegend = FALSE,
      # One trace per region; overlay stops plotly splitting
      # each row between regions
      barmode = "overlay",
      bargap = 0.2,
      xaxis = list(
        title = axis_title,
        range = c(axis_min, axis_end),
        fixedrange = TRUE
      ),
      yaxis = list(
        # Reversed so rank 1 is at the top
        range = c(top_n + 0.5, 0.5),
        showticklabels = FALSE,
        showgrid = FALSE,
        zeroline = FALSE,
        title = "",
        fixedrange = TRUE
      ),
      margin = list(l = 10, r = 10, t = 8, b = 35)
    ) %>%

    # Equal frame and transition times keep the motion steady
    animation_opts(
      frame = 400,
      transition = 400,
      easing = "linear",
      redraw = FALSE
    ) %>%

    # Play and pause are page buttons (ui/race_tab.R)
    animation_button(visible = FALSE) %>%

    # Year slider at the top, after the buttons
    animation_slider(
      x = 0.1,
      len = 0.9,
      y = 1,
      yanchor = "bottom",
      pad = list(t = 0, b = 10),
      font = list(size = 10),
      currentvalue = list(visible = FALSE)
    ) %>%

    config(displayModeBar = FALSE)

  # Same height for every bar, otherwise plotly sizes them
  # per region. Set after building: plot_ly(width = ) would
  # set the chart width instead.
  p <- plotly_build(p)
  set_bar_width <- function(traces) {
    lapply(traces, function(tr) {
      if (identical(tr$type, "bar")) tr$width <- 0.8
      tr
    })
  }
  p$x$data <- set_bar_width(p$x$data)
  p$x$frames <- lapply(p$x$frames, function(fr) {
    fr$data <- set_bar_width(fr$data)
    fr
  })

  p
}
