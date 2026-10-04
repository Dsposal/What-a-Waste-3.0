############################################################
# COUNTRY RACE TAB
#
# Animated ranking of the countries generating the most
# municipal solid waste, 2022 to 2050. Data and chart are
# in R/race.R.
############################################################

raceTab <- tabPanel(
  "Country race",

  h3("Municipal solid waste generation by country, 2022 to 2050"),

  # Controls and colour key on one row so the tab fits on
  # one screen
  fluidRow(

    column(
      4,
      radioButtons(
        "raceUnit",
        NULL,
        choices = c(
          "Tonnes per year",
          "Kilograms per person per day"
        ),
        selected = "Tonnes per year",
        inline = TRUE
      )
    ),

    column(
      2,
      selectInput(
        "raceTop",
        NULL,
        choices = c(
          "Top 10" = 10,
          "Top 15" = 15,
          "Top 20" = 20
        ),
        selected = 15
      )
    ),

    # Region colour key
    column(
      6,
      lapply(names(race_colours), function(r) {
        span(
          style = "margin-right: 12px; white-space: nowrap; font-size: 13px;",
          span(style = paste0(
            "display: inline-block; width: 11px; height: 11px;",
            "margin-right: 4px; background:", race_colours[[r]], ";"
          )),
          r
        )
      })
    )

  ),

  # Chart, with play and pause over its top left corner
  div(
    style = "position: relative;",

    # Height follows the window
    plotlyOutput(
      "racePlot",
      height = "calc(100vh - 240px)"
    ),

    div(
      style = "position: absolute; top: 6px; left: 10px; z-index: 10;",

      # Plotly.animate(chart, null) plays from the current
      # year; [null] stops it
      tags$button(
        type = "button",
        class = "btn btn-default btn-sm",
        title = "Play",
        onclick = paste0(
          "Plotly.animate('racePlot', null, {fromcurrent: true,",
          " mode: 'immediate', frame: {duration: 400, redraw: false},",
          " transition: {duration: 400, easing: 'linear'}});"
        ),
        icon("play")
      ),

      tags$button(
        type = "button",
        class = "btn btn-default btn-sm",
        title = "Pause",
        onclick = paste0(
          "Plotly.animate('racePlot', [null], {mode: 'immediate',",
          " frame: {duration: 0, redraw: false},",
          " transition: {duration: 0}});"
        ),
        icon("pause")
      )
    )
  ),

  p(
    class = "small text-muted",
    "2030 to 2050 are World Bank projections; years in between are interpolated.",
    "Per person view: countries above 1 million people."
  )

)
