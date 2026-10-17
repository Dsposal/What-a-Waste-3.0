############################################################
# TREATMENT GAP 2050 TAB
#
# Bubble map of the new waste that could end up outside
# controlled facilities by 2050 if today's shares stay the
# same. Data and map are in R/treatment_gap.R; the legend
# sits over the map's bottom left corner.
############################################################

treatmentGapTab <- tabPanel(
  "Treatment gap 2050",

  h3("New waste outside controlled facilities by 2050, if today's treatment stays the same"),

  p(
    class = "text-muted",
    "Each country's projected growth in tonnes from 2022 to 2050 (World Bank projections),",
    "times the share of its waste that does not go to controlled facilities today."
  ),

  radioButtons(
    "treatmentGapMeasure",
    "Count",
    choices = treatment_gap_measures,
    inline = TRUE
  ),

  div(
    style = "position: relative;",

    plotlyOutput("treatmentGapMap", height = "calc(100vh - 250px)"),

    div(
      style = paste(
        "position: absolute; left: 8px; bottom: 8px; z-index: 10;",
        "background: rgba(255, 255, 255, 0.92); padding: 10px 12px;",
        "border: 1px solid #e1e0d9; border-radius: 6px; font-size: 12px;"
      ),
      uiOutput("treatmentGapLegend")
    )
  )

)

# Legend: bubble sizes, what the colour means, and the grey
# fill for countries without a complete survey
treatment_gap_legend <- function(measure) {

  swatch <- function(colour, round = TRUE) {
    span(style = sprintf(
      "display: inline-block; width: 12px; height: 12px; margin-right: 6px; vertical-align: -1px; background: %s; border-radius: %s;",
      colour, if (round) "50%" else "2px"
    ))
  }

  # Three reference bubbles, drawn with the map's own scale
  sizes <- c(5, 25, 100)
  bubbles <- div(
    style = "display: flex; align-items: flex-end; gap: 10px; margin: 4px 0 8px;",
    lapply(sizes, function(mt) {
      d <- treatment_gap_px(mt)
      div(
        style = "text-align: center;",
        div(style = sprintf(
          "width: %.0fpx; height: %.0fpx; margin: 0 auto; border-radius: 50%%; border: 1px solid #888780;",
          d, d
        )),
        div(class = "text-muted", sprintf("%d Mt", mt))
      )
    })
  )

  colour_key <- if (measure == "both") {
    div(
      div(style = sprintf(
        "width: 230px; height: 10px; border-radius: 2px; background: linear-gradient(90deg, %s, %s);",
        treatment_gap_colours[["unclear"]], treatment_gap_colours[["lost"]]
      )),
      div(
        class = "text-muted",
        style = "display: flex; justify-content: space-between; width: 230px;",
        span("Mostly unclear"),
        span("Mostly uncollected or dumped")
      )
    )
  } else if (measure == "lost") {
    div(swatch(treatment_gap_colours[["lost"]]), "Uncollected or dumped")
  } else {
    div(swatch(treatment_gap_colours[["unclear"]]), "Unclear")
  }

  div(
    strong("New waste outside controlled facilities by 2050"),
    bubbles,
    colour_key,
    div(
      style = "margin-top: 8px;",
      swatch(treatment_gap_colours[["no_survey"]], round = FALSE),
      "No complete treatment survey"
    ),
    if (measure != "lost") {
      div(
        class = "text-muted",
        style = "margin-top: 6px; max-width: 230px;",
        "Unclear means unspecified landfill, other or unaccounted for."
      )
    }
  )

}
