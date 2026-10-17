############################################################
# POLICY TAB
#
# EPR status and treatment performance, by country (map)
# and by income group (dot chart). Data and charts are in
# R/policy.R.
############################################################

policyTab <- tabPanel(
  "Policy",

  h3("EPR status and treatment performance"),

  radioButtons(
    "policyMetric",
    NULL,
    choices = c(
      "Controlled facilities" = "controlled",
      "Recycling" = "recycling"
    ),
    inline = TRUE
  ),

  h4("By country"),

  fluidRow(
    column(
      9,
      plotlyOutput("eprMap", height = "440px")
    ),
    column(
      3,
      uiOutput("eprKey")
    )
  ),

  p(
    class = "small text-muted",
    "Latest treatment survey per country. EPR status as recorded in 2026. White means not reported."
  ),

  h4("By income group"),

  plotlyOutput("eprGap", height = "400px"),

  p(
    class = "small text-muted",
    "Countries above 1 million people. Big dots are medians (3 or more countries)."
  )

)

# Map key, read like a small chart: EPR status on the
# vertical axis (one colour ramp per status), the selected
# metric on the horizontal axis
epr_key <- function(m) {

  info <- policy_metrics[[m]]

  ramp <- function(status) {
    div(
      style = "margin-bottom: 8px;",
      div(class = "small text-muted", status),
      div(style = sprintf(
        "height: 14px; border-radius: 3px; background: linear-gradient(90deg, %s, %s);",
        epr_ramps[[status]][1], epr_ramps[[status]][2]
      ))
    )
  }

  div(
    style = "padding-top: 40px; display: flex; gap: 6px;",

    # Vertical axis title, rotated to read bottom to top
    div(
      style = "writing-mode: vertical-rl; transform: rotate(180deg); text-align: center; font-weight: bold;",
      "EPR status"
    ),

    div(
      style = "flex: 1;",
      lapply(rev(epr_levels), ramp),
      div(
        class = "small text-muted",
        style = "display: flex; justify-content: space-between;",
        span("0%"),
        span(if (info$max < 100) paste0(info$max, "%+") else "100%")
      ),
      # Horizontal axis title
      div(
        style = "text-align: center; font-weight: bold; margin-top: 2px;",
        paste(info$label, "(%)")
      )
    )
  )

}
