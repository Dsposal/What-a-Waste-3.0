############################################################
# WHERE WASTE GOES TAB
#
# Sankey of municipal solid waste from income groups (or
# countries) to what happens to it. Click an income group
# to see its countries, then a country to see it alone.
# Data and chart are in R/waste_flows.R.
############################################################

flowsTab <- tabPanel(
  "Where waste goes",

  h3("Municipal solid waste by treatment and disposal"),

  fluidRow(

    column(
      2,
      radioButtons("flowYear", "Year", choices = flow_years, inline = TRUE)
    ),

    column(
      3,
      radioButtons(
        "flowMeasure",
        "Show",
        choices = c(
          "Million tonnes" = "tonnes",
          "Share of each group" = "share"
        ),
        inline = TRUE
      )
    ),

    column(
      2,
      selectInput(
        "flowIncome",
        "Income group",
        choices = c("All groups" = "All", rev(levels(policy_countries$income)))
      )
    ),

    column(
      3,
      selectInput("flowCountry", "Country", choices = flow_country_choices())
    ),

    column(
      2,
      style = "padding-top: 25px;",
      actionButton("flowReset", "Show all", class = "btn-sm")
    )

  ),

  # Fate colour key
  div(
    style = "margin-bottom: 6px; font-size: 13px;",
    lapply(
      list(
        c("Managed", fate_colours[["Incinerated"]]),
        c("Unclear", fate_colours[["Unaccounted for"]]),
        c("Lost to the environment", fate_colours[["Uncollected"]])
      ),
      function(k) {
        span(
          style = "margin-right: 14px; white-space: nowrap;",
          span(style = paste0(
            "display: inline-block; width: 11px; height: 11px;",
            "margin-right: 4px; background:", k[2], ";"
          )),
          k[1]
        )
      }
    ),
    span(class = "text-muted", "Click a group or country to look inside it.")
  ),

  plotlyOutput("flowSankey", height = "calc(100vh - 300px)"),

  p(class = "small text-muted", textOutput("flowNote", inline = TRUE))

)
