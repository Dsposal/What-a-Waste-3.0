############################################################
# E-WASTE EXPLORER
# app.R — Shiny entry point
#
# Study focus: e-waste and the electronics trade, with
# China, the USA and India (the largest producers in
# What a Waste 3.0) contrasted against their population.
#
# FILE MAP
# --------
# R/data.R      load + reshape WaW3 (pure functions)
# R/trade.R     load + summarise Chatham House trade csv
# R/lookups.R   ISO codes, focus countries, colours
# R/classes.R   EwasteStore (R6) + ewaste_profile (S3)
# R/plots.R     plotly chart functions
# ui/tabs.R     one function per tab
#
# AI NOTES
# --------
# Keep data logic out of this file. The server should only
# call store methods and plot_* functions.
############################################################

library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(DT)

for (f in c("R/lookups.R", "R/data.R", "R/trade.R", "R/classes.R",
            "R/plots.R", "ui/tabs.R")) source(f)

store <- EwasteStore$new(
  waw_path   = "data/WaW3.csv",
  trade_path = "data/trade_electronics.csv"   # optional
)

country_choices <- with(store$countries %>% filter(has_both) %>% arrange(country),
                        setNames(iso3, country))

# ---------------------------------------------------------- UI
ui <- page_navbar(
  title = "E-waste explorer",
  theme = bs_theme(version = 5, base_font = font_collection(font_google("Inter", local = FALSE), "system-ui", "sans-serif"),
                   primary = "#2a78d6", bg = "#fcfcfb", fg = "#2b2b29"),
  sidebar = sidebar(
    selectizeInput("focus", "Focus countries", choices = country_choices,
                   selected = FOCUS_DEFAULT, multiple = TRUE,
                   options = list(maxItems = 8)),
    actionButton("reset", "Back to China · USA · India", class = "btn-sm"),
    helpText("China, the USA and India keep fixed colours. Extra countries show in dark grey with labels.")
  ),
  tab_overview(), tab_per_person(), tab_trade(), tab_data(), tab_about()
)

# ------------------------------------------------------ server
server <- function(input, output, session) {

  observeEvent(input$reset, updateSelectizeInput(session, "focus", selected = FOCUS_DEFAULT))

  focus_iso <- reactive({
    validate(need(length(input$focus) > 0, "Pick at least one country."))
    input$focus
  })
  focus_df <- reactive(store$focus(focus_iso()))

  # Trade data lives in a reactiveVal so uploads re-render
  trade_rv <- reactiveVal(store$trade)
  observeEvent(input$trade_file, {
    tryCatch({
      store$set_trade(input$trade_file$datapath)
      trade_rv(store$trade)
    }, error = function(e) showNotification(conditionMessage(e), type = "error", duration = 15))
  })

  # ---- Overview
  output$profile_cards <- renderUI({
    profiles <- lapply(focus_iso(), store$profile)
    boxes <- lapply(profiles, function(p) {
      value_box(
        title = p$country,
        value = sprintf("%.1f million t", p$tonnes / 1e6),
        p(sprintf("%.1f kg per person · %s data", p$kg_per_person, p$year)),
        p(sprintf("%.1f%% of e-waste, %.1f%% of people",
                  100 * p$share_ewaste, 100 * p$share_population)),
        theme = value_box_theme(bg = "#ffffff", fg = "#2b2b29"),
        showcase = div(style = sprintf("width:6px;height:60px;border-radius:3px;background:%s",
                                       country_colour(p$iso3, focus_iso())))
      )
    })
    do.call(layout_columns, c(list(fill = FALSE), boxes))
  })

  output$share_contrast <- renderPlotly(plot_share_contrast(focus_df()))

  output$share_note <- renderText({
    s <- store$combined_share(focus_iso())
    sprintf("Together these countries produce %.0f%% of reported e-waste but hold %.0f%% of the population.",
            100 * s$share_ewaste, 100 * s$share_population)
  })

  # ---- Per person
  output$per_person <- renderPlotly(
    plot_per_person(focus_df(), store$median_per_person(), focus_iso()))
  output$scatter <- renderPlotly(plot_population_scatter(store$countries, focus_iso()))

  # ---- Trade
  output$trade_year_ui <- renderUI({
    req(trade_rv())
    yrs <- store$years_available()
    if (length(yrs)) selectInput("trade_year", "Year", yrs, selected = yrs[1])
  })
  trade_year <- reactive(if (is.null(input$trade_year)) NULL else as.integer(input$trade_year))

  output$trade_status <- renderText({
    if (is.null(trade_rv())) "No trade file loaded yet."
    else sprintf("%s trade rows loaded.", format(nrow(trade_rv()), big.mark = ","))
  })

  output$trade_embed <- renderUI({
    tags$iframe(src = trade_embed_url(input$embed_year, input$trade_units),
                width = "100%", height = "380", frameborder = "0", scrolling = "no",
                style = "border:0;display:block")
  })

  output$trade_balance <- renderPlotly({
    validate(need(trade_rv(), "Load a trade csv to see exports and imports."))
    bal <- tryCatch(trade_balance(trade_rv(), focus_iso(), trade_year(), input$trade_units),
                    error = function(e) validate(need(FALSE, conditionMessage(e))))
    plot_trade_balance(bal, store$countries, input$trade_units)
  })

  # extract_waste(): tonnes traded per country + population
  waste_df <- reactive({
    req(trade_rv())
    tryCatch(extract_waste(trade_rv(), store$countries, trade_year()),
             error = function(e) validate(need(FALSE, conditionMessage(e))))
  })

  output$export_vs_generated <- renderPlotly({
    validate(need(trade_rv(), "Load a weight-units trade csv."))
    plot_export_vs_generated(waste_df(), focus_iso())
  })

  output$waste_table <- renderDT({
    validate(need(trade_rv(), "Load a weight-units trade csv."))
    d <- waste_df() %>%
      transmute(Country = country, ISO3 = iso3,
                `Exported (t)` = round(exported_t), `Imported (t)` = round(imported_t),
                `Net import (t)` = round(net_import_t),
                `E-waste generated (t)` = round(ewaste_tonnes),
                `Population (m)` = round(population / 1e6, 1),
                `Net import kg/person` = round(net_import_kg_per_person, 2),
                `Exported % of generated` = exported_pct_of_generated)
    datatable(d, rownames = FALSE, filter = "top", options = list(pageLength = 10)) %>%
      formatPercentage("Exported % of generated", 1)
  })

  output$partner_cards <- renderUI({
    req(trade_rv())
    cards <- lapply(focus_iso(), function(i)
      card(plotlyOutput(paste0("partners_", i), height = "300px")))
    do.call(layout_columns, c(list(col_widths = 4), cards))
  })

  observe({
    req(trade_rv())
    flows <- tryCatch(
      top_flows(trade_rv(), focus_iso(), n = 8, direction = input$flow_dir,
                yr = trade_year(), measure = input$trade_units),
      error = function(e) NULL)
    req(flows)
    for (i in focus_iso()) local({
      iso <- i
      output[[paste0("partners_", iso)]] <- renderPlotly(
        plot_top_partners(flows, iso, input$flow_dir, input$trade_units))
    })
  })

  # ---- Data
  table_df <- reactive({
    store$countries %>%
      transmute(Country = country, ISO3 = iso3, Region = region, Year = ewaste_year,
                `E-waste (t)` = round(ewaste_tonnes),
                `Population (m)` = round(population / 1e6, 1),
                `kg / person` = round(kg_per_person, 1),
                `Share e-waste` = share_ewaste, `Share population` = share_population,
                `E-waste EPR` = ewaste_epr)
  })
  output$table <- renderDT(
    datatable(table_df(), rownames = FALSE, filter = "top",
              options = list(pageLength = 15, order = list(list(4, "desc")))) %>%
      formatPercentage(c("Share e-waste", "Share population"), 2) %>%
      formatRound("E-waste (t)", 0)
  )
  output$download_csv <- downloadHandler(
    filename = function() "ewaste_by_country.csv",
    content = function(file) readr::write_csv(table_df(), file)
  )
}

shinyApp(ui, server)
