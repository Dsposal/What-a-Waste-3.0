############################################################
# EPR AND TREATMENT PERFORMANCE
#
# PURPOSE
# -------
# Data and charts for the "EPR and treatment" tab: each country's
# extended producer responsibility (EPR) status, and the
# share of its waste sent to controlled facilities or
# recycled, from its latest treatment survey.
#
# AI NOTES
# --------
# EPR answers are text in the year columns, which the
# numeric read in R/data.R turns into NA, so this file
# reads the CSV as text.
#
# A survey with no "Recycling" row has not reported
# recycling. That is NA, not 0%.
############################################################

library(readr)
library(dplyr)
library(tidyr)
library(plotly)

waw_text <- read_csv(
  "data/WaW3.csv",
  col_types = cols(.default = col_character())
)

policy_years <- grep("^\\d{4}$", names(waw_text), value = TRUE)

income_levels <- c(
  LIC  = "Low income",
  LMIC = "Lower middle income",
  UMIC = "Upper middle income",
  HIC  = "High income"
)

epr_levels <- c("None recorded", "In progress", "Legally binding")

# Treatment types that count as a controlled facility (the
# idea behind SDG 11.6.1). Unspecified landfill is left out.
controlled_types <- c(
  "Sanitary landfill",
  "Controlled landfill",
  "Incineration",
  "Recycling",
  "Composting",
  "Anaerobic digestion (AD)",
  "Refuse-derived fuel (RDF)",
  "Mechanical biological treatment (MBT)"
)

# Country, income group and 2022 population (R/data.R)
policy_countries <- waw_text %>%
  filter(INDICATOR == "WM_MSW_GEN") %>%
  group_by(iso3 = REF_AREA) %>%
  summarise(
    country = first(REF_AREA_LABEL),
    income = first(INCOME_GR[INCOME_GR %in% names(income_levels)]),
    .groups = "drop"
  ) %>%
  mutate(income = factor(income_levels[income], levels = income_levels)) %>%
  left_join(country_population, by = c("iso3" = "REF_AREA"))

# EPR status as recorded in the dataset (2026). A country
# without EPR rows is "None recorded", not a confirmed "no".
epr_rows <- waw_text %>%
  filter(INDICATOR == "WM_EPR_SYS") %>%
  mutate(answer = coalesce(!!!syms(rev(policy_years))))

epr_binding <- epr_rows %>%
  filter(
    EV_CRITERIA_LABEL == "Operational status",
    answer == "Legally-binding regulations in place"
  ) %>%
  pull(REF_AREA)

epr_any <- epr_rows %>%
  filter(EV_CRITERIA_LABEL == "Existence") %>%
  pull(REF_AREA)

# All treatment rows, one per country, year and type
treatment_rows <- waw_text %>%
  filter(INDICATOR == "WM_MSW_TREAT") %>%
  pivot_longer(
    all_of(policy_years),
    names_to = "year",
    values_to = "pct",
    values_drop_na = TRUE
  ) %>%
  mutate(
    year = as.integer(year),
    pct = suppressWarnings(as.numeric(pct))
  ) %>%
  filter(!is.na(pct)) %>%
  group_by(REF_AREA, year) %>%
  filter(sum(pct) > 0) %>%   # some surveys are all zeros
  ungroup() %>%
  select(iso3 = REF_AREA, year, type = TREATMENT_TYPE_LABEL, pct)

# Pick one complete survey for a country. Some countries
# report collected waste in one year and "Uncollected" in
# another (Colombia: 86% in 2022, 14% uncollected in 2023),
# so the latest year alone can be a fragment.
#  1. Latest year, if it adds up to 90% or more.
#  2. Otherwise the latest year plus the latest earlier year
#     with different treatment types, if together they add
#     up to 90-110%.
#  3. Otherwise the latest year that adds up on its own.
#  4. Otherwise none: the country has no complete survey.
pick_survey <- function(rows) {

  totals <- rows %>%
    group_by(year) %>%
    summarise(total = sum(pct), .groups = "drop") %>%
    arrange(desc(year))

  latest <- rows %>% filter(year == totals$year[1])
  if (totals$total[1] >= 90) return(latest)

  for (y in totals$year[-1]) {
    other <- rows %>% filter(year == y)
    if (any(other$type %in% latest$type)) next
    combined <- bind_rows(latest, other)
    if (between(sum(combined$pct), 90, 110)) return(combined)
    break
  }

  complete <- totals %>% filter(total >= 90)
  if (nrow(complete) > 0) return(rows %>% filter(year == complete$year[1]))

  rows[0, ]

}

# One complete survey per country, one row per treatment
# type. 'share' is the fraction of all MSW (the shares add
# up to 1 per country). 'survey_year' is the year holding
# most of the survey. Shared with other tabs.
treatment_latest <- treatment_rows %>%
  group_by(iso3) %>%
  group_modify(~ pick_survey(.x)) %>%
  mutate(
    survey_year = first(year[which.max(ave(pct, year, FUN = sum))]),
    share = pct / sum(pct)
  ) %>%
  ungroup() %>%
  select(iso3, survey_year, type, share)

policy_treatment <- treatment_latest %>%
  group_by(iso3) %>%
  summarise(
    survey_year = first(survey_year),
    controlled = 100 * sum(share[type %in% controlled_types]),
    recycling = if (any(type == "Recycling")) {
      100 * sum(share[type == "Recycling"])
    } else {
      NA_real_
    },
    .groups = "drop"
  )

policy <- policy_countries %>%
  mutate(
    epr = case_when(
      iso3 %in% epr_binding ~ "Legally binding",
      iso3 %in% epr_any ~ "In progress",
      TRUE ~ "None recorded"
    ),
    epr = factor(epr, levels = epr_levels)
  ) %>%
  left_join(policy_treatment, by = "iso3")


############################################################
# CHARTS
############################################################

# The two indicators most countries report. 'max' caps the
# map's colour scale.
policy_metrics <- list(
  controlled = list(label = "Controlled facilities", max = 100),
  recycling  = list(label = "Recycling", max = 50)
)

# One hue per EPR status. On the map it runs from pale (low
# value) to full (high value); the dots use the full hue.
epr_ramps <- list(
  "Legally binding" = c("#c9d6e8", "#184f95"),
  "In progress"     = c("#c8e0d6", "#0e7a54"),
  "None recorded"   = c("#ecd2c6", "#b8441a")
)
epr_colours <- sapply(epr_ramps[epr_levels], `[`, 2)

# Map: one choropleth layer per EPR status, each with its
# own colour ramp
epr_map <- function(m) {

  info <- policy_metrics[[m]]

  df <- policy %>%
    filter(!is.na(.data[[m]])) %>%
    mutate(
      value = pmin(.data[[m]], info$max),
      hover = sprintf(
        "<b>%s</b><br>EPR: %s<br>%s: %.0f%% (survey %d)",
        country, epr, info$label, .data[[m]], survey_year
      )
    )

  p <- plot_geo()

  for (status in epr_levels) {
    layer <- df %>% filter(epr == status)
    if (nrow(layer) == 0) next
    p <- p %>%
      add_trace(
        data = layer,
        type = "choropleth",
        locations = ~iso3,
        z = ~value,
        zmin = 0,
        zmax = info$max,
        colorscale = list(
          list(0, epr_ramps[[status]][1]),
          list(1, epr_ramps[[status]][2])
        ),
        showscale = FALSE,
        text = ~hover,
        hoverinfo = "text",
        marker = list(line = list(color = "white", width = 0.5))
      )
  }

  p %>%
    layout(
      geo = list(
        showframe = FALSE,
        showcoastlines = FALSE,
        showcountries = TRUE,
        countrycolor = "#c3c2b7",
        projection = list(type = "natural earth"),
        lataxis = list(range = c(-58, 85))
      ),
      margin = list(l = 0, r = 0, t = 0, b = 0)
    ) %>%
    config(displayModeBar = FALSE)

}

# Income group chart: faint dots are countries, big dots are
# the median for each EPR status, and the grey bar spans the
# gap between medians. Countries under 1 million people are
# left out: in some groups they are the majority and would
# set the median on their own.
epr_gap_chart <- function(m) {

  info <- policy_metrics[[m]]

  df <- policy %>%
    filter(
      !is.na(.data[[m]]),
      !is.na(income),
      population >= 1e6
    ) %>%
    mutate(value = .data[[m]])

  set.seed(5)
  countries <- df %>%
    mutate(
      y = as.integer(income) + runif(n(), -0.2, 0.2),
      hover = sprintf("<b>%s</b><br>%s: %.0f%%<br>EPR: %s", country, info$label, value, epr)
    )

  # A median of one or two countries is not a pattern
  medians <- df %>%
    group_by(income, epr) %>%
    summarise(median = median(value), n = n(), .groups = "drop") %>%
    filter(n >= 3) %>%
    mutate(y = as.integer(income))

  gaps <- medians %>%
    group_by(y) %>%
    summarise(lo = min(median), hi = max(median), .groups = "drop")

  groups <- df %>% count(income)

  p <- plot_ly() %>%
    add_markers(
      data = countries,
      x = ~value,
      y = ~y,
      color = ~epr,
      colors = unname(epr_colours),
      text = ~hover,
      hoverinfo = "text",
      showlegend = FALSE,
      marker = list(size = 6, opacity = 0.3)
    ) %>%
    add_segments(
      data = gaps,
      x = ~lo,
      xend = ~hi,
      y = ~y,
      yend = ~y,
      inherit = FALSE,
      line = list(color = "#c3c2b7", width = 6),
      hoverinfo = "none",
      showlegend = FALSE
    )

  for (status in epr_levels) {
    s <- medians %>% filter(epr == status)
    if (nrow(s) == 0) next
    p <- p %>%
      add_markers(
        data = s,
        x = ~median,
        y = ~y,
        inherit = FALSE,
        name = status,
        text = sprintf("<b>%s</b>, %s<br>Median %.0f%% (%d countries)", status, s$income, s$median, s$n),
        hoverinfo = "text",
        marker = list(
          color = epr_colours[[status]],
          size = 10 + 2 * sqrt(s$n),
          line = list(color = "white", width = 2)
        )
      )
  }

  p %>%
    layout(
      xaxis = list(
        title = paste(info$label, "(%)"),
        range = c(-3, 103),
        ticksuffix = "%",
        zeroline = FALSE,
        fixedrange = TRUE
      ),
      yaxis = list(
        title = "",
        tickvals = as.integer(groups$income),
        ticktext = sprintf("%s (%d)", groups$income, groups$n),
        ticksuffix = "   ",
        range = c(0.4, length(income_levels) + 0.6),
        showgrid = FALSE,
        zeroline = FALSE,
        fixedrange = TRUE
      ),
      legend = list(orientation = "h", x = 0, y = 1.02, yanchor = "bottom"),
      margin = list(l = 10, r = 10, t = 10, b = 40)
    ) %>%
    config(displayModeBar = FALSE)

}
