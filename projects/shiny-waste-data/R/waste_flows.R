############################################################
# WHERE WASTE GOES
#
# PURPOSE
# -------
# Data and chart for the "Where waste goes" tab: a Sankey
# diagram of municipal solid waste flowing from income
# groups (or countries) to what happens to it.
#
# AI NOTES
# --------
# Builds on R/policy.R (treatment_latest, policy_countries).
# Shiny loads R/ files in alphabetical order, so this file
# must sort after policy.R.
#
# Treatment shares are a percentage of ALL MSW, and include
# "Uncollected" and "Unaccounted for", so one survey covers
# the whole journey. A type missing from a survey counts as
# 0% (China's latest survey, for example, has no Recycling
# row), so the note under the chart says so.
#
# 2050 uses projected tonnes with today's shares held
# constant: a "nothing changes" scenario, not a forecast.
############################################################

library(dplyr)
library(tidyr)
library(plotly)

# Treatment types grouped into fates, in display order (top
# to bottom on the right of the chart)
fate_groups <- c(
  "Recycling"                             = "Recycled or composted",
  "Composting"                            = "Recycled or composted",
  "Anaerobic digestion (AD)"              = "Recycled or composted",
  "Incineration"                          = "Incinerated",
  "Refuse-derived fuel (RDF)"             = "Incinerated",
  "Sanitary landfill"                     = "Controlled landfill",
  "Controlled landfill"                   = "Controlled landfill",
  "Unspecified landfill"                  = "Unspecified landfill",
  "Mechanical biological treatment (MBT)" = "Other",
  "Other"                                 = "Other",
  "Unaccounted for"                       = "Unaccounted for",
  "Open dump"                             = "Open dump",
  "Uncollected"                           = "Uncollected"
)

fate_levels <- unique(unname(fate_groups))

# Teal = managed, grey = unclear, orange = lost to the
# environment
fate_colours <- c(
  "Recycled or composted" = "#0F6E56",
  "Incinerated"           = "#1D9E75",
  "Controlled landfill"   = "#5DCAA5",
  "Unspecified landfill"  = "#B4B2A9",
  "Other"                 = "#B4B2A9",
  "Unaccounted for"       = "#888780",
  "Open dump"             = "#F0997B",
  "Uncollected"           = "#D85A30"
)

flow_years <- c("2022", "2050")

# MSW tonnes per country for the two years shown
flow_generation <- waw_text %>%
  filter(INDICATOR == "WM_MSW_GEN", UNIT_MEASURE == "T_YR") %>%
  group_by(iso3 = REF_AREA) %>%
  summarise(
    # first non-blank value: some countries have extra rows
    across(all_of(flow_years), ~ first(na.omit(as.numeric(.x)))),
    .groups = "drop"
  ) %>%
  pivot_longer(all_of(flow_years), names_to = "year", values_to = "generated") %>%
  filter(!is.na(generated)) %>%
  inner_join(
    policy_countries %>% filter(!is.na(income)) %>% select(iso3, country, income),
    by = "iso3"
  )

# One row per country, year and fate
flows <- treatment_latest %>%
  mutate(fate = factor(fate_groups[type], levels = fate_levels)) %>%
  group_by(iso3, fate) %>%
  summarise(share = sum(share), .groups = "drop") %>%
  # each fate row meets both years
  inner_join(flow_generation, by = "iso3", relationship = "many-to-many") %>%
  mutate(tonnes = share * generated)

# How much of each income group the chart covers: countries
# with a treatment survey, and their share of 2022 tonnes
flow_coverage <- flow_generation %>%
  filter(year == "2022") %>%
  mutate(surveyed = iso3 %in% treatment_latest$iso3) %>%
  group_by(income) %>%
  summarise(
    countries = n(),
    surveyed_countries = sum(surveyed),
    tonnes_pct = 100 * sum(generated[surveyed], na.rm = TRUE) / sum(generated, na.rm = TRUE),
    .groups = "drop"
  )

# Countries to pick from, biggest first
flow_country_choices <- function(group = "All") {
  df <- flows %>% filter(year == "2022")
  if (group != "All") df <- df %>% filter(income == group)
  df <- df %>%
    group_by(iso3, country) %>%
    summarise(tonnes = sum(tonnes), .groups = "drop") %>%
    arrange(desc(tonnes))
  c("All countries" = "All", setNames(df$iso3, df$country))
}


############################################################
# CHART
############################################################

# Sankey. The left column is income groups, or the countries
# of one group (biggest 10, the rest pooled), or one country.
# 'measure' is "tonnes" or "share" (each source sums to 100).
# Try it in the console:
#   flow_sankey("2022", "tonnes")
flow_sankey <- function(year, measure, group = "All", iso = "All") {

  df <- flows %>% filter(year == !!year)

  # 'key' goes into customdata so a click can drill down
  if (iso != "All") {
    df <- df %>% filter(iso3 == iso) %>%
      mutate(source = country, key = "")
  } else if (group == "All") {
    df <- df %>%
      mutate(source = as.character(income), key = paste0("income:", income))
  } else {
    df <- df %>% filter(income == group)
    top <- df %>%
      group_by(iso3) %>%
      summarise(tonnes = sum(tonnes)) %>%
      slice_max(tonnes, n = 10) %>%
      pull(iso3)
    rest <- n_distinct(df$iso3[!df$iso3 %in% top])
    df <- df %>%
      mutate(
        source = if_else(iso3 %in% top, country, paste("Other", rest, "countries")),
        key = if_else(iso3 %in% top, paste0("country:", iso3), "")
      )
  }

  links <- df %>%
    group_by(source, key, fate) %>%
    summarise(tonnes = sum(tonnes), .groups = "drop") %>%
    group_by(source) %>%
    mutate(
      source_tonnes = sum(tonnes),
      value = if (measure == "share") 100 * tonnes / source_tonnes else tonnes / 1e6
    ) %>%
    ungroup() %>%
    filter(value > 0)

  # Sources biggest first, except income groups, which keep
  # their order (high income at the top). Pooled countries
  # go last.
  sources <- links %>%
    distinct(source, source_tonnes) %>%
    arrange(grepl("^Other \\d", source), desc(source_tonnes)) %>%
    pull(source)
  if (group == "All" && iso == "All") {
    sources <- rev(levels(policy_countries$income))
    sources <- sources[sources %in% links$source]
  }
  fates <- fate_levels[fate_levels %in% links$fate]

  # Fixed node positions keep the fates in a steady order.
  # y is the centre of each node, from 0 (top) to 1.
  node_y <- function(totals) {
    gap <- 0.02
    h <- totals / sum(totals) * (1 - gap * (length(totals) - 1))
    pmin(pmax(cumsum(h) - h / 2 + gap * (seq_along(h) - 1), 0.001), 0.999)
  }
  source_totals <- sapply(sources, function(s) sum(links$value[links$source == s]))
  fate_totals <- sapply(fates, function(f) sum(links$value[links$fate == f]))

  unit <- if (measure == "share") "%" else " Mt"
  label_value <- function(v) {
    if (measure == "share") "" else sprintf("  %s Mt", format(round(v), big.mark = ",", trim = TRUE))
  }

  node_labels <- c(
    paste0(sources, label_value(source_totals)),
    if (measure == "share") fates else paste0(fates, label_value(fate_totals))
  )
  node_keys <- c(
    links$key[match(sources, links$source)],
    rep("", length(fates))
  )

  plot_ly(
    type = "sankey",
    source = "flows",
    arrangement = "fixed",   # no dragging, so clicks register
    valueformat = ".0f",
    valuesuffix = unit,
    node = list(
      label = node_labels,
      customdata = node_keys,
      x = c(rep(0.001, length(sources)), rep(0.999, length(fates))),
      y = c(node_y(source_totals), node_y(fate_totals)),
      color = c(rep("#5F5E5A", length(sources)), unname(fate_colours[fates])),
      pad = 12,
      thickness = 14,
      line = list(width = 0),
      hovertemplate = "%{label}<extra></extra>"
    ),
    link = list(
      source = match(links$source, sources) - 1,
      target = length(sources) + match(as.character(links$fate), fates) - 1,
      value = links$value,
      customdata = links$key,
      label = paste(links$source, "to", tolower(links$fate)),
      color = paste0(unname(fate_colours[as.character(links$fate)]), "73"),
      hovertemplate = paste0(
        "%{label}<br><b>%{value:.0f}", unit, "</b><extra></extra>"
      )
    )
  ) %>%
    layout(
      font = list(size = 13),
      margin = list(l = 10, r = 10, t = 10, b = 10)
    ) %>%
    config(displayModeBar = FALSE) %>%
    event_register("plotly_click")

}

