############################################################
# TREATMENT GAP 2050
#
# PURPOSE
# -------
# Data and map for the "Treatment gap 2050" tab: where
# the most new waste outside controlled facilities could
# appear by 2050 if nothing changes.
#
# For each country:
#   growth            = 2050 tonnes - 2022 tonnes
#   new uncontrolled  = growth x today's share NOT going to
#                       controlled facilities
# split into uncollected or dumped, and unclear (unspecified
# landfill, other, unaccounted for).
#
# AI NOTES
# --------
# Builds on R/policy.R (treatment_latest, controlled_types,
# policy_countries). Shiny loads R/ files in alphabetical
# order, so this file must sort after policy.R.
#
# "If nothing changes" means today's shares applied to the
# extra tonnes: a scenario, not a forecast.
############################################################

library(dplyr)
library(tidyr)
library(plotly)

lost_types <- c("Uncollected", "Open dump")

# Today's split per country, from its latest complete survey
treatment_gap_shares <- treatment_latest %>%
  group_by(iso3) %>%
  summarise(
    controlled = sum(share[type %in% controlled_types]),
    lost = sum(share[type %in% lost_types]),
    .groups = "drop"
  ) %>%
  mutate(unclear = pmax(1 - controlled - lost, 0))

# MSW tonnes in 2022 and 2050
treatment_gap_tonnes <- waw_text %>%
  filter(INDICATOR == "WM_MSW_GEN", UNIT_MEASURE == "T_YR") %>%
  group_by(iso3 = REF_AREA) %>%
  summarise(
    # first non-blank value: some countries have extra rows
    t2022 = first(na.omit(as.numeric(`2022`))),
    t2050 = first(na.omit(as.numeric(`2050`))),
    .groups = "drop"
  ) %>%
  filter(!is.na(t2022), !is.na(t2050))

treatment_gap <- treatment_gap_shares %>%
  inner_join(treatment_gap_tonnes, by = "iso3") %>%
  inner_join(
    policy_countries %>% filter(!is.na(income)) %>% select(iso3, country, income),
    by = "iso3"
  ) %>%
  mutate(
    growth = pmax(t2050 - t2022, 0),   # a few countries shrink
    new_lost = growth * lost,
    new_unclear = growth * unclear,
    new_uncontrolled = new_lost + new_unclear
  ) %>%
  arrange(desc(new_uncontrolled))

# Countries with growth but no complete treatment survey
# (Pakistan, Nigeria and others). They can't be sized, so
# the map fills them light grey rather than leaving them out
# without a word.
treatment_gap_unranked <- treatment_gap_tonnes %>%
  filter(!iso3 %in% treatment_gap$iso3) %>%
  inner_join(
    policy_countries %>% filter(!is.na(income)) %>% select(iso3, country, income),
    by = "iso3"
  ) %>%
  mutate(growth = pmax(t2050 - t2022, 0)) %>%
  filter(growth > 0)


############################################################
# MAP
############################################################

# What the bubbles count. "both" adds the two together.
treatment_gap_measures <- c(
  "Uncollected or dumped and unclear" = "both",
  "Uncollected or dumped" = "lost",
  "Unclear" = "unclear"
)

treatment_gap_colours <- c(
  unclear   = "#B4B2A9",
  lost      = "#D85A30",
  no_survey = "#E4E2DA"
)

# Bubble sizes. Area follows tonnes; the biggest bubble (the
# "both" total for India) is 70px across whatever is picked,
# so bubbles stay comparable between the choices. The legend
# in ui/treatment_gap_tab.R uses the same function.
treatment_gap_max_mt <- max(treatment_gap$new_uncontrolled) / 1e6
treatment_gap_max_px <- 70
treatment_gap_px <- function(mt) {
  treatment_gap_max_px * sqrt(mt / treatment_gap_max_mt)
}

# Bubble map. In "both" mode the colour shows how much of a
# bubble is uncollected or dumped (grey = mostly unclear,
# orange = mostly uncollected or dumped).
# Try it in the console:
#   treatment_gap_map("both")
treatment_gap_map <- function(measure = "both") {

  df <- treatment_gap %>%
    mutate(
      mt = switch(measure,
        both = new_uncontrolled,
        lost = new_lost,
        unclear = new_unclear
      ) / 1e6,
      lost_pct = 100 * new_lost / new_uncontrolled,
      hover = sprintf(
        paste0(
          "<b>%s</b><br>",
          "New waste outside controlled facilities by 2050: %.1f Mt<br>",
          "  uncollected or dumped: %.1f Mt<br>",
          "  unclear: %.1f Mt<br>",
          "Growth 2022 to 2050: %.1f Mt<br>",
          "Controlled facilities today: %.0f%%"
        ),
        country, new_uncontrolled / 1e6, new_lost / 1e6,
        new_unclear / 1e6, growth / 1e6, 100 * controlled
      )
    ) %>%
    filter(mt > 0)

  unranked <- treatment_gap_unranked %>%
    mutate(hover = sprintf(
      "<b>%s</b><br>Growth 2022 to 2050: %.1f Mt<br>No complete treatment survey",
      country, growth / 1e6
    ))

  bubble_colour <- if (measure == "both") {
    list(
      color = df$lost_pct,
      cmin = 0,
      cmax = 100,
      colorscale = list(
        list(0, treatment_gap_colours[["unclear"]]),
        list(1, treatment_gap_colours[["lost"]])
      ),
      showscale = FALSE   # key is in the legend beside the map
    )
  } else {
    list(color = treatment_gap_colours[[measure]])
  }

  plot_geo() %>%
    add_trace(
      data = unranked,
      type = "choropleth",
      locations = ~iso3,
      z = 1,
      colorscale = list(
        list(0, treatment_gap_colours[["no_survey"]]),
        list(1, treatment_gap_colours[["no_survey"]])
      ),
      showscale = FALSE,
      text = ~hover,
      hoverinfo = "text",
      marker = list(line = list(color = "white", width = 0.5))
    ) %>%
    add_trace(
      data = df,
      type = "scattergeo",
      mode = "markers",
      locations = ~iso3,
      locationmode = "ISO-3",
      text = ~hover,
      hoverinfo = "text",
      marker = c(
        list(
          size = df$mt,
          sizemode = "area",
          # plotly's area rule: diameter = sqrt(2 * size / sizeref)
          sizeref = 2 * treatment_gap_max_mt / treatment_gap_max_px^2,
          sizemin = 3,
          opacity = 0.85,
          line = list(color = "white", width = 1)
        ),
        bubble_colour
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
        projection = list(type = "natural earth"),
        lataxis = list(range = c(-58, 85))
      ),
      margin = list(l = 0, r = 0, t = 0, b = 0)
    ) %>%
    config(displayModeBar = FALSE)

}
