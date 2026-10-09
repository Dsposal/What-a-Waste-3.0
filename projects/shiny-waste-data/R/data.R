############################################################
# What a Waste 3.0
# Data Loading and Global Aggregations
#
# This file:
# - Loads the source WaW3 dataset
# - Defines supported projection years
# - Creates global MSW datasets used by charts
#
# No UI or visualisation logic should be placed here.
############################################################

library(readr)
library(dplyr)
library(tidyr)

# Main What a Waste 3.0 dataset
#
# Contains:
# - Country level waste indicators
# - Historical observations
# - Future projections
# - Multiple unit measures
#
# The REF_AREA_LABEL field is used as the country key
# throughout the application.

waw <- readr::read_csv("data/WaW3.csv")

# Create global totals by year
global_msw <- waw %>%
  filter(
    INDICATOR_LABEL == "Municipal Solid Waste (MSW) Generation",
    UNIT_MEASURE_LABEL == "Tonnes per year"
  ) %>%
  summarise(
    across(
      matches("^\\d{4}$"),
      ~ sum(as.numeric(.), na.rm = TRUE)
    )
  )

# Convert to Year/Value format for charting
global_msw_long <- global_msw %>%
  pivot_longer(
    everything(),
    names_to = "Year",
    values_to = "Value"
  ) %>%
  mutate(
    Year = as.numeric(Year)
  ) %>%
  arrange(Year)

# 2022 population per country. The dataset has no population
# column, but it is implied by the two generation units:
# tonnes / (kg per person per day * 365). Used to keep small
# territories out of per person rankings and medians.
country_population <- waw %>%
  filter(
    INDICATOR == "WM_MSW_GEN",
    UNIT_MEASURE %in% c("T_YR", "KG_PS_D"),
    !is.na(`2022`)
  ) %>%
  group_by(REF_AREA) %>%
  summarise(
    tonnes = first(as.numeric(`2022`[UNIT_MEASURE == "T_YR"])),
    kg = first(as.numeric(`2022`[UNIT_MEASURE == "KG_PS_D"])),
    .groups = "drop"
  ) %>%
  mutate(population = tonnes * 1000 / (kg * 365)) %>%
  select(REF_AREA, population)

# The maps don't need all years because not all the data is in
mapYears <- c(
  "2021",
  "2022",
  "2030",
  "2040",
  "2050"
)

# Build a simple forecast trend from trusted points
trusted_points <- global_msw_long %>%
  filter(Year %in% c(2022, 2030, 2040, 2050))

forecast_model <- lm(Value ~ Year, data = trusted_points)

forecast_line <- data.frame(
  Year = seq(
    min(global_msw_long$Year),
    max(global_msw_long$Year),
    by = 1
  )
)

forecast_line$Value <- predict(
  forecast_model,
  newdata = forecast_line
)