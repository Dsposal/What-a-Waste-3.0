# Standalone exploration helper, not sourced by app.R. It reshapes the
# source CSV's year columns into rows for ad hoc analysis; keep the source
# data unchanged and use the app's R/data.R for dashboard data loading.
library(tidyverse)
library(tidyr)

waw <- read_csv("data/WaW3.csv")

waw_long <- waw %>%
  pivot_longer(
    cols = matches("^\\d{4}$"),
    names_to = "Year",
    values_to = "Value"
  ) %>%
  filter(!is.na(Value))
