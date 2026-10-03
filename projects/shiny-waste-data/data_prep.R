library(tidyverse)
library(tidyr)

waw <- read_csv("data/waw3.csv")

waw_long <- waw %>%
  pivot_longer(
    cols = matches("^\\d{4}$"),
    names_to = "Year",
    values_to = "Value"
  ) %>%
  filter(!is.na(Value))
