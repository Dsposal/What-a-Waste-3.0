mapTab <- tabPanel(

  "Map",
  
  h2("Municipal Solid Waste Generation by Country"),
  
  selectInput(
    "mapYear",
    "Year",
    choices = mapYears,
    selected = "2030"
  ),
  
  selectInput(
    "mapPalette",
    "Colour Palette",
    choices = c(
      "viridis",
      "magma",
      "plasma",
      "inferno",
      "cividis"
    ),
    selected = "viridis"
  ),
  
  selectInput(
    "mapScale",
    "Scale Method",
    choices = c(
      "Natural",
      "Logarithmic",
      "Quantiles",
      "Percentile"
    ),
    selected = "Natural"
  ),
  
  leafletOutput(
    "map",
    height = "700px"
  )
  
)