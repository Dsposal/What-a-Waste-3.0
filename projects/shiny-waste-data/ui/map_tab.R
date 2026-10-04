mapTab <- tabPanel(

  "Map",
  
  h2("Municipal Solid Waste Generation by Country"),
  
  selectInput(
    "mapYear",
    "Year",
    choices = mapYears,
    selected = "2030"
  ),
  
  sliderInput(
    "bubbleScale",
    "Bubble Scale",
    min = 150,
    max = 1250,
    value = 475,
    step = 25
  ),
  
  leafletOutput(
    "map",
    height = "700px"
  )
  
)