

cartogramTab <- tabPanel(
  "Cartogram",
  
  h2("Dorling Cartogram"),
  
  selectInput(
    "cartogramUnit",
    "Measure",
    choices = c(
      "Tonnes per year",
      "Kilograms per person per day"
    ),
    selected = "Kilograms per person per day"
  ),
  
  plotOutput(
    "dorlingPlot",
    height = "700px"
  )
  
)