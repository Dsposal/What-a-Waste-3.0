

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
    selected = "Tonnes per year"
  ),
  
  actionButton(
    "animateDorling",
    "▶ Show Per-Capita View"
  ),
  
  plotOutput(
    "dorlingPlot",
    height = "700px"
  )
  
)