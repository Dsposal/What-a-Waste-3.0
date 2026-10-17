

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
  
  sliderInput(
    "animationProgress",
    "Animation Progress",
    min = 0,
    max = 1,
    value = 0,
    step = 0.05
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