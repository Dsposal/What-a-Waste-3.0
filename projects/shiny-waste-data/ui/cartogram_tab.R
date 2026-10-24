cartogramTab <- tabPanel(
  "Waste Transition",
  
  h2("From Total Waste to Waste Per Person"),
  
  p(
    class = "text-muted",
    "This animation shows how the global picture changes when waste is viewed from two different perspectives. ",
    "At the start, bubble size represents each country's total municipal waste generation (tonnes per year). ",
    "As the animation progresses, bubbles transition to represent waste generation per person (kilograms per person per day). ",
    "Countries that shrink produce large volumes of waste mainly because of their population size, while countries that grow generate comparatively more waste per person."
  ),
  
  sliderInput(
    "animationProgress",
    "Animation Progress",
    min = 0,
    max = 1,
    value = 0,
    step = 0.001
  ),
  
  selectInput(
    "animationSpeed",
    "Animation Speed",
    choices = c(
      "Very Slow" = 1,
      "Slow" = 2,
      "Normal" = 3,
      "Fast" = 4,
      "Very Fast" = 5
    ),
    selected = 3
  ),
  
  actionButton(
    "animateDorling",
    "▶ Play Animation"
  ),
  
  plotlyOutput(
    "dorlingPlot",
    height = "700px"
  )
  
)