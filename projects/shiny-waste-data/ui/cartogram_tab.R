# Controls and explanatory copy for the Waste Transition animation.
# The animation and material-estimate calculations are implemented in
# R/waste_animation.R; keep the estimate caveat visible when adding controls.
cartogramTab <- tabPanel(
  "Waste Transition",
  
  h2("From Waste Amounts to Waste per Person"),
  
  p(
    class = "text-muted",
    "This animation compares waste amounts across countries in tonnes per year and kilograms per person per day. ",
    "For an individual material, those amounts are estimates derived from its MSW composition share. ",
    "Bubble sizes transition from ranking the selected waste amount in tonnes to ranking it per person. ",
    "Countries that shrink produce large volumes of waste mainly because of their population size, while countries that grow generate comparatively more waste per person."
  ),

  selectInput(
    "animationWasteType",
    "Waste type",
    choices = waste_animation_choices(waw_clean)
  ),

  p(
    class = "small text-muted",
    "For an individual material, both measures are estimates: the latest reported MSW composition share is applied to 2030 MSW generation. This assumes the reported share still represents the 2030 waste mix; hover over a country to see the share and its reporting year."
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
  
  div(
    style = "position: relative;",
    
    plotlyOutput(
      "dorlingPlot",
      height = "calc(100vh - 250px)"
    ),
    
    div(
      style = paste(
        "position: absolute; left: 8px; bottom: 8px; z-index: 10;",
        "background: rgba(255,255,255,0.92);",
        "padding: 10px 12px;",
        "border: 1px solid #e1e0d9;",
        "border-radius: 6px;",
        "font-size: 12px;"
      ),
      
      uiOutput("wasteTransitionLegend")
    )
  )
  
)