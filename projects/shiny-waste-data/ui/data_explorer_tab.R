# Controls and outputs for the country/global time-series view.
# The selected area is processed in app.R; its dashed linear guide is a
# simple visualization based on the dataset's projection milestones.
dataExplorerTab <- tabPanel(
    "Data Explorer",
    
    h2("Municipal Solid Waste Generation"),
    
    selectInput(
      "area",
      "Country / Region",
      choices = sort(unique(waw$REF_AREA_LABEL)),
      selected = "Global"
    ),
    
    plotOutput("trendChart", height = "500px"),
    
    hr(),
    
    DTOutput("dataTable")
  
)