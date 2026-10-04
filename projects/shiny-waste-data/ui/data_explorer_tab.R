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