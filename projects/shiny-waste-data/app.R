#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)

# Load data
waw <- read_csv("data/WaW3.csv")

# Create global totals by year
global_msw <- waw %>%
  filter(
    INDICATOR_LABEL == "Municipal Solid Waste (MSW) Generation",
    UNIT_MEASURE_LABEL == "Tonnes per year"
  ) %>%
  summarise(
    across(
      matches("^\\d{4}$"),
      ~ sum(as.numeric(.), na.rm = TRUE)
    )
  )

# Convert to Year/Value format for charting
global_msw_long <- global_msw %>%
  pivot_longer(
    everything(),
    names_to = "Year",
    values_to = "Value"
  ) %>%
  mutate(
    Year = as.numeric(Year)
  ) %>%
  arrange(Year)

# User Interface
ui <- fluidPage(
  
  titlePanel("What a Waste 3.0"),
  
  h2("Global Municipal Solid Waste Generation"),
  
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

# Server
server <- function(input, output, session) {
  
  output$trendChart <- renderPlot({
    
    ggplot(
      global_msw_long,
      aes(
        x = Year,
        y = Value
      )
    ) +
      geom_line(linewidth = 1.2, colour = "#0072B2") +
      geom_point(colour = "#0072B2") +
      labs(
        x = "Year",
        y = "Tonnes per year",
        title = "Global Municipal Solid Waste Generation"
      ) +
      theme_minimal()
    
  })
  
  output$dataTable <- renderDT({
    
    datatable(
      global_msw_long,
      options = list(
        pageLength = 20
      )
    )
    
  })
  
}

shinyApp(ui, server)