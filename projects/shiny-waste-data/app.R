#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(DT)

sites <- read.csv("data/WaW3.csv")

ui <- fluidPage(
  
  titlePanel("What a Waste 3.0"),
  
  DTOutput("table")
  
)

server <- function(input, output, session) {
  
  output$table <- renderDT({
    datatable(sites)
  })
  
}

shinyApp(ui, server)
