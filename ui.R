library(shiny)

shinyUI(fluidPage(
  
  title = "Seasonality of LA City Revenue",
  
  
  fluidRow(
    column(10,offset=1,
           plotOutput('tsPlot'))),
  
  #hr(),
  
  fluidRow(
    column(4,offset=5,
           h4("Seasonality of LA City Revenue"),
           sliderInput('periods', 'Select a number of periods to analyze:', 
                       min=25, max=56, value=56))
    )
  )
)
