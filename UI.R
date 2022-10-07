library(shiny)
shinyUI(fluidPage(
  titlePanel("Galton : Regression & Mean square error"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("sliderSLOPE", "Slide to minimize Mean Square Error", -0.3, 1.0, value = 0.25),
      checkboxInput("showNorm1", "Show/Hide line built whith normalized values ", value = FALSE),
      checkboxInput("showBest1", "Show/Hide line built whith best fit ", value = FALSE),
    ),
    mainPanel(
      plotOutput("plot1"),
#      h3("MSE value :"),
      textOutput("pred1"),
    )
  )
))