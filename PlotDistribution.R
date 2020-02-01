library(shiny)
library(ggplot2)
library(tibble)
library(colourpicker)

ui <- tagList(
  selectInput("dist", 
              label = "Distribution", 
              choices = c("Select a distribution" = "",
                          "Normal" = "norm",
                          "Lognormal" = "lnorm",
                          "Expontential" = "exp")),
  uiOutput("params"),
  colourInput("color", 
              label = "What color should it be?", 
              value = "black",
              showColour = "both"),
  plotOutput("density")
)

server <- function(input, output, session) {
    source("reactive_logic.R")
}

shinyApp(ui, server)