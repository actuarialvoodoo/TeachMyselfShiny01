library(shiny)
library(ggplot2)
library(tibble)
library(colourpicker)
library(stringr)

source("plot_data.R")
ui <- fluidPage(
    column(3,
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
    ),
    column(9, plotOutput("density"))
)

server <- function(input, output, session) {
    source("reactive_logic.R", local = TRUE)
}

shinyApp(ui, server)