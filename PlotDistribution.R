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
    output$params <- renderUI({
        req(input$dist)
        dfun <- get(paste0("d", input$dist))
        params <- names(formals(dfun))
        use_params <- params[!(params %in% c("x", "log"))]
        numInputs <- list()
        for(i in seq_along(use_params)) {
            numInputs[[i]] <- numericInput(
                paste0("param_", use_params[i]), 
                label = paste("Enter ", use_params[i], ": "), 
                value = 1
            )
        }
        class(numInputs) <- c("shiny.tag.list", "list")
        numInputs
    })
    output$density <- renderPlot({
        req(input$dist)
        dfun <- get(paste0("d", input$dist))
        qfun <- get(paste0("q", input$dist))
        percentiles <- seq_len(100)/100 - 0.005
        vals <- qfun(percentiles)
        probs <- dfun(vals)
        color <- input$color
        dat <- tibble(vals = vals, probs = probs)
        ggplot(dat, aes(x = vals, y = probs)) + geom_line(color = color)
    }) 
    options(shiny.reactlog = TRUE)
}

shinyApp(ui, server)