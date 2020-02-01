library(shiny)
library(ggplot2)
library(tibble)

ui <- tagList(
  selectInput("dist", 
              "Distribution", 
              choices = c("Select a distribution" = "",
                          "Normal" = "norm",
                          "Lognormal" = "lnorm",
                          "Expontential" = "exp")),
  plotOutput("density")
)

server <- function(input, output, session) {
    output$density <- renderPlot({
        req(input$dist)
        dfun <- get(paste0("d", input$dist))
        qfun <- get(paste0("q", input$dist))
        percentiles <- seq_len(100)/100 - 0.005
        vals <- qfun(percentiles)
        probs <- dfun(vals)
        dat <- tibble(vals = vals, probs = probs)
        ggplot(dat, aes(x = vals, y = probs)) + geom_line()
    })  
}

shinyApp(ui, server)