
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
    numInputs[[length(use_params) + 1]] <- actionButton("update", "Update Plot")
    
    class(numInputs) <- c("shiny.tag.list", "list")
    numInputs
})

output$density <- renderPlot({
    req(input$dist)
    req(input$update)
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