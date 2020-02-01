
output$params <- renderUI({
    req(input$dist)
    dfun <- get(paste0("d", input$dist))
    params <- names(formals(dfun))
    use_params <- params[!(params %in% c("x", "log"))]
    params_range <- set_param_range(input$dist)
    numInputs <- list()
    for(i in seq_along(use_params)) {11
        
        numInputs[[i]] <- numericInput(
            paste0("param_", use_params[i]), 
            label = paste("Enter ", use_params[i], ": "), 
            value = 1, 
            min = params_range[[use_params[i]]]$min, 
            max = params_range[[use_params[i]]]$max 
        )
    }
    numInputs[[length(use_params) + 1]] <- actionButton("update", "Update Plot")
    
    class(numInputs) <- c("shiny.tag.list", "list")
    numInputs
})

output$density <- renderPlot({
    req(input$dist)
    req(input$update)
    color <- input$color
    params_idx <- str_which(names(input), "^param_")
    arg_list <- vector("list", length = length(params_idx))
    input_names <- names(input)[params_idx]
    for(i in seq_along(arg_list)) {
        arg_list[[i]] <- isolate(input[[input_names[i]]])
    }
    names(arg_list) <- str_extract(input_names, "(?<=_)[A-Za-z]+$")
    dat <- make_plot_data(input$dist, arg_list)
    ggplot(dat, aes(x = vals, y = probs)) + geom_line(color = color)
}) 
options(shiny.reactlog = TRUE)