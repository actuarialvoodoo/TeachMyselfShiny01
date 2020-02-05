
output$params <- renderUI({
    req(input$dist)
    use_params <- extract_dist_params(input$dist)
    params_range <- set_param_range(input$dist)
    numInputs <- list()
    for(i in seq_along(use_params)) {
        
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
    use_params <- extract_dist_params(input$dist)
    arg_list <- vector("list", length = length(use_params))
    input_names <- paste0("param_", use_params)
    
    for(i in seq_along(arg_list)) {
        arg_list[[i]] <- input[[input_names[i]]]
    }
    
    names(arg_list) <- use_params
    dat <- make_plot_data(input$dist, arg_list)
    ggplot(dat, aes(x = vals, y = probs)) + geom_line(color = color)
}) 
options(shiny.reactlog = TRUE)