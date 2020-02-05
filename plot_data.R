make_plot_data <- function(dist_name, arg_list) {
    dfun <- get(paste0("d", dist_name))
    qfun <- get(paste0("q", dist_name))
    percentiles <- seq_len(100)/100 - 0.005
    vals <- do.call(qfun, args = c(list(p = percentiles), arg_list))
    probs <- do.call(dfun, args = c(list(x = vals), arg_list))
    tibble(vals = vals, probs = probs)
}

set_param_range <- function(dist_name) {
    if (dist_name == "exp") {
        return(list(rate = list(min = 0, max = Inf)))
    }
    
    if (dist_name == "lnorm") {
        return(list(
            meanlog = list(min = -Inf, max = Inf),
            sdlog = list(min = 0, max = Inf)
        ))    
    }
    # norm
    list(mean = list(min = -Inf, max = Inf),
         sd = list(min = 0, max = Inf))
}

extract_dist_params <- function(dist_name) {
    dfun <- get(paste0("d", dist_name))
    params <- names(formals(dfun))
    params[!(params %in% c("x", "log"))]
}