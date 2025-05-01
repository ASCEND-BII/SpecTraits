################################################################################
##### Plot performance
################################################################################


#-------------------------------------------------------------------------------
#UI
performance_plot_ui <- function(perf_plot) {
  ns <- NS(perf_plot)

  fluidRow(
    plotOutput(ns("scatter_plot")),
    plotOutput(ns("histogram")),
    plotOutput(ns("residuals")),
    DT::dataTableOutput(ns("metrics"))
    )
}

#-------------------------------------------------------------------------------
#Server
performance_plot_server <- function(perf_plot,
                                    result,
                                    trait_selector,
                                    method) {

  moduleServer(perf_plot,
               function(input, output, session) {


                 summary_values <- reactive({

                   values <- result[, -c(1:3)]
                   mean_predicted <- rowMeans(values, na.rm = TRUE)
                   sd_predicted <- apply(values, 1, sd, na.rm = TRUE)

                   values <- cbind(result[, c(1:3)], mean_predicted, sd_predicted)
                   return(values)

                 })

                 #Scatter plot   -----------------------------------------------
                 scatter_figure <- reactive({

                   figure <- scatter_performance_plot(summary_values = summary_values(),
                                                      trait_selector = trait_selector,
                                                      method = method)
                   return(figure)

                 })

                 output$scatter_plot <- renderPlot({
                   scatter_figure()
                 })

                 #Histogram comparing distributions   --------------------------
                 plot2 <- reactive({
                   req(summary_values())
                   req(trait_selector)
                   figure <- histogram_performance_plot(summary_values = summary_values(),
                                                        trait_selector = trait_selector)
                   return(figure)

                 })

                 output$histogram <- renderPlot({
                   plot2()
                 })

                 #Scatter plot of residuals   ----------------------------------
                 plot3 <- reactive({
                   req(summary_values())
                   req(trait_selector)
                   figure <- residuals_performance_plot(summary_values = summary_values(),
                                                        trait_selector = trait_selector)
                   return(figure)

                 })

                 output$residuals <- renderPlot({
                   plot3()
                 })

                 #Validation metrics    ----------------------------------------
                 metrics_frame <- reactive({
                   req(result)
                   req(method)
                   table <- metrics_performance_frame(result,
                                                      method)
                   return(table)

                 })

                 output$metrics <- DT::renderDataTable(DT::datatable(
                   metrics_frame(),
                   options = list(rowCallback = DT::JS(
                     'function(row, data) {
                       // Bold cells for those >= 5 in the first column
                       if (parseFloat(data[1]) >= 5.0)
                       $("td:eq(1)", row).css("font-weight", "bold");
                       }'
                   ))))
               })
}

#-------------------------------------------------------------------------------
#Function
#Scatter plot
scatter_performance_plot <- function(summary_values,
                                     trait_selector,
                                     method) {

  if(method == "permutation") {

    legend <- "The standard deviation of values is calculated from permutations"

  } else {

    legend <- "The standard deviation of values is calculated from cross validation segements"

  }

  #Plotting element
  plot <- ggplot(summary_values, aes(x = mean_predicted, y = observed)) +
    geom_abline(intercept = 0, slope = 1, colour = "grey50", linetype = "dotted") +
    geom_errorbarh(aes(xmin = mean_predicted - sd_predicted,
                       xmax = mean_predicted + sd_predicted,
                       y = observed),
                   colour = "grey") +
    geom_point(size=2, shape=21, fill= "#005F5F", color = "grey95") +
    geom_smooth(method = "lm", se = FALSE, colour = "black", linetype = "solid", linewidth = 0.5) +
    ylab(paste0("Observed ", trait_selector)) +
    xlab(paste0("Predicted ", trait_selector)) +
    # scale_y_continuous(expand = c(0,0)) +
    theme_bw(base_size = 14) +
    theme(plot.margin = margin(t = 20, r = 20, b = 0, l = 0, unit = "pt")) +
    labs(caption = legend)

  return(plot)

}

#Histograms
histogram_performance_plot <- function(summary_values,
                                       trait_selector) {

  p <- ggplot() +
    geom_histogram(data = summary_values, aes(x = observed),
                   fill = "grey", color = "grey95", position="identity", alpha = 0.9) +
    geom_histogram(data = summary_values, aes(x = mean_predicted),
                   fill = "#005F5F", color = "grey95", position="identity", alpha = 0.5) +
    ylab("Frequency") + xlab(trait_selector) +
    scale_y_continuous(expand = c(0,0))+
    theme_bw(base_size = 14) +
    theme(plot.margin = margin(t = 20, r = 20, b = 0, l = 0, unit = "pt"))

  return(p)

}

#Residuals plot
residuals_performance_plot <- function(summary_values,
                                       trait_selector) {

  y_name <- "Observed – Predicted"
  x_name <- trait_selector

  values <- summary_values
  values$residuals <- values$observed - values$mean_predicted

  #Plotting element
  plot <- ggplot(values, aes(x = observed, y = residuals)) +
    geom_errorbar(aes(ymin = residuals - sd_predicted,
                      ymax = residuals + sd_predicted,
                      x = observed),
                   colour = "grey") +
    geom_abline(intercept = 0, slope = 0, colour = "grey50", linetype = "dotted") +
    geom_point(size = 2, shape = 21, fill = "#005F5F", color = "grey95") +
    ylab(y_name) + xlab(x_name) +
    scale_y_continuous(expand = c(0,0)) +
    theme_bw(base_size = 14) +
    theme(plot.margin = margin(t = 20, r = 20, b = 0, l = 0, unit = "pt"))

  return(plot)

}

#Validation metrics
metrics_performance_frame <- function(result, method) {

  if(method == "permutation") {

    perf <- model_performance(observed = result$observed,
                              predicted = result[, -c(1:3)])

    perf_mean <- colMeans(perf[,-1])
    perf_sd <- apply(perf[,-1], 2, sd)
    frame <- data.table(Parameter = names(perf_mean),
                        Mean = round(perf_mean, 5),
                        SD = round(perf_sd, 5))

  } else {

    perf_final <- model_performance(observed = result$observed,
                                    predicted = result[, c(4)])

    perf_cv <- model_performance(observed = result$observed,
                                    predicted = result[, -c(1:4)])
    perf_sd <- apply(perf_cv[,-1], 2, sd)

    frame <- data.table(Parameter = names(perf_sd),
                        final = round(as.numeric(perf_final[1, 2:7]), 5),
                        SD = round(perf_sd, 5))

  }

  return(as.data.frame(frame))

}
