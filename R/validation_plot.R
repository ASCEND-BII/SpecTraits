################################################################################
##### Validation plot function
################################################################################

################################################################################
#UI

validation_plot_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(6,
             plotOutput(ns("scatter"))
      ),
      column(6,
             plotOutput(ns("histogram"))
      )
    ),
    fluidRow(
      column(6,
             plotOutput(ns("residuals"))
      ),
      column(6, align="center",
             DT::dataTableOutput(ns("metrics"))
      )
    )
  )
}

################################################################################
#Server
validation_plot_server <- function(id, observed, predicted, arguments, variable) {

  moduleServer(id,
               function(input, output, session) {

                 #Scatter plot   -----------------------------------------------
                 plot1 <- reactive({
                   req(predicted())
                   req(observed())
                   figure <- scatter_validation_plot(observed(),
                                                     predicted(),
                                                     arguments,
                                                     variable())
                 return(figure)

                 })

                 output$scatter <- renderPlot({
                   plot1()
                 })

                 #Histogram comparing distributions   --------------------------
                 plot2 <- reactive({
                   req(observed())
                   req(predicted())
                   figure <- histogram_validation_plot(observed(),
                                                       predicted(),
                                                       arguments,
                                                       variable())
                   return(figure)

                 })

                 output$histogram <- renderPlot({
                   plot2()
                 })

                 #Scatter plot of residuals   ----------------------------------
                 plot3 <- reactive({
                   req(predicted())
                   req(observed())
                   figure <- residuals_validation_plot(observed(),
                                                       predicted(),
                                                       variable())
                   return(figure)

                 })

                 output$residuals <- renderPlot({
                   plot3()
                 })

                 #Validation metrics    ----------------------------------------
                 metrics_frame <- reactive({
                   req(predicted())
                   req(observed())
                   table <- metrics_validation_frame(observed(),
                                                     predicted(),
                                                     variable())
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

################################################################################
#Functions for plots

#Scatter plot
scatter_validation_plot <- function(observed, predicted, arguments, variable) {

  #Axis name
  x_name <- paste0("Predicted ", arguments[2], " (", arguments[4], ")")

  x <- rlang::sym(variable)
  y_name <- paste0(as.character(x), " (Observed)")

  predicted_frame <- data.frame(observed = observed[ , as.character(x)],
                                predicted = predicted$predicted)

  #Plotting element
  plot <- ggplot(predicted_frame, aes(x = predicted, y = observed)) +
    geom_abline(intercept = 0, slope = 1, colour = "grey50", linetype = "dotted") +
    geom_point(size=2, shape=21, fill="#0097a7ff", color = "grey95") +
    geom_smooth(method = "lm", se = FALSE, colour = "black", linetype = "solid", size = 0.5) +
    ylab(y_name) + xlab(x_name) +
    scale_y_continuous(expand = c(0,0)) +
    theme_bw(base_size = 14) +
    theme(plot.margin = margin(t = 20, r = 20, b = 0, l = 0, unit = "pt"))


  return(plot)

}

#Histograms
histogram_validation_plot <- function(observed, predicted, arguments, variable) {

  #Axis name
  x_name <- paste0(arguments[2])

  y <- rlang::sym(variable)

  p <- ggplot() +
       geom_histogram(data = observed, aes(x = !!y),
                      fill="#0097a7ff", color = "grey95", position="identity", alpha = 0.5) +
       geom_histogram(data = predicted, aes(x = predicted),
                      fill="grey", color = "grey95", position="identity", alpha = 0.5) +
       ylab("Frequency") + xlab(x_name) +
       scale_y_continuous(expand = c(0,0))+
       theme_bw(base_size = 14) +
       theme(plot.margin = margin(t = 20, r = 20, b = 0, l = 0, unit = "pt"))

  return(p)

}

#Residuals plot
residuals_validation_plot <- function(observed, predicted, variable) {

  y_name <- "Observed – Predicted"
  x_name <- "Observed"

  x <- rlang::sym(variable)

  frame <- data.frame(residuals = (predicted$predicted - observed[ , as.character(x)]),
                      observed = observed[ , as.character(x)])

  #Plotting element
  plot <- ggplot(frame, aes(x = observed, y = residuals)) +
    geom_abline(intercept = 0, slope = 0, colour = "grey50", linetype = "dotted") +
    geom_point(size = 2, shape = 21, fill="#0097a7ff", color = "grey95") +
    ylab(y_name) + xlab(x_name) +
    scale_y_continuous(expand = c(0,0)) +
    theme_bw(base_size = 14) +
    theme(plot.margin = margin(t = 20, r = 20, b = 0, l = 0, unit = "pt"))

  return(plot)

}

#Validation metrics
metrics_validation_frame <- function(observed, predicted, variable) {

  x <- rlang::sym(variable)

  frame <- data.frame(observed = observed[ , as.character(x)],
                      predicted = predicted$predicted)

  #Metrics
  metricsoi <- c("R2","MAE", "RMAE", "MBE", "MSE", "RMSE", "RRMSE")

  msummary <- metrics_summary(data = frame,
                              obs = observed,
                              pred = predicted,
                              type = "regression",
                              metrics_list = metricsoi)


  msummary$Score <- round(msummary$Score, 5)

  return(as.data.frame(msummary))

}
