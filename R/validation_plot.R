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
validation_plot_server <- function(id, observed, predicted, variable, method) {

  moduleServer(id,
               function(input, output, session) {

                 #Scatter plot   -----------------------------------------------
                 plot1 <- reactive({
                   req(predicted())
                   req(observed())
                   figure <- scatter_validation_plot(observed(),
                                                     predicted(),
                                                     variable(),
                                                     method)
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
                                                       variable(),
                                                       method)
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
                                                       variable(),
                                                       method)
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
                                                     variable(),
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

################################################################################
#Functions for plots

#Scatter plot
scatter_validation_plot <- function(observed, predicted, variable, method) {

  x <- as.character(rlang::sym(variable))

  if(method == "pls") {

    frame <- cbind(data.table(observed = observed[, ..x]),
                   predicted[, .(predicted = apply(.SD, 1,  mean),
                                 sd = apply(.SD, 1,  sd)),
                             by = ID])

    colnames(frame)[1] <- "observed"


  } else if(method == "rtm") {

    frame <- cbind(data.table(observed = observed[, ..x]),
                   predicted[, ..x])

    colnames(frame)[1] <- "observed"
    colnames(frame)[2] <- "predicted"

  }

  frame$sd <- 0

  #Plotting element
  plot <- ggplot(frame, aes(x = predicted, y = observed)) +
    geom_abline(intercept = 0, slope = 1, colour = "grey50", linetype = "dotted") +
    geom_errorbarh(aes(xmin = predicted - sd,
                       xmax = predicted + sd,
                       y = observed),
                   colour = "grey") +
    geom_point(size=2, shape=21, fill= "#005F5F", color = "grey95") +
    geom_smooth(method = "lm", se = FALSE, colour = "black", linetype = "solid", size = 0.5) +
    ylab("Observed") + xlab("Predicted") +
    scale_y_continuous(expand = c(0,0)) +
    theme_bw(base_size = 14) +
    theme(plot.margin = margin(t = 20, r = 20, b = 0, l = 0, unit = "pt"))

  return(plot)

}

#Histograms
histogram_validation_plot <- function(observed, predicted, variable, method) {

  y <- as.character(rlang::sym(variable))

  if(method == "pls") {

    frame <- cbind(data.table(observed = observed[, ..y]),
                   predicted[, .(predicted = apply(.SD, 1,  mean)),
                             by = ID])

    colnames(frame)[1] <- "observed"


  } else if(method == "rtm") {

    frame <- cbind(data.table(observed = observed[, ..y]),
                   predicted[, ..y])

    colnames(frame)[1] <- "observed"
    colnames(frame)[2] <- "predicted"

  }

  p <- ggplot() +
       geom_histogram(data = frame, aes(x = observed),
                      fill = "grey", color = "grey95", position="identity", alpha = 0.5) +
       geom_histogram(data = frame, aes(x = predicted),
                      fill = "#005F5F", color = "grey95", position="identity", alpha = 0.5) +
       ylab("Frequency") + xlab("Trait distribution") +
       scale_y_continuous(expand = c(0,0))+
       theme_bw(base_size = 14) +
       theme(plot.margin = margin(t = 20, r = 20, b = 0, l = 0, unit = "pt"))

  return(p)

}

#Residuals plot
residuals_validation_plot <- function(observed, predicted, variable, method) {

  y_name <- "Observed – Predicted"
  x_name <- "Observed"

  x <- as.character(rlang::sym(variable))

  if(method == "pls") {

    frame <- cbind(data.table(observed = observed[, ..x]),
                   predicted[, .(predicted = apply(.SD, 1,  mean),
                                 sd = apply(.SD, 1,  sd)),
                             by = ID])

    colnames(frame)[1] <- "observed"


  } else if(method == "rtm") {

    frame <- cbind(data.table(observed = observed[, ..x]),
                   predicted[, ..x])

    colnames(frame)[1] <- "observed"
    colnames(frame)[2] <- "predicted"

  }

  frame$residuals <- frame$observed - frame$predicted

  #Plotting element
  plot <- ggplot(frame, aes(x = observed, y = residuals)) +
    geom_abline(intercept = 0, slope = 0, colour = "grey50", linetype = "dotted") +
    geom_point(size = 2, shape = 21, fill = "#005F5F", color = "grey95") +
    ylab(y_name) + xlab(x_name) +
    scale_y_continuous(expand = c(0,0)) +
    theme_bw(base_size = 14) +
    theme(plot.margin = margin(t = 20, r = 20, b = 0, l = 0, unit = "pt"))

  return(plot)

}

#Validation metrics
metrics_validation_frame <- function(observed, predicted, variable, method) {

  x <- as.character(rlang::sym(variable))

  if(method == "pls") {

    frame <- cbind(data.table(observed = observed[, ..x]),
                   predicted[, .(predicted = apply(.SD, 1,  mean),
                                 sd = apply(.SD, 1,  sd)),
                             by = ID])

    colnames(frame)[1] <- "observed"


  } else if(method == "rtm") {

    frame <- cbind(data.table(observed = observed[, ..x]),
                   predicted[, ..x])

    colnames(frame)[1] <- "observed"
    colnames(frame)[2] <- "predicted"

  }

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
