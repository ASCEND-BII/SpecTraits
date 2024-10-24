################################################################################
##### Predicted histogram plot function
################################################################################

################################################################################
#UI

#Selection of predict
predicted_plot_ui <- function(id) {
  ns <- NS(id)

  column(12, plotOutput(ns("predicted")))

}

################################################################################
#Server
predicted_plot_server <- function(input, output, session, data, arguments) {

  plot <- reactive({
    req(data())
    req(arguments)

    figure <- predicted_plot(data(), arguments)

    return(figure)

  })

  output$predicted <- renderPlot({
    plot()
  })

}

################################################################################
#Function
predicted_plot <- function(frame, arguments) {

  #Axis name
  x_name <- paste0("Predicted ", arguments[2], " (", arguments[4], ")")

  predicted_frame <- frame

  #Plotting element
  plot <- ggplot(predicted_frame, aes(x= predicted)) +
    geom_histogram(fill="#0097a7ff",
                   color = "grey95", position="identity") +
    ylab("Frequency") + xlab(x_name) +
    scale_y_continuous(expand = c(0,0))+
    theme_bw(base_size = 14) +
    theme(plot.margin = margin(t = 20, r = 20, b = 0, l = 0, unit = "pt"))

  rm(x_name)

  return(plot)

}
