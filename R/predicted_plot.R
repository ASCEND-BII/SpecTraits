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
predicted_plot_server <- function(input, output, session, data) {

  output$predicted <- renderPlot({
    predicted_plot(data())
  })

}

################################################################################
#Function
predicted_plot <- function(frame) {

  predicted_frame <- frame

  #Plotting element
  plot <- ggplot(predicted_frame, aes(x= predicted)) +
    geom_histogram(fill="#0097a7ff",
                   color = "grey95", position="identity") +
    ylab("Frequency") + xlab("Predicted") +
    scale_y_continuous(expand = c(0,0))+
    theme_bw(base_size = 14) +
    theme(plot.margin = margin(t = 20, r = 20, b = 0, l = 0, unit = "pt"))

  return(plot)

}
