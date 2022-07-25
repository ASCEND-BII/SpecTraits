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

  output$predicted <- renderPlot({
    predicted_plot(data(), arguments)
  })

}

################################################################################
#Function
predicted_plot <- function(frame, arguments) {

  if(arguments == "Serbin et al. (2019)") {
    x_name <- expression(paste("Predicted LMA (g m"^-2, ")"), sep = "")
  }

  predicted_frame <- frame

  #Plotting element
  plot <- ggplot(predicted_frame, aes(x= predicted)) +
    geom_histogram(fill="#0097a7ff",
                   color = "grey95", position="identity") +
    ylab("Frequency") + xlab(x_name) +
    scale_y_continuous(expand = c(0,0))+
    theme_bw(base_size = 14) +
    theme(plot.margin = margin(t = 20, r = 20, b = 0, l = 0, unit = "pt"))

  return(plot)

}
