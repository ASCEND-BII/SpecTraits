################################################################################
##### Predicted histogram plot function
################################################################################

#-------------------------------------------------------------------------------
#UI

#Selection of predict
predicted_plot_ui <- function(plot_predicted) {

  ns <- NS(plot_predicted)
  column(12, plotOutput(ns("predicted")))

}

#-------------------------------------------------------------------------------
#Server

predicted_plot_server <- function(plot_predicted, data, method) {
  moduleServer(plot_predicted, function(input, output, session) {

    plot_data <- reactive({
      req(data(), method)
      predicted_plot(data(), method)
    })

    output$predicted <- renderPlot({
      plot_data()
    })

  })
}

#-------------------------------------------------------------------------------
#Function

predicted_plot <- function(frame, method) {

  if(method == "pls") {

    summary_frame <- frame[, .(mean = apply(.SD, 1,  mean)),
                                        by = ID]

    #Plotting element
    plot <- ggplot(summary_frame, aes(x= mean)) +
      geom_histogram(fill= "#005F5F",
                     color = "grey95", position="identity",
                     alpha = 0.75) +
      geom_vline(xintercept = mean(summary_frame$mean),
                 colour = "red", linetype = "dashed", linewidth = 1) +
      ylab("Frequency") + xlab("Predicted") +
      scale_y_continuous(expand = c(0, 0))+
      theme_bw(base_size = 14) +
      theme(plot.margin = margin(t = 20, r = 20, b = 0, l = 0, unit = "pt"))

  } else if(method == "rtm") {

    mean_exclude <-  colMeans(frame[,-1])
    mean_exclude <- c("ID", names(mean_exclude[mean_exclude != 0.00]))
    mean_exclude <- mean_exclude[mean_exclude != "alpha"]
    summary_frame <- frame[, ..mean_exclude]
    summary_frame <- melt(summary_frame, id.vars = "ID", variable.name = "Trait")

    # Ploting elements
    plot <- ggplot(summary_frame, aes(x = value)) +
      geom_histogram(fill= "#005F5F",
                     color = "grey95", position="identity",
                     alpha = 0.75) +
      geom_vline(xintercept = mean(summary_frame$mean),
                 colour = "red", linetype = "dashed", linewidth = 1) +
      ylab("Frequency") + xlab("Predicted") +
      # scale_y_continuous(expand = c(0, 0))+
      theme_bw(base_size = 14) +
      theme(plot.margin = margin(t = 20, r = 20, b = 0, l = 0, unit = "pt")) +
      facet_wrap(~Trait, scales = "free")

  }

  return(plot)

}
