################################################################################
##### Predicted histogram plot function
################################################################################

################################################################################
#Server
predicted_plot <- function(frame) {

  predicted_frame <- frame

  #Plotting element
  plot <- ggplot(predicted_frame, aes(x= predicted)) +
    geom_histogram(aes(y= ..density..), fill="#0097a7ff",
                   color = "grey95", position="identity") +
    geom_density(alpha=.2, fill="grey80") +
    ylab("Density") + xlab("Predicted") +
    theme_bw(base_size = 14) +
    theme(plot.margin = margin(t = 20, r = 20, b = 0, l = 0, unit = "pt"))

  return(plot)

}
