################################################################################
##### Validation plot function
################################################################################

################################################################################
#UI

validation_plot_ui <- function(id) {
  ns <- NS(id)
  fluidRow(column(6, offset = 0, plotOutput(ns("scatter"))),
           column(6, offset = 0, plotOutput(ns("histogram"))))
}

################################################################################
#Server
validation_plot_server <- function(id, observed, predicted, arguments, variable) {

  moduleServer(id,
               function(input, output, session) {

                 plot1 <- reactive({
                   req(predicted())
                   req(observed())
                   figure <- scatter_validation_plot(observed(),
                                                     predicted(),
                                                     arguments,
                                                     variable())
                 return(figure)

                 })

                 plot2 <- reactive({
                   req(observed())
                   figure <- histogram_validation_plot(observed(),
                                                       variable())
                   return(figure)

                 })

                 output$scatter <- renderPlot({
                    plot1()
                 })

                 output$histogram <- renderPlot({
                   plot2()
                 })

               }
  )
}

################################################################################
#Function

#Scatter plot
scatter_validation_plot <- function(observed, predicted, arguments, variable) {

  if(arguments == "Serbin_2019") {
    x_name <- expression(paste("Predicted LMA (g m"^-2, ")"), sep = "")
    y_name <- expression(paste("Observed LMA (g m"^-2, ")"), sep = "")
  }

  x <- rlang::sym(variable)

  print(x)
  print(observed$'!!x')
  print(observed[, !!x])

  predicted_frame <- data.frame(observed = observed[, !!x],
                                predicted = predicted$predicted)

  #Plotting element
  plot <- ggplot(predicted_frame, aes(x = predicted, y = observed)) +
    geom_point(size=2, shape=21, fill="#0097a7ff", color = "grey95") +
    ylab(y_name) + xlab(x_name) +
    scale_y_continuous(expand = c(0,0)) +
    theme_bw(base_size = 14) +
    theme(plot.margin = margin(t = 20, r = 20, b = 0, l = 0, unit = "pt"))

  return(plot)

}

#Histograms
histogram_validation_plot <- function(dataset, xvar) {

  x <- rlang::sym(xvar)

  x_name <- as.character(x)

  p <- ggplot(dataset, aes(x = !!x)) +
       geom_histogram(fill="#0097a7ff",
                   color = "grey95", position="identity") +
       ylab("Frequency") + xlab(x_name) +
       scale_y_continuous(expand = c(0,0))+
       theme_bw(base_size = 14) +
       theme(plot.margin = margin(t = 20, r = 20, b = 0, l = 0, unit = "pt"))

  return(p)

}
