################################################################################
##### PRESS plot
################################################################################

################################################################################
#UI
press_action_plot_ui <- function(press_action_plot) {
  ns <- NS(press_action_plot)

  fluidPage(
    fluidRow(
      plotOutput(ns("press_summary")))
  )
}

################################################################################
#Server
press_action_plot_server <- function(press_action_plot,
                                     press_frame) {

  moduleServer(press_action_plot, function(input, output, session) {

    plot_press <- reactive({
      req(press_frame())
      press_figure(frame = press_frame()$press,
                   optimal = press_frame()$optimal)
    })

    output$press_summary <- renderPlot({
      plot_press()
    })
  })
}

################################################################################
#Function

# frame <- fread("inst/extdata/spectra.csv")
# frame <- fread("inst/extdata/traits.csv")

#All the spectra
press_figure <- function(frame, optimal) {

  if(is.vector(frame)) {

    values <- data.table(Component = 1:length(frame),
                         press = frame,
                         sd = 0)

  } else if(is.data.table(frame)) {

    values <- data.table(Component = 1:ncol(frame),
                         press = colMeans(frame),
                         sd = apply(frame, 2, sd))

  }

  #Plotting element
  plot <- ggplot(data = values) +
    geom_line(aes(x = Component, y = press),
              linetype = "solid",
              linewidth = 0.2,
              colour = "grey") +
    geom_errorbar(aes(x = Component,
                      ymin = press - sd,
                      ymax = press + sd),
                  width=0.2,
                  colour = "#2fa4e7") +
    geom_point(aes(x = Component,
                   y = press),
              shape = 21,
              fill = "white",
              colour = "#2fa4e7",
              size = 3) +
    geom_vline(xintercept = optimal,
               linetype = "dotted",
               colour = "red",
               linewidth = 1.0) +
    ylab("PRESS") + xlab("Component") +
    scale_x_continuous(limits = c(0.5, length(frame)+0.5), expand = c(0, 0)) +
    # scale_y_continuous(limits = y_limits, expand = c(0, 0)) +
    # scale_colour_manual(" ", values = c("#2fa4e7", "grey")) +
    theme_bw(base_size = 14) +
    theme(plot.margin = margin(t = 20, r = 20, b = 0, l = 0, unit = "pt")) +
    theme(legend.position="none")

  return(plot)

}
