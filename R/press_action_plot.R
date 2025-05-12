################################################################################
##### PRESS plot
################################################################################

# ------------------------------------------------------------------------------
# UI
press_action_plot_ui <- function(press_action_plot) {
  ns <- NS(press_action_plot)

  fluidPage(
    fluidRow(
      plotOutput(ns("press_summary")))
  )
}

# ------------------------------------------------------------------------------
# Server
press_action_plot_server <- function(press_action_plot,
                                     press_frame) {

  moduleServer(press_action_plot, function(input, output, session) {

    plot_press <- reactive({
      req(press_frame())
      press_figure(frame = press_frame()$rmsep,
                   optimal = press_frame()$optimal,
                   legend = press_frame()$legend)
    })

    output$press_summary <- renderPlot({
      plot_press()
    })

    return(plot_press)

  })
}

# ------------------------------------------------------------------------------
# Function

# All the spectra
press_figure <- function(frame, optimal, legend) {

  #Plotting element
  plot <- ggplot(data = frame) +
    geom_line(aes(x = ncomp, y = rmsep_mean),
              linetype = "solid",
              linewidth = 0.2,
              colour = "grey") +
    geom_errorbar(aes(x = ncomp,
                      ymin = rmsep_mean - rmsep_sd,
                      ymax = rmsep_mean + rmsep_sd),
                  width=0.2,
                  colour = "#005F5F") +
    geom_point(aes(x = ncomp,
                   y = rmsep_mean),
              shape = 21,
              fill = "white",
              colour = "#005F5F",
              size = 2) +
    geom_vline(xintercept = optimal,
               linetype = "dotted",
               colour = "red",
               linewidth = 1.0) +
    geom_vline(xintercept = which.min(frame$rmsep_mean),
               linetype = "dotted",
               colour = "blue",
               linewidth = 1.0) +
    ylab("RMSEP") + xlab("PLRS components") +
    scale_x_continuous(limits = c(0.5, nrow(frame)+0.5), expand = c(0, 0)) +
    theme_bw(base_size = 14) +
    theme(plot.margin = margin(t = 20, r = 20, b = 0, l = 0, unit = "pt")) +
    theme(legend.position="none") +
    labs(subtitle = paste0("Optimal number of components using one standar deviation method = ", (optimal)),
         caption = legend)

  return(plot)

}
