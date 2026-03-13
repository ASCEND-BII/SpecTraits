################################################################################
##### Preprocessing spectra comparison plot
################################################################################

################################################################################
#UI
preprocessing_plot_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    fluidRow(
      column(6,
             h4("Original Spectra"),
             plotOutput(ns("original_plot"))),
      column(6,
             h4("Processed Spectra"),
             plotOutput(ns("processed_plot")))
    )
  )
}

################################################################################
#Server
preprocessing_plot_server <- function(id, original_spectra, processed_spectra) {
  moduleServer(id, function(input, output, session) {

    original_figure <- reactive({
      req(original_spectra())
      spectra_plot_figure(original_spectra(), title = "Original Spectra")
    })

    processed_figure <- reactive({
      req(processed_spectra())
      spectra_plot_figure(processed_spectra(), title = "Processed Spectra")
    })

    output$original_plot <- renderPlot({
      original_figure()
    })

    output$processed_plot <- renderPlot({
      processed_figure()
    })

    return(list(original_figure = original_figure,
                processed_figure = processed_figure))
  })
}

################################################################################
#Function

# Plot all spectra
spectra_plot_figure <- function(frame, title = "Spectra") {

  spectra_frame <- frame

  # Melt to plot each spectrum
  frame_melt <- spectra_frame %>% reshape2::melt(id.vars = names(frame)[1],
                                                 variable.name = "Wavelength",
                                                 value.name = "Value")

  # Transform wavelength to numeric
  frame_melt$Wavelength <- as.numeric(as.character(frame_melt$Wavelength))

  # X and Y limits
  x_limits <- range(frame_melt$Wavelength, na.rm = TRUE)
  y_limits <- range(frame_melt$Value, na.rm = TRUE)
  y_limits[2] <- y_limits[2] * 1.025

  # Plotting element
  plot <- ggplot(data = frame_melt) +
    geom_line(aes(x = Wavelength, y = Value,
                  group = .data[[names(frame)[1]]]),
              colour = "grey",
              linetype = "solid", linewidth = 0.2) +
    ylab("Value") + xlab("Wavelength (nm)") +
    ggtitle(title) +
    scale_x_continuous(limits = x_limits, expand = c(0, 0)) +
    scale_y_continuous(limits = y_limits, expand = c(0, 0)) +
    theme_bw(base_size = 14) +
    theme(plot.margin = margin(t = 20, r = 20, b = 0, l = 0, unit = "pt"),
          plot.title = element_text(hjust = 0.5))

  return(plot)

}
