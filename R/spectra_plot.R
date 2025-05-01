################################################################################
##### Spectra plot function
################################################################################

################################################################################
#UI
spectra_plot_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    fluidRow(
      column(6,
             plotOutput(ns("figure_summary"))),
      column(6,
             plotOutput(ns("figure_all")))
    )
  )
}

################################################################################
#Server
spectra_plot_server <- function(input, output, session, data) {

  output$figure_summary <- renderPlot({
    spectra_plot_summary(data())
  })

  output$figure_all <- renderPlot({
    spectra_plot_all(data())
  })

}


################################################################################
#Function
spectra_plot_summary <- function(frame) {

  spectra_frame <- frame

  #Melt to plot each spectrum
  frame_melt <- spectra_frame %>% reshape2::melt(id.vars = "ID",
                                                 variable = "Wavelength",
                                                 value.name = "Reflectance")

  #Transform to number
  frame_melt$Wavelength <- as.numeric(as.character(frame_melt$Wavelength))

  #Get spectra summary
  frame_summary <- frame_melt %>%
    group_by(Wavelength) %>%
    summarize(mean = mean(Reflectance),
              q05 = quantile(Reflectance, 0.05),
              q95 = quantile(Reflectance, 0.95),
              min = min(Reflectance),
              max = max(Reflectance))

  #Transform to number
  frame_summary$Wavelength <- as.numeric(as.character(frame_summary$Wavelength))
  frame_summary$type <- 1

  #X limits
  x_limits <- range(frame_summary$Wavelength)
  y_limits <- c(0, max(frame_summary$max)*1.025)

  #Plotting element
  plot <- ggplot(data = frame_summary) +
    geom_ribbon(aes(x = Wavelength, ymin= q05, ymax= q95, group = type),
                fill = "#005F5F", alpha=0.4) +
    geom_line(data  = frame_summary,
              aes(x = Wavelength, y = min),
              colour = "red", linetype = "dashed") +
    geom_line(data  = frame_summary,
              aes(x = Wavelength, y = max),
              colour = "red", linetype = "dashed") +
    geom_line(data  = frame_summary,
              aes(x = Wavelength, y = mean, ),
              colour = "black", linetype = "solid") +
    ylab("Reflectance") + xlab("Wavelength (nm)") +
    scale_x_continuous(limits = x_limits, expand = c(0, 0)) +
    scale_y_continuous(limits = y_limits, expand = c(0, 0)) +
    theme_bw(base_size = 14) +
    theme(plot.margin = margin(t = 20, r = 20, b = 0, l = 0, unit = "pt"))

  return(plot)

}

#All the spectra
spectra_plot_all <- function(frame) {

  spectra_frame <- frame

  #Melt to plot each spectrum
  frame_melt <- spectra_frame %>% reshape2::melt(id.vars = "ID",
                                                 variable = "Wavelength",
                                                 value.name = "Reflectance")

  #Transform to number
  frame_melt$Wavelength <- as.numeric(as.character(frame_melt$Wavelength))

  #X limits
  x_limits <- range(frame_melt$Wavelength)
  y_limits <- c(0, max(frame_melt$Reflectance)*1.025)

  #Plotting element
  plot <- ggplot(data = frame_melt) +
    geom_line(aes(x = Wavelength, y = Reflectance, group = ID), colour = "grey", linetype = "solid", size = 0.2) +
    ylab("Reflectance") + xlab("Wavelength (nm)") +
    scale_x_continuous(limits = x_limits, expand = c(0, 0)) +
    scale_y_continuous(limits = y_limits, expand = c(0, 0)) +
    theme_bw(base_size = 14) +
    theme(plot.margin = margin(t = 20, r = 20, b = 0, l = 0, unit = "pt"))

  return(plot)

}
