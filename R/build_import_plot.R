################################################################################
##### Build import files plot
################################################################################

################################################################################
#UI
build_import_plot_ui <- function(build_import_plot) {
  ns <- NS(build_import_plot)

  fluidPage(
    fluidRow(
      column(6,
             plotOutput(ns("spectra_summary"))),
      column(6,
             plotOutput(ns("trait_summary")))
    )
  )
}

################################################################################
#Server
build_import_plot_server <- function(build_import_plot, spectra, trait, variable) {
  moduleServer(build_import_plot, function(input, output, session) {

    output$spectra_summary <- renderPlot({
      req(spectra())
      spectra_summary_plot(spectra())
    })

    output$trait_summary <- renderPlot({
      req(trait(), variable())
      trait_summary_plot(trait(), variable())
    })
  })
}

################################################################################
#Function

# frame <- fread("inst/extdata/spectra.csv")
# frame <- fread("inst/extdata/traits.csv")

#All the spectra
spectra_summary_plot <- function(frame) {

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
    geom_line(aes(x = Wavelength, y = Reflectance,
                  group = ID),
              colour = "grey",
              linetype = "solid", linewidth = 0.2) +
    ylab("Reflectance") + xlab("Wavelength (nm)") +
    scale_x_continuous(limits = x_limits, expand = c(0, 0)) +
    scale_y_continuous(limits = y_limits, expand = c(0, 0)) +
    theme_bw(base_size = 14) +
    theme(plot.margin = margin(t = 20, r = 20, b = 0, l = 0, unit = "pt"))

  return(plot)

}

trait_summary_plot <- function(frame, variable) {

  x <- as.character(rlang::sym(variable))
  trait <- as.vector(as.matrix(frame[, .SD, .SDcols = x]))

  #Plotting element
  plot <- ggplot() +
    geom_histogram(aes(x= trait),
                   fill="#2fa4e7",
                   color = "grey95", position="identity",
                   alpha = 0.75) +
    geom_vline(xintercept = mean(trait),
               colour = "red", linetype = "dashed", linewidth = 1) +
    ylab("Frequency") + xlab(x) +
    scale_y_continuous(expand = c(0, 0))+
    theme_bw(base_size = 14) +
    theme(plot.margin = margin(t = 20, r = 20, b = 0, l = 0, unit = "pt"))

  return(plot)

}
