################################################################################
##### Spectra plot function
################################################################################

################################################################################
#Spectra plot
spectra_plot <- function(input) {

  #Load
  spectra_frame <- spectra_load(input)

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
              sd = sd(Reflectance),
              min = min(Reflectance),
              max = max(Reflectance))

  #Transform to number
  frame_summary$Wavelength <- as.numeric(as.character(frame_summary$Wavelength))

  #X limits
  x_limits <- range(frame_summary$Wavelength)

  #Plotting element
  plot <- ggplot() +
    geom_line(data  = frame_melt,
              aes(x = Wavelength, y = Reflectance, group = ID),
              colour = "grey80") +
    geom_line(data  = frame_summary,
              aes(x = Wavelength, y = min),
              colour = "red", linetype = "dashed") +
    geom_line(data  = frame_summary,
              aes(x = Wavelength, y = max),
              colour = "red", linetype = "dashed") +
    geom_line(data  = frame_summary,
              aes(x = Wavelength, y = mean),
              colour = "#0097a7ff") +
    xlab("Reflectance") + ylab("Wavelength (nm)") +
    scale_x_continuous(limits = x_limits, expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    theme_bw(base_size = 14) +
    theme(plot.margin = margin(t = 20, r = 20, b = 0, l = 0, unit = "pt"))

  return(plot)

}
