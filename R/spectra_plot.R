################################################################################
##### Spectra plot function
################################################################################

spectra_plot <- function(load_spectra) {
  
  frame <- load_spectra
  
  #Melt to plot each spectrum
  frame_melt <- melt(frame, 
                     id.vars = c("ID"),
                     measure.vars = c(.SD),
                     variable.name = "Wavelength",
                     value.name = "Reflectance")
  
  #Transform to number
  frame_melt$Wavelength <- as.numeric(as.character(frame_melt$Wavelength))
  
  #Get spectra summary
  frame_summary <- frame_melt[, list(mean = mean(Reflectance),
                                     sd = sd(Reflectance),
                                     min = min(Reflectance),
                                     max = max(Reflectance)), 
                                     by = .(Wavelength)]
  
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
    scale_x_continuous(limits = x_limits, expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    theme_classic()
  
  export(plot)
  
}

