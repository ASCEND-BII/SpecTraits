spectra_frame <- read.csv("inst/extdata/spectra.csv", check.names = FALSE)
wavelength <- as.numeric(colnames(spectra_frame)[-1])


frame_melt <- spectra_frame %>% melt(id.vars = "ID", 
                                     variable = "Wavelength",
                                     value.name = "Reflectance")
