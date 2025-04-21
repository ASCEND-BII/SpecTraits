################################################################################
##### Split action plot
################################################################################

################################################################################
#UI
split_action_plot_ui <- function(split_action_plot) {
  ns <- NS(split_action_plot)

  fluidPage(
    fluidRow(
      column(6,
             plotOutput(ns("spectra_split_summary"))),
      column(6,
             plotOutput(ns("trait_split_summary")))
    )
  )
}

################################################################################
#Server
split_action_plot_server <- function(split_action_plot,
                                     spectra,
                                     trait,
                                     trait_selector,
                                     split_vector,
                                     group) {

  moduleServer(split_action_plot, function(input, output, session) {

    plot_spectra <- reactive({
      req(spectra(), split_vector())
      spectra_split_summary_plot(spectra(), split_vector())
    })

    plot_trait <- reactive({
      req(trait(), split_vector(), trait_selector(), group)
      trait_split_summary_plot(trait(), split_vector(), trait_selector(), group)
    })

    output$spectra_split_summary <- renderPlot({
      plot_spectra()
    })

    output$trait_split_summary <- renderPlot({
      plot_trait()
    })

  })
}

################################################################################
#Function

# frame <- fread("inst/extdata/spectra.csv")
# frame <- fread("inst/extdata/traits.csv")

#All the spectra
spectra_split_summary_plot <- function(frame, split_vector) {

  spectra_frame <- frame
  spectra_frame[split_vector, Dataset := "training"]
  spectra_frame[!split_vector, Dataset := "testing"]

  #Melt to plot each spectrum
  frame_melt <- spectra_frame %>% reshape2::melt(id.vars = c("ID", "Dataset"),
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
                  group = ID,
                  colour = Dataset),
              linetype = "solid", linewidth = 0.2) +
    ylab("Reflectance") + xlab("Wavelength (nm)") +
    scale_x_continuous(limits = x_limits, expand = c(0, 0)) +
    scale_y_continuous(limits = y_limits, expand = c(0, 0)) +
    scale_colour_manual(" ", values = c("#2fa4e7", "grey")) +
    theme_bw(base_size = 14) +
    theme(plot.margin = margin(t = 20, r = 20, b = 0, l = 0, unit = "pt")) +
    theme(legend.position="none")

  return(plot)

}

trait_split_summary_plot <- function(frame, split_vector, trait_selector, group) {

  # Define frame
  trait_frame <- frame
  trait_frame[split_vector, Dataset := "training"]
  trait_frame[!split_vector, Dataset := "testing"]

  if(group == "none") {

    x <- c("Dataset", as.character(rlang::sym(trait_selector)))
    trait_frame <- trait_frame[, .SD, .SDcols = x]
    colnames(trait_frame)[2] <- "trait"

    #Plotting element
    plot <- ggplot(trait_frame) +
      geom_density(aes(x = trait,
                       fill = Dataset),
                     color = "grey95",
                     position="identity",
                     alpha = 0.5) +
      ylab("Density") + xlab(trait_selector) +
      scale_fill_manual(values = c("grey", "#2fa4e7")) +
      scale_y_continuous(expand = c(0, 0))+
      theme_bw(base_size = 14) +
      theme(plot.margin = margin(t = 20, r = 20, b = 0, l = 0, unit = "pt")) +
      theme(legend.position="none")

  } else {

    x <- c("Dataset", group, as.character(rlang::sym(trait_selector)))
    trait_frame <- trait_frame[, .SD, .SDcols = x]
    colnames(trait_frame)[2] <- "Group"
    colnames(trait_frame)[3] <- "trait"

    plot <- ggplot(trait_frame) +
      geom_boxplot(aes(x = Group,
                       y = trait,
                       fill = Dataset,
                       alpha = 0.5)) +
      ylab(trait_selector) + xlab("Group selected") +
      scale_fill_manual(values = c("grey", "#2fa4e7")) +
      scale_colour_manual(values = c("grey", "#2fa4e7")) +
      theme_bw(base_size = 14) +
      theme(plot.margin = margin(t = 20, r = 20, b = 0, l = 0, unit = "pt")) +
      theme(legend.position="none")

  }

  return(plot)

}
