################################################################################
##### Plot coefficients
################################################################################


#-------------------------------------------------------------------------------
#UI
coefficients_plot_ui <- function(coef_plot) {
  ns <- NS(coef_plot)

  fluidPage(plotOutput(ns("coefficient_figure")),
            plotOutput(ns("vip_figure")))
    # fluidRow(
    #   column(6,
    #          ),
    #   column(6,
    #          )
    # )
  # )
}

#-------------------------------------------------------------------------------
#Server
coefficients_plot_server <- function(coef_plot, results, method) {
  moduleServer(coef_plot, function(input, output, session) {

    output$coefficient_figure <- renderPlot({
      req(results())
      coef_figure(results(), method)
    })

    output$vip_figure <- renderPlot({
      req(results())
      vip_figure(results(), method)
    })
  })
}

#-------------------------------------------------------------------------------
#Function
coef_figure <- function(frame, method) {

  coefficients <- frame$coefficients
  coefficients <- coefficients[,-2]

  #Melt to plot each spectrum
  frame_melt <- data.table::melt(coefficients,
                                 id.vars = "model",
                                 variable.name = "Wavelength",
                                 value.name = "coefficient")
  frame_melt$coefficient <- as.numeric(frame_melt$coefficient)

  if(method == "permutation") {

    frame_melt <- frame_melt[, .(coefficient = mean(coefficient),
                                 lCI = confidence_interval(coefficient)[1],
                                 uCI = confidence_interval(coefficient)[2]),
                             by = "Wavelength"]

    legend <- "Shaded area represents the 95% confidence intervals among permutations"

  } else {

    frame_final <- frame_melt[model == "final", -1]
    frame_melt <- frame_melt[model != "final", .(lCI = confidence_interval(coefficient)[1],
                                                 uCI = confidence_interval(coefficient)[2]),
                             by = "Wavelength"]
    frame_melt <- merge(frame_final, frame_melt, by = "Wavelength")

    legend <- "Shaded area represents the 95% confidence intervals among segments"

  }

  #Transform to number
  frame_melt$Wavelength <- as.numeric(as.character(frame_melt$Wavelength))

  #X limits
  x_limits <- range(frame_melt$Wavelength)

  #Plotting element
  plot <- ggplot(data = frame_melt) +
    geom_hline(yintercept = 0,
               linetype = "dotted",
               color = "grey") +
    geom_ribbon(aes(x = Wavelength,
                    ymin = lCI,
                    ymax = uCI), fill = "#005F5F", alpha = 0.5) +
    geom_line(aes(x = Wavelength,
                  y = coefficient), colour = "black", linewidth = 0.2) +
    ylab("PLSR coefficients") + xlab("Wavelength") +
    scale_x_continuous(limits = x_limits, expand = c(0, 0)) +
    theme_bw(base_size = 14) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    labs(caption = legend)

  return(plot)

}

# VIP
vip_figure <- function(frame, method) {

  vip_frame <- frame$vip

  #Melt to plot each spectrum
  frame_melt <- data.table::melt(vip_frame,
                                 # id.vars = "model",
                                 variable.name = "Wavelength",
                                 value.name = "VIP")
  frame_melt$VIP <- as.numeric(frame_melt$VIP)

  if(method == "permutation") {

    frame_melt <- frame_melt[, .(VIP = mean(VIP),
                                 lCI = confidence_interval(VIP)[1],
                                 uCI = confidence_interval(VIP)[2]),
                             by = "Wavelength"]

    legend <- "Shaded area represents the 95% confidence intervals among permutations"

  } else {

    frame_melt$lCI <- 0
    frame_melt$uCI <- 0

    legend <- " "

  }

  #Transform to number
  frame_melt$Wavelength <- as.numeric(as.character(frame_melt$Wavelength))

  #X limits
  x_limits <- range(frame_melt$Wavelength)

  #Plotting element
  plot <- ggplot(data = frame_melt) +
    geom_hline(yintercept = 0,
               linetype = "dotted",
               color = "grey") +
    geom_ribbon(aes(x = Wavelength,
                    ymin = lCI,
                    ymax = uCI), fill = "#005F5F", alpha = 0.5) +
    geom_line(aes(x = Wavelength,
                  y = VIP), colour = "black", linewidth = 0.2) +
    ylab("Variable of Importance of Projection") + xlab("Wavelength") +
    scale_x_continuous(limits = x_limits, expand = c(0, 0)) +
    theme_bw(base_size = 14) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    labs(caption = legend)

  return(plot)

}
