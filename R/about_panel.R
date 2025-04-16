################################################################################
### About panel

about_panel_ui <- function(id) {
  ns <- NS(id)
  fluidRow(column(width = 6, offset = 3,
                  wellPanel(align = "justify",
                            HTML("<h1 align = 'center'>SpecTraits 0.1 </h1>"),
                            p("2024-04-15", align = "center"),
                            HTML("<p align = 'center'><img src = 'github.png' width = '20px' height = 'auto'> <a target='_blank' rel='noopener noreferrer' href='https://github.com/ASCEND-BII/SpecTraits'> We are on GitHub </a></p>"),
                            HTML("<p><b>Cite the application:</b> https://doi.org/ '>https://doi.org/10.1002/ece3.6928</a></p>")
                  )
                  )
           )
}
