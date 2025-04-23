################################################################################
##### PRESS approach
################################################################################

#-------------------------------------------------------------------------------
# UI
press_input_ui <- function(press) {
  ns <- NS(press)
  tagList(
    radioButtons(ns("press_slection"),
                 label = "Approach for PRESS estimation:",
                 choices = c("LOO" = "loo",
                             "10-fold CV" = "cv",
                             "Permutation" = "permutation"),
                 selected = "cv"),

    conditionalPanel(
      condition = sprintf("input['%s'] == 'loo'", ns("press_slection")),
      numericInput(ns("maxcomp"), "Enter a maximun number of components to start:", value = 30, min = 1, max = 100, step = 1)
    ),

    conditionalPanel(
      condition = sprintf("input['%s'] == 'cv'", ns("press_slection")),
      numericInput(ns("maxcomp"), "Enter a maximun number of components to start:", value = 30, min = 1, max = 100, step = 1)
    ),

    conditionalPanel(
      condition = sprintf("input['%s'] == 'permutation'", ns("press_slection")),
      numericInput(ns("maxcomp"), "Enter a maximun number of components to start:", value = 30, min = 1, max = 100, step = 1),
      numericInput(ns("iterations"), "Enter number of iterations:", value = 100, min = 1, max = 1001, step = 1),
      sliderInput(ns("prop"),
                  "Proportion of data used for each permutation:",
                  min = 0,
                  max = 100,
                  value = 85,
                  step = 1,
                  post = "%")

    ),
  )
}

#-------------------------------------------------------------------------------
# Server
press_input_server <- function(press) {
  moduleServer(
    press,
    function(input, output, session) {

      press_selection <- reactive({

        if(input$press_slection == "loo") {

          press_sel <- list(method = "loo",
                            maxcomp = input$maxcomp,
                            permutation = "none",
                            iterations = "none")

        } else if(input$press_slection == "cv") {

          press_sel <- list(method = "cv",
                            maxcomp = input$maxcomp,
                            permutation = "none",
                            iterations = "none")

        } else if(input$press_slection == "permutation") {

          press_sel <- list(method = "permutation",
                            maxcomp = input$maxcomp,
                            permutation = input$prop/100,
                            iterations = input$iterations)

        }

        return(press_sel)

      })

      return(press_selection)

    })
}
