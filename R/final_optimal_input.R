################################################################################
##### Final PLSR input approach
################################################################################

#-------------------------------------------------------------------------------
# UI
final_optimal_input_ui <- function(final) {
  ns <- NS(final)
  tagList(
    radioButtons(ns("final_selection"),
                 label = "ML framework to use:",
                 choices = c("Leave-one-out CV" = "loo",
                             "10-fold CV" = "cv",
                             "Permutation" = "permutation"),
                 selected = "cv"),

    conditionalPanel(
      condition = sprintf("input['%s'] == 'loo'", ns("final_selection")),
      numericInput(ns("ncomp"), "Enter the optimal number of components to use:", value = 30, min = 1, max = 100, step = 1)
    ),

    conditionalPanel(
      condition = sprintf("input['%s'] == 'cv'", ns("final_selection")),
      numericInput(ns("ncomp"), "Enter the optimal number of components to use:", value = 30, min = 1, max = 100, step = 1)
    ),

    conditionalPanel(
      condition = sprintf("input['%s'] == 'permutation'", ns("final_selection")),
      numericInput(ns("ncomp"), "Enter the optimal number of components to use:", value = 30, min = 1, max = 100, step = 1),
      numericInput(ns("iterations"), "Number of permutation:", value = 100, min = 1, max = 1001, step = 1),
      sliderInput(ns("prop"),
                  "Proportion of data for each permutation:",
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
final_optimal_input_server <- function(final) {
  moduleServer(
    final,
    function(input, output, session) {

      final_selection <- reactive({

        if(input$final_selection == "loo") {

          final_sel <- list(method = "loo",
                            ncomp = input$ncomp,
                            permutation = "none",
                            iterations = "none")

        } else if(input$final_selection == "cv") {

          final_sel <- list(method = "cv",
                            ncomp = input$ncomp,
                            permutation = "none",
                            iterations = "none")

        } else if(input$final_selection == "permutation") {

          final_sel <- list(method = "permutation",
                            ncomp = input$ncomp,
                            permutation = input$prop/100,
                            iterations = input$iterations)

        }

        return(final_sel)

      })

      return(final_selection)

    })
}
