################################################################################
##### Method selection: PLSR or RTM
################################################################################

#-------------------------------------------------------------------------------
# UI

method_input_ui <- function(method_selection) {
  ns <- NS(method_selection)
  tagList(
    radioButtons(ns("selection"),
                 label = "Select method:",
                 choices = c("PLSR coefficients" = "pls",
                             "Radiative Transfer Models" = "rtm")),

    conditionalPanel(
      condition = sprintf("input['%s'] == 'pls'", ns("selection")),
      fileInput(ns("coeff"), "Upload PLSR coefficients:", accept = c(".csv"))
    ),

    conditionalPanel(
      condition = sprintf("input['%s'] == 'rtm'", ns("selection")),
      radioButtons(ns("rtm_model"),
                   label = "RTM model selection:",
                   choices = c("PROSPECT-D" = "pros_d",
                               "PROSPECT-PRO" = "pros_pro"))
    )
  )
}

#-------------------------------------------------------------------------------
# Server

method_input_server <- function(method_selection) {
  moduleServer(
    method_selection,
    function(input, output, session) {

      results <- reactive({
        if (input$selection == "pls") {

          # The selected file, if any
          coefFile <- reactive({
            # If no file is selected, don't do anything
            validate(need(input$coeff, message = FALSE))
            input$coeff
          })

          # The user's data, parsed into a data frame
          value <- reactive({
            fread(coefFile()$datapath,
                  header = TRUE)
          })

          # We can run observers in here if we want to
          observe({
            msg <- sprintf("File %s was uploaded", coefFile()$name)
            cat(msg, "\n")
          })

        } else if (input$selection == "rtm") {
          value <- input$rtm_model
        }

        list(
          method = input$selection,
          value = value()
        )
      })

      return(results)
    }
  )
}

