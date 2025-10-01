################################################################################
##### Method selection: PLSR or RTM
################################################################################

#-------------------------------------------------------------------------------
# UI

method_input_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("selection"),
                label = "Choose a method:",
                choices = c("PLSR coefficients" = "pls",
                            "Radiative Transfer Models" = "rtm")),

    conditionalPanel(
      condition = sprintf("input['%s'] == 'pls'", ns("selection")),
      fileInput(ns("coeff"),
                label = "Upload PLSR coefficients:",
                accept = c(".csv"))
    ),

    conditionalPanel(
      condition = sprintf("input['%s'] == 'rtm'", ns("selection")),
      radioButtons(ns("rtm_model"),
                   label = "RTM model selection (cctm package; Visser 2021):",
                   choices = c("PROSPECT-D" = "prospect_d",
                               "PROSPECT-5B" = "prospect_5b"))
    )
  )
}

#-------------------------------------------------------------------------------
# Server
method_input_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      results <- reactive({
        if (input$selection == "pls") {

          req(input$coeff)
          msg <- sprintf("[INFO] File %s was uploaded", input$coeff$name)

          df <- fread(input$coeff$datapath, header = TRUE)

          res <- list(method = "pls", value = df)

        } else if(input$selection == "rtm") {

          res <- list(method = "rtm", value = c(input$rtm_model))

        }

        return(res)

      })

      return(results)
    }
  )
}

