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
                   label = "RTM model selection:",
                   choices = c("PROSPECT-D" = "prospect_d",
                               "PROSPECT-PRO" = "prospect_pro"))
    ),

    conditionalPanel(
      condition = sprintf("input['%s'] == 'rtm'", ns("selection")),
      radioButtons(ns("rtm_prior"),
                   label = "Prior estimation of N:",
                   choices = c("Yes" = "N_yes",
                               "No" = "N_no"))
    ),

    conditionalPanel(
      condition = sprintf("input['%s'] == 'rtm'", ns("selection")),
      radioButtons(ns("rtm_optimal"),
                   label = "Using optimal spectral domain:",
                   choices = c("Yes" = "opt_yes",
                               "No" = "opt_no"))
    ),

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
          cat(msg, "\n")
          df <- fread(input$coeff$datapath, header = TRUE)
          cat("[INFO] Head of uploaded data:\n")
          print(head(df))
          res <- list(method = "pls", value = df)

        } else if (input$selection == "rtm") {

          res <- list(method = "rtm", value = c(input$rtm_model,
                                                input$rtm_prior,
                                                input$rtm_optimal))

        }

        return(res)

      })

      return(results)
    }
  )
}

