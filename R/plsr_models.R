################################################################################
#### Load coefficients
################################################################################

################################################################################
#UI
plsr_models_IU <- function(id, label = "Model:") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  tagList(
    selectInput(ns("model"),
                label,
                choices = c("LMA (Serbin et al. 2019)" = "Serbin_2019",
                            "Chl (Canvender-Bares et al. ###)" = "to_send.rda")),
    actionButton(ns("predict_action"), label = "Predict trait")
    )
}

################################################################################
#Server
plsr_models_server <- function(id) {
  moduleServer(
    id,

    ## Below is the module function
    function(input, output, session) {

      frame <- observeEvent(input$predict_action, {
        get(load(paste0(here::here(), "/data/", input$model, ".rda")))
      })

      return(frame)
    }
  )
}
