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
                choices = c("Choose a model" = "no_apply",
                            "LMA (Serbin et al. 2019)" = "Serbin_2019",
                            "Chl (Canvender-Bares et al. ###)" = "to_send.rda"))
  )
}

################################################################################
#Server
plsr_models_server <- function(id) {
  moduleServer(
    id,

    ## Below is the module function
    function(input, output, session, model) {

      # If there is selection
      userFile <- reactive({
        # If no file is selected, don't do anything
        validate(need(input$model, message = FALSE))
        input$file
      })

      if(userFile == "no_apply") {
        return(null)
      }

      coefficient <- reactive({
        filename <- paste0("data/", userFile, ".rda")
        get(load(filename))
      })

      # Return the reactive that yields the data frame
      return(coefficient)
    }
  )
}
