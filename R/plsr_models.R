################################################################################
#### Load coefficients
################################################################################

################################################################################
#UI
plsr_models_IU <- function(id, label = "Model:") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  selectInput(ns("model"),
              label,
              choices = c("LMA (Serbin et al. 2019)" = "Serbin_2019",
                          "Chl (Canvender-Bares et al. ###)" = "to_send.rda"))
}

################################################################################
#Server

#Coefficients
plsr_models_server <- function(id) {
  moduleServer(
    id,

    ## Below is the module function
    function(input, output, session) {

      frame <- reactive({get(load(
        paste0(here::here(), "/data/", input$model, ".rda")))
      })

      return(frame)

    }
  )
}

#Arguments
models_arguments_server <- function(id) {
  moduleServer(
    id,

    ## Below is the module function
    function(input, output, session) {

      arguments <- reactive({

        #Serbin 2019 model
        if(input$model == "Serbin_2019") {
          model <- "Serbin_2019"
        }

        return(model)

      })

      return(arguments)

    }
  )
}



