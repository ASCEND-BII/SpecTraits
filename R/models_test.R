################################################################################
#### Load coefficients
################################################################################

################################################################################
#Load model-info
model_info <- read.csv(paste0(here::here(), "/data/model-info.csv"),
                       header = TRUE,
                       check.names = FALSE)

################################################################################
#UI
models_IU <- function(id) {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  fluidPage(
    selectInput(ns("model"),
              label = "Select method:",
              choices = unique(model_info$model)),
    selectInput(ns("trait"),
              label = "Select trait:",
              choices = NULL),
    selectInput(ns("condition"),
              label = "Select leaf condition:",
              choices = NULL),
    selectInput(ns("authors"),
              label = "Select model authors:",
              choices = NULL),
    actionButton(ns("set_model"), "Set model"))

}

################################################################################
#Server

models_arguments_server <- function(id) {
  moduleServer(
    id,

    ## Below is the module function
    function(input, output, session) {

      observeEvent(input$model,{
        updateSelectInput(session,
                          inputId = 'trait',
                          choices = unique(model_info$trait[model_info$model == input$model]))
      })

      observeEvent(input$trait,{
        updateSelectInput(session,
                          inputId = 'condition',
                          choices = unique(model_info$condition[model_info$model == input$model & model_info$trait == input$trait]))
      })

      observeEvent(input$condition,{
        updateSelectInput(session,
                          inputId = 'authors',
                          choices = unique(model_info$authors[model_info$model == input$model & model_info$trait == input$trait & model_info$condition == input$condition]))
      })

      arguments <- eventReactive(input$set_model, {

        #Final selection
        model <- model_info[model_info$model == input$model &
                            model_info$trait == input$trait &
                            model_info$condition == input$condition &
                            model_info$authors == input$authors, ]

        #Upload or not the coefficients
        if(model$model == "PLSR") {
          frame <- get(load(paste0(here::here(), "/data/", model$data_name)))
        } else if(model$model == "RTM") {
          frame <- "RTM"
        }

        #Vector of arguments
        arguments_vector <- c(model$model, model$trait, model$condition, model$sensor, model$spectral_resolution, model$units, model$data_name)

        #Return list
        parameters <- list(list(arguments = arguments_vector, coefficients = frame))

        return(parameters)

      })

      return(arguments)

    }
  )
}
