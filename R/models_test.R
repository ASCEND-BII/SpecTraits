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
              label = "Select model:",
              choices = unique(model_info$model)),
    selectInput(ns("trait"),
              label = "Select trait of interest:",
              choices = NULL),
    selectInput(ns("condition"),
              label = "Select leaf condition:",
              choices = NULL),
    selectInput(ns("authors"),
              label = "Select model authors:",
              choices = NULL))

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

      arguments <- reactive({

        model <- unique(model_info$data_name[model_info$model == input$model &
                                             model_info$trait == input$trait &
                                             model_info$condition == input$condition &
                                             model_info$authors == input$authors])

        print(model)

        return(model)

      })

      return(arguments)

      print(arguments)

    }
  )
}


#Coefficients
models_server <- function(id) {
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
