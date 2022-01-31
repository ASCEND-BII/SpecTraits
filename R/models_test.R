################################################################################
#### Load coefficients
################################################################################

################################################################################
#UI
models_IU <- function(id, label = "Model:") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  selectInput(ns("model"),
              label,
              choices = c("Radiative Transfer Model" = "RTM",
                          "Partial Least Square Regression" = "PLSR"))

  selectInput("inSelect", "Select input",
              c("Item A", "Item B", "Item C"))

}

################################################################################
#Server

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

#Arguments
models_arguments_server <- function(id) {
  moduleServer(
    id,

    ## Below is the module function
    function(input, output, session) {

      arguments <- reactive({

        #Serbin 2019 model
        if(input$model == "RTM") {
          updateSelectInput(session, "inSelect",
                            label = paste("Select input label", length(x)),
                            choices = x,
                            selected = tail(x, 1)
        } else if(input$model == "PLSR") {
          updateSelectInput(session, "inSelect",
                            label = paste("Select input label", length(x)),
                            choices = x,
                            selected = tail(x, 1)
        return(model)
      })

      return(arguments)

    }
  )
}
