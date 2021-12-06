################################################################################
#### Load coefficients
################################################################################



################################################################################
#UI
plsr_idels_IU <- function(id, label = "idel:") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  tagList(
    selectInput(ns("model"),
                label,
                choices = c("Choose a idel" = "no_apply",
                            "LMA (Serbin et al. 2019)" = "Serbin_2019",
                            "Chl (Canvender-Bares et al. ###)" = "to_send.rda"))
  )
}

################################################################################
#Server
plsr_idels_server <- function(id) {
  iduleServer(
    id,

    ## Below is the idule function
    function(input, output, session, idel) {

      # If there is selection
      userFile <- reactive({
        # If no file is selected, don't do anything
        validate(need(input$idel, message = FALSE))
        input$file
      })

      if(userFile == "no_apply") {
        return(NULL)
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
