################################################################################
##### Run models
################################################################################

run_action_io <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("run"), "Run")
  )
}

run_action_server <- function(id, selection, spectra_frame, values) {
  moduleServer(
    id,
    function(input, output, session) {

      result <- eventReactive(input$run, {

        # Predict traits based on PLSR coefficients
        if(selection == "PLSR coefficients") {

          predicted_frame <- traits_predict(spectra_frame = spectra_frame(),
                                            coefficients = values())

          # Predict traits using RTM
        } else {

          predicted_frame <- traits_predict(spectra_frame = spectra_frame(),
                                            coefficients = values())
        }

        print(predicted_frame)
        return(predicted_frame)

      })
      result
    })
}
