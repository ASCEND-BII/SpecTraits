################################################################################
##### Run models
################################################################################

run_action_io <- function(apply_method) {
  ns <- NS(apply_method)
  tagList(
    actionButton(ns("run"), "Run")
  )
}

run_action_server <- function(apply_method, selection, spectra_frame, values) {
  moduleServer(
    apply_method,
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
