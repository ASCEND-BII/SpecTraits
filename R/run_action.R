################################################################################
##### Run models
################################################################################

#-------------------------------------------------------------------------------
# UI

run_action_io <- function(apply_method) {
  ns <- NS(apply_method)
  tagList(
    actionButton(ns("run"), "Run")
  )
}

#-------------------------------------------------------------------------------
# Server

run_action_server <- function(apply_method, method, spectra_frame, values) {
  moduleServer(
    apply_method,
    function(input, output, session) {

      frame <- eventReactive(input$run, {
        showPageSpinner()
        if(method == "pls") {

          req(spectra_frame, values)
          predicted_frame <- plsr_traits_predict(spectra_frame = spectra_frame,
                                                 coefficients = values)
          # print(head(predicted_frame))

        } else if (method == "rtm") {

          req(spectra_frame, values)
          predicted_frame <- rtm_traits_predict(spectra_frame = spectra_frame,
                                                rtm_model = values)

          # print(head(predicted_frame))
        }

        hidePageSpinner()
        return(predicted_frame)

      })

      return(frame)
    })
}
