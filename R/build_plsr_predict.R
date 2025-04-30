################################################################################
##### Frame column info
################################################################################

################################################################################
#UI
build_plsr_predict_ui <- function(predict) {
  ns <- NS(predict)



}


################################################################################
#Server
build_plsr_predict_server <- function(predict,
                                      coefficients,
                                      spectra_frame,
                                      trait_frame,
                                      trait_selector,
                                      split_vector,
                                      method) {

  moduleServer(predict,
               function(input, output, session) {


                 frame_results <- reactive({

                   predicted <- plsr_predict(spectra_frame = spectra_frame,
                                             coefficients = coefficients)


                   predicted <- cbind(predicted[,1],
                                      trait_frame[, .SD, .SDcols = trait_selector],
                                      predicted[,-1])

                   colnames(predicted)[2] <- "observed"
                   return(predicted)

                 })

                 return(frame_results)

               }
  )
}
