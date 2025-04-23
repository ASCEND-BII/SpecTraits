################################################################################
##### Run PREES action approach
################################################################################

#-------------------------------------------------------------------------------
# UI

run_press_action_ui <- function(run_press) {
  ns <- NS(run_press)
  tagList(
    actionButton(ns("run"), "Run")
  )
}

#-------------------------------------------------------------------------------
# Server

run_press_action_server <- function(run_press,
                                    spectra_frame,
                                    trait_frame,
                                    trait_selector,
                                    split_vector,
                                    method,
                                    maxcomp,
                                    prop,
                                    iterations) {
  moduleServer(
    run_press,
    function(input, output, session) {

      press <- eventReactive(input$run, {
        showPageSpinner()

        req(spectra_frame(), trait_frame(), trait_selector, split_vector())

        if(method == "loo") {

          plsr_model <- plsr(trait ~ .,
                             scale=FALSE,
                             center=TRUE,
                             ncomp = maxcomp,
                             validation = "LOO",
                             trace=FALSE,
                             data = frame_to_model)

        } else if (method == "cv") {

          plsr_model <- plsr(trait ~ .,
                             scale=FALSE,
                             center=TRUE,
                             ncomp = maxcomp,
                             validation = "CV",
                             trace=FALSE,
                             data = frame_to_model)

        } else if (method == "permutation") {

          plsr_model <- pls_permutation(trait ~ .,
                                        maxcomp = maxcomp,
                                        iterations = iterations,
                                        prop = prop)

        }

        showPageSpinner()
        return(spl)

      })

      return(press)

    })
}



# trait_frame <- fread("inst/extdata/traits.csv")
