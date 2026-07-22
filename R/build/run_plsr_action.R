################################################################################
##### Run final PLSR action approach
################################################################################

#-------------------------------------------------------------------------------
# UI

run_plsr_action_ui <- function(run_plsr) {
  ns <- NS(run_plsr)
  tagList(
    actionButton(ns("run_final"),
                 "Run",
                 class = "btn-primary")
  )
}

#-------------------------------------------------------------------------------
# Server

run_plsr_action_server <- function(run_plsr,
                                   spectra_frame,
                                   trait_frame,
                                   trait_selector,
                                   split_vector,
                                   method,
                                   ncomp,
                                   prop,
                                   iterations,
                                   seed = 42) {
  moduleServer(
    run_plsr,
    function(input, output, session) {

      plsr_final <- reactiveVal(NULL)

      observeEvent(input$run_final, {

        showPageSpinner()

        req(spectra_frame, trait_frame, trait_selector, split_vector)

        plsr_results <- build_plsr_model(spectra_dt   = spectra_frame,
                                          traits_dt    = trait_frame,
                                          trait_name   = trait_selector,
                                          split_vector = split_vector,
                                          method       = method,
                                          ncomp        = ncomp,
                                          prop         = prop,
                                          iterations   = iterations,
                                          seed         = seed)

        plsr_final(plsr_results)
        hidePageSpinner()

      })

      return(plsr_final)

    })
}
#
# trait_frame <- fread("inst/extdata/traits.csv")
# spectra_frame <- fread("inst/extdata/spectra_resampled.csv")
# trait_selector <- "LMA"
# split_vector <- sample(1:nrow(trait_frame), floor(nrow(trait_frame)*0.6))
# method <- "cv"
# ncomp <- 30
# prop <- 0.8
# iterations <- 100

# fwrite(plsr_results$coefficients, "inst/extdata/plsr_coefficients.csv")
