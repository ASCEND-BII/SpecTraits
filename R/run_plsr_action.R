################################################################################
##### Run final PLSR action approach
################################################################################

#-------------------------------------------------------------------------------
# UI

run_plsr_action_ui <- function(run_plsr) {
  ns <- NS(run_plsr)
  tagList(
    actionButton(ns("run_final"), "Run")
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
                                   iterations) {
  moduleServer(
    run_plsr,
    function(input, output, session) {

      plsr_final <- eventReactive(input$run_final, {

        showPageSpinner()

        # Required data
        req(spectra_frame, trait_frame, trait_selector, split_vector)

        # Define frames to work
        variables <- c("ID", trait_selector)
        frame_to_model <- merge(trait_frame[, .SD, .SDcols = variables],
                                spectra_frame,
                                by = "ID")
        frame_to_model <- frame_to_model[, -"ID"]
        colnames(frame_to_model)[1] <- "trait"
        frame_to_model <- frame_to_model[split_vector, ]

        if(method == "loo") {

          plsr_model <- plsr(formula = trait ~ .,
                             scale = FALSE,
                             center = TRUE,
                             ncomp = ncomp,
                             validation = "LOO",
                             trace = FALSE,
                             jackknife = TRUE,
                             method = "oscorespls",
                             data = frame_to_model)

        } else if(method == "cv") {

          plsr_model <- plsr(formula = trait ~ .,
                             scale = FALSE,
                             center = TRUE,
                             ncomp = ncomp,
                             validation = "CV",
                             trace = FALSE,
                             jackknife = TRUE,
                             method = "oscorespls",
                             data = frame_to_model)

          plsr_results <- pls_summary(model = plsr_model,
                                      ncomp = ncomp,
                                      data = frame_to_model)

        } else if(method == "permutation") {

          plsr_results <- pls_permutation_coef(formula = trait ~ .,
                                               maxcomp = ncomp,
                                               iterations = iterations,
                                               prop = prop,
                                               data = frame_to_model)

        }

        hidePageSpinner()
        return(plsr_results)

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
