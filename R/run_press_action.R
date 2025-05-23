################################################################################
##### Run PRESS action approach
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
                             ncomp = maxcomp,
                             validation = "LOO",
                             trace = FALSE,
                             method = "oscorespls",
                             data = frame_to_model)

          opt <- find_optimal_ncomp(model = plsr_model,
                                    traits = frame_to_model$trait,
                                    method = "loo")

        } else if(method == "cv") {

          plsr_model <- plsr(formula = trait ~ .,
                             scale = FALSE,
                             center = TRUE,
                             ncomp = maxcomp,
                             validation = "CV",
                             trace = FALSE,
                             method = "oscorespls",
                             data = frame_to_model)

          opt <- find_optimal_ncomp(model = plsr_model,
                                    traits = frame_to_model$trait,
                                    method = "cv")

        } else if(method == "permutation") {

          press_results <- pls_permutation_press(formula = trait ~ .,
                                                 maxcomp = maxcomp,
                                                 iterations = iterations,
                                                 prop = prop,
                                                 data = frame_to_model)

          opt <- find_optimal_ncomp(model = press_results,
                                    traits = frame_to_model$trait,
                                    method = "permutation")

        }

        hidePageSpinner()
        return(opt)

      })

      return(press)

    })
}

# trait_frame <- fread("inst/extdata/traits.csv")
# spectra_frame <- fread("inst/extdata/spectra.csv")
# trait_selector <- "LMA"
# split_vector <- sample(1:nrow(trait_frame), floor(nrow(trait_frame)*0.6))
# method <- "cv"
# maxcomp <- 30
# prop <- 0.8
# iterations <- 100
