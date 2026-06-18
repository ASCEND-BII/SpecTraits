################################################################################
##### Run PRESS action approach
################################################################################

#-------------------------------------------------------------------------------
# UI

run_press_action_ui <- function(run_press) {
  ns <- NS(run_press)
  tagList(
    actionButton(ns("run"),
                 "Run",
                 class = "btn-primary")
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
                                    iterations,
                                    seed = 42) {
  moduleServer(
    run_press,
    function(input, output, session) {

      press <- reactiveVal(NULL)

      observeEvent(input$run, {

        showPageSpinner()

        req(spectra_frame, trait_frame, trait_selector, split_vector)

        opt <- press_plsr_eval(spectra_dt   = spectra_frame,
                               traits_dt    = trait_frame,
                               trait_name   = trait_selector,
                               split_vector = split_vector,
                               method       = method,
                               maxcomp      = maxcomp,
                               prop         = prop,
                               iterations   = iterations,
                               seed         = seed)

        press(opt)
        hidePageSpinner()

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
