################################################################################
##### Run split action approach
################################################################################

#-------------------------------------------------------------------------------
# UI

run_split_action_ui <- function(run_split) {
  ns <- NS(run_split)
  tagList(
    actionButton(ns("run"),
                 "Run",
                 class = "btn-primary")
  )
}

#-------------------------------------------------------------------------------
# Server

run_split_action_server <- function(run_split, trait_frame, trait_selector, method, ratio, group, seed = 42) {
  moduleServer(
    run_split,
    function(input, output, session) {

      split <- reactiveVal(NULL)

      observeEvent(input$run, {

        showPageSpinner()

        req(trait_frame)

        spl <- split_data(traits_dt   = trait_frame,
                          trait_name  = trait_selector,
                          method      = method,
                          ratio       = ratio,
                          group       = group,
                          seed        = seed)

        split(spl)
        hidePageSpinner()

      })

      return(split)
    })
}

# trait_frame <- fread("inst/extdata/traits.csv")
