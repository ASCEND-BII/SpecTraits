################################################################################
##### Run split action approach
################################################################################

#-------------------------------------------------------------------------------
# UI

run_split_action_ui <- function(run_split) {
  ns <- NS(run_split)
  tagList(
    actionButton(ns("run"), "Run")
  )
}

#-------------------------------------------------------------------------------
# Server

run_split_action_server <- function(run_split, trait_frame, trait_selector, method, ratio, group) {
  moduleServer(
    run_split,
    function(input, output, session) {

      split <- eventReactive(input$run, {
        showPageSpinner()
        if(method == "none") {

          req(trait_frame)
          spl <- 1:nrow(trait_frame)
          # print(spl)

        } else if (method == "random") {

          req(trait_frame, ratio)
          spl <- sample(1:nrow(trait_frame), floor(nrow(trait_frame)*ratio))
          # print(spl)

        } else if (method == "stratified") {

          req(trait_frame, ratio)
          spl <- stratified(trait_frame = trait_frame,
                            trait_selector = trait_selector,
                            ratio = ratio)
          # print(spl)

        } else if (method == "group") {

          req(trait_frame, ratio, group)
          plt <- trait_frame[, ..group]
          colnames(plt)[1] <- "group"
          spl <- createDataPartition(plt$group, p = ratio,
                                     list = FALSE,
                                     times = 1)
          # print(spl)

        }

        hidePageSpinner()
        return(spl)

      })

      return(split)
    })
}

stratified <- function(trait_frame, trait_selector, ratio = ratio) {

  # Create breaks
  breaks <- hist(trait_frame[[trait_selector]], plot = FALSE)$breaks

  # Step 2: Assign bins
  dt <- trait_frame[, bin := cut(get(trait_selector),
                                 breaks = breaks,
                                 include.lowest = TRUE,
                                 right = FALSE)]
  dt$row <- 1:nrow(dt)

  # Step 3: Stratified sampling using .SD
  sampled <- dt[, .SD[sample(.N, floor(.N * ratio))], by = bin]

  # Return selection
  return(sampled$row)

}

# trait_frame <- fread("inst/extdata/traits.csv")
