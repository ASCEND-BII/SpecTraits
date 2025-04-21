################################################################################
##### Split data approach
################################################################################

#-------------------------------------------------------------------------------
# UI

split_input_ui <- function(split) {
  ns <- NS(split)
  tagList(
    selectInput(ns("split_slection"),
                label = "Choose a data split approach:",
                choices = c("None" = "none",
                            "Random" = "random",
                            "Stratified sampling" = "stratified",
                            "Group" = "group")),

    conditionalPanel(
      condition = sprintf("input['%s'] == 'random'", ns("split_slection")),
      sliderInput(ns("ratio"),
                  "Data for training:",
                  min = 0,
                  max = 100,
                  value = 70,
                  step = 1,
                  post = "%")
    ),

    conditionalPanel(
      condition = sprintf("input['%s'] == 'stratified'", ns("split_slection")),
      sliderInput(ns("ratio"),
                  "Data for training:",
                  min = 0,
                  max = 100,
                  value = 70,
                  step = 1,
                  post = "%")
    ),

    conditionalPanel(
      condition = sprintf("input['%s'] == 'group'", ns("split_slection")),
      sliderInput(ns("ratio"),
                  "Data for training:",
                  min = 0,
                  max = 100,
                  value = 70,
                  step = 1,
                  post = "%"),
      uiOutput(ns("gruop_selector"))
    ),
  )
}

#-------------------------------------------------------------------------------
# Server

split_input_server <- function(split, frame) {
  moduleServer(
    split,
    function(input, output, session) {

      output$gruop_selector <- renderUI({
        req(frame())
        categorical_cols <- names(frame())[sapply(frame(), is.character)]
        selectInput(session$ns("columns"), "Select gruop",
                    choices = categorical_cols,
                    selected = NULL,
                    multiple = FALSE)
      })

      results <- reactive({

        if(input$split_slection == "none") {

          res <- list(split = "none", ratio = NULL, gruop = NULL)

        } else if(input$split_slection == "random") {

          res <- list(split = "random", ratio = input$ratio, gruop = NULL)

        } else if(input$split_slection == "stratified") {

          res <- list(split = "stratified", ratio = input$ratio, gruop = NULL)

        } else if(input$split_slection == "group") {

          res <- list(split = "group", ratio = input$ratio, gruop = input$columns)

        }

        return(res)

      })

      return(results)

    })
}

