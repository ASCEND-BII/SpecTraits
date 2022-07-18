################################################################################
##### Frame column info
################################################################################

################################################################################
#UI
info_frame_ui <- function(id) {
  ns <- NS(id)
  var_choices <- ""
  tagList(selectInput(ns("observed"), "Select trait:", choices = var_choices, selected = NULL))
}


################################################################################
#Server
info_frame_server <- function(id, dataset) {
  moduleServer(id,
               function(input, output, session) {
                 observeEvent(dataset(), {
                   updateSelectInput(session,
                                     "observed",
                                     choices = colnames(dataset()))
                   })

                 return(
                   list(
                     observed = reactive({input$observed})
                   )
                 )
               }
  )
}
