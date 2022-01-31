################################################################################
##### Frame column info
################################################################################

################################################################################
#UI
info_frame_ui <- function(id, label = "Select trait:") {
  # `NS(frame)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  tagList(
    selectInput(ns("info"), label, choices = NULL))
}


################################################################################
#Server
info_frame_server <- function(id, frame) {

  moduleServer(
    id,

    ## Below is the module function
    function(input, output, session) {

      observeEvent(frame, {
        updateSelectInput(session, "info", choices=colnames(frame))
      })

    }
  )
}
