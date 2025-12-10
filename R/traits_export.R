################################################################################
##### Download predicted traits
################################################################################

################################################################################
#UI
traits_export_ui <- function(frame, label = "Download predicted traits") {

  ns <- NS(frame)

  tagList(
    downloadButton(ns("downloadData"), "Download",  class = "btn-primary")
  )
}

################################################################################
#Server
traits_export_server <- function(input, output, session, data) {

  output$downloadData<- downloadHandler(
    filename = function() {
      paste0("predicted-traits_", Sys.Date(), ".csv")
    },
    content = function(file) {
      fwrite(data(), file, row.names = FALSE)
    }
  )
}


