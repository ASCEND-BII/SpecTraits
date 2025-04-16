################################################################################
##### Download predicted traits
################################################################################

################################################################################
#UI
traits_export_ui <- function(frame, label = "Download predicted traits") {
  # `NS(frame)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(frame)

  tagList(
    downloadButton(ns("downloadData"), "Download")
  )
}

################################################################################
#Server
traits_export_server <- function(input, output, session, data) {

  output$downloadData<- downloadHandler(
    filename = function() {
      paste0("predicted-traits ", Sys.Date()," .csv")
    },
    content = function(file) {
      fwrite(data(), file, row.names = FALSE)
    }
  )
}


