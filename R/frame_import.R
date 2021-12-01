################################################################################
##### Import file
################################################################################

#Module UI function
import_UI <- function(id, label = "Choose spectra to import") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  tagList(
    fileInput(ns("file"), label)
  )
}


#Load spectra data
import_server <- function(id, stringsAsFactors) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      # The selected file, if any
      userFile <- reactive({
        # If no file is selected, don't do anything
        validate(need(input$file, message = FALSE))
        input$file
      })

      # The user's data, parsed into a data frame
      dataframe <- reactive({
        read.csv(userFile()$datapath,
                 header = TRUE,
                 check.names = FALSE,
                 stringsAsFactors = stringsAsFactors)
      })

      # We can run observers in here if we want to
      observe({
        msg <- sprintf("File %s was uploaded", userFile()$name)
        cat(msg, "\n")
      })

      # Return the reactive that yields the data frame
      return(dataframe)
    }
  )
}
