################################################################################
##### Import spectra file
################################################################################

################################################################################
#UI
spectra_import_ui <- function(spectra, label = "Choose spectra to import") {
  # `NS(frame)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(spectra)

  tagList(
    fileInput(ns("file"), label, accept = c(".csv"))
  )
}

################################################################################
#Server
spectra_import_server <- function(spectra) {
  moduleServer(
    spectra,
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
        fread(userFile()$datapath,
              header = TRUE)
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
