################################################################################
##### Import traits file
################################################################################

################################################################################
#UI
traits_import_ui <- function(frame, label = "Choose traits to import") {
  # `NS(frame)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(frame)

  tagList(
    fileInput(ns("file"), label))
}

################################################################################
#Server
traits_import_server <- function(frame) {

  moduleServer(
    frame,

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
