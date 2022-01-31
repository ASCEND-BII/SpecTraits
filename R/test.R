library(shiny)

## Only run examples in interactive R sessions
ui <- fluidPage(
    p("The checkbox group controls the select input"),
    selectInput("inCheckboxGroup", "Input checkbox",
                       c("Item A", "Item B", "Item")),
    selectInput("inSelect", "Select input",
                c("A", "B", "C"))
)

server <- function(input, output, session) {
    observe({
      x <- input$inCheckboxGroup

      # Can use character(0) to remove all choices
      if (is.null(x))
        x <- character(0)

      # Can also set the label and select items
      updateSelectInput(session, "inSelect",
                        label = paste("Select input label", length(x)),
                        choices = x,
                        selected = tail(x, 1)
      )
    })
}

shinyApp(ui, server)


