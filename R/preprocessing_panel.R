################################################################################
### Preprocessing panel

preprocessing_panel_ui <- function(id) {
  ns <- NS(id)
  tagList(
    HTML("<h3 style='color:#005F5F; font-weight:bold;'>Apply basic pre-processing methods on leaf spectra</h3>"),
    br(" "),
    # Add build-related inputs/outputs here
  )
}

# Analysis panel module Server logic
preprocessing_panel_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Build logic
  })
}
