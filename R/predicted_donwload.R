################################################################################
##### Spectra plot function
################################################################################

################################################################################
#UI
donwload_IU <- function(id, label = "Model:") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  downloadButton(ns("donwload"), label = label, class = NULL, ...)

}

################################################################################
#Server
donwload_server <- function(id) {

  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {



    }
  )
}

