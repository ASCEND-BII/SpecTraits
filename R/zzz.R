#' @importFrom shiny addResourcePath
.onLoad <- function(...) {
  # Create link to javascript files.
  shiny::addResourcePath("SpecTraits", system.file("www", package="SpecTraits"))
}
