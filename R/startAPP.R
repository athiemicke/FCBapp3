
#' launches the shinyAppDemo app
#'
#' @export startApp
#'
#' @return shiny application object
#'
#' @example \dontrun {startApp()}
#'
#' @import shiny
#'


# wrapper for shiny::shinyApp()
startApp <- function() {
  shiny::shinyApp(ui = AppUI, server = AppServer)
}
