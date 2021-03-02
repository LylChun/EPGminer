#' Run the EPGminer app (shiny)
#'
#' @export
#'
# https://deanattali.com/2015/04/21/r-package-shiny-app/

epgminer_app <- function () {

  appDir <- system.file("epgminer_app", package = "epgminer")
  shiny::runApp(appDir, display.mode = "normal")
}
