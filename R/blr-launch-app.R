#' @importFrom shiny runApp
#' @title Launch Shiny App
#' @description Launches shiny app
#' @examples
#' \dontrun{
#' blr_launch_app()
#' }
#' @export
#'
blr_launch_app <- function() {
  runApp(appDir = system.file("application", package = "blorr"))
}
