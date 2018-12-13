#' @title Launch shiny app
#' @description Launches shiny app for interactive model building.
#' @examples
#' \dontrun{
#' blr_launch_app()
#' }
#' @export
#'
blr_launch_app <- function() {
  shiny::runApp(appDir = system.file("application", package = "blorr"))
}
