#' @title Launch shiny app
#' @description Launches shiny app for interactive model building.
#' @examples
#' \dontrun{
#' blr_launch_app()
#' }
#' @export
#'
blr_launch_app <- function() {

	check_suggests('xplorerr')
	check_suggests('descriptr')
	check_suggests('jsonlite')
	check_suggests('haven')
	check_suggests('lubridate')
	check_suggests('readr')
	check_suggests('readxl')
	check_suggests('shinyBS')
	check_suggests('shinycssloaders')
	check_suggests('shinythemes')
	check_suggests('stringr')
	check_suggests('tidyr')

	xplorerr::app_logistic_regression()

}

