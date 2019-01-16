#' @title Launch Shiny App
#' @description Launches shiny app
#' @section Deprecated Function:
#' \code{launch_descriptr()} has been deprecated. Instead
#' use \code{ds_launch_shiny_app()}.
#' @examples
#' \dontrun{
#' ds_launch_shiny_app()
#' }
#' @export
#'
ds_launch_shiny_app <- function() {

	rlang::inform("`ds_launch_shiny_app()` has been soft-deprecated and will be removed in the next release. In future, to launch the app, run the below code:\n 
	- install.packages('xplorerr')\n - xplorerr::app_descriptive()\n")

	check_suggests('haven')
	check_suggests('jsonlite')
	check_suggests('readr')
	check_suggests('readxl')
	check_suggests('shinyBS')
	check_suggests('shinycssloaders')
	check_suggests('shinythemes')
	check_suggests('stringr')
	check_suggests('lubridate')

	xplorerr::app_descriptive()

}
 