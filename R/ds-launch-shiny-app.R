##' @importFrom shiny runApp
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
  rlang::abort("The shiny app has been moved to a new package, `xplorerr`. To launch the app, run the below code:\n 
	- install.packages('xplorerr')\n - xplorerr::app_descriptive()")
}
 
#' @export
#' @rdname ds_launch_shiny_app
#' @usage NULL
#'
launch_descriptr <- function(data) {
  .Deprecated("ds_launch_shiny_app()")
  ds_launch_shiny_app()
}