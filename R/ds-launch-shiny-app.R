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
  shiny::runApp(appDir = system.file("application", package = "descriptr"))
}

#' @export
#' @rdname ds_launch_shiny_app
#' @usage NULL
#'
launch_descriptr <- function(data) {
  .Deprecated("ds_launch_shiny_app()")
  ds_launch_shiny_app()
}
