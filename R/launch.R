#' @importFrom shiny runApp
#' @title Launch Shiny App
#' @description Launches shiny app
#' @examples
#' \dontrun{
#' launch_descriptr()
#' }
#' @export
#'
launch_descriptr <- function() {
    shiny::runApp(appDir = system.file("application", package = "descriptr"))
}
