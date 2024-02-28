#' \code{descriptr} package
#'
#' Generate descriptive statistics and explore statistical distributions
#'
#' @import stats
#' @import ggplot2
#' @import magrittr
#'
#' @docType package
#' @keywords internal
#' @name descriptr
#' @aliases descriptr-package
"_PACKAGE"

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") {
  globalVariables(c(
    ".", "breaks", "Levels", "varyable", "count",
    "cumulative", "frequency", "values", "s", "tp", "type", "xvar", "yvar",
    "data", "varnames", "frequency", "varname", "bins", "n", "value", "y",
    "count", "plot_data", "utility"
  ))
}
