#' \code{descriptr} package
#'
#' Generate descriptive statistics and explore statistical distributions
#'
#' @docType package
#' @name descriptr
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    ".", "breaks", "Levels", "varyable",
    "cumulative", "frequency", "values", "s", "tp", "type", "xvar", "yvar",
    "data", "varnames", "frequency", "varname", "bins", "n", "value", "y", "
    count"
  ))
}
