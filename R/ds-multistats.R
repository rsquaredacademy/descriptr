#' Multiple variable statistics
#'
#' Descriptive statistics for multiple variables.
#'
#' @param x A \code{tibble} or a \code{data.frame}.
#' @param ... Columns in \code{x}.
#'
#' @return A tibble.
#'
#' @section Deprecated function:
#' \code{multistats()} has been deprecated. Instead use \code{ds_multi_stats()}
#'
#' @examples
#' ds_multi_stats(mtcarz, mpg, disp, hp)
#'
#' @export
#'
ds_multi_stats <- function(x, ...) {

  vars <- rlang::quos(...)

  x %>%
    dplyr::select(!!! vars) %>%
    tidyr::gather(vars, values) %>%
    dplyr::group_by(vars) %>%
    dplyr::summarise_all(dplyr::funs(
      min = min, max = max, mean = mean, t_mean = trimmed_mean,
      median = stats::median, mode = ds_mode, range = ds_range, variance = stats::var,
      stdev = stats::sd, skew = ds_skewness, kurtosis = ds_kurtosis,
      coeff_var = ds_cvar, q1 = quant1, q3 = quant3, iqrange = stats::IQR),
      na.rm = TRUE)
}

#' @export
#' @rdname ds_multi_stats
#' @usage NULL
#'
multistats <- function(x, ...) {
  .Deprecated("ds_multi_stats()")
  ds_multi_stats(x, ...)
}
