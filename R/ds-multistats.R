#' @importFrom rlang quos
#' @importFrom tidyr gather
#' @importFrom dplyr group_by summarise_all funs
#' @title Multiple Variable Statistics
#' @description Summary/descriptive statistics for multiple variables
#' @param x a tibble/data.frame
#' @param ... columns in \code{x}
#' @return a tibble
#' @section Deprecated Function:
#' \code{multistats()} has been deprecated. Instead use \code{ds_multi_stats()}
#' @examples
#' ds_multi_stats(mtcarz, mpg, disp, hp)
#' @export
#'
ds_multi_stats <- function(x, ...) {

  vars <- quos(...)

  x %>%
    select(!!! vars) %>%
    gather(vars, values) %>%
    group_by(vars) %>%
    summarise_all(funs(
      min = min, max = max, mean = mean, t_mean = trimmed_mean,
      median = median, mode = ds_mode, range = ds_range, variance = var,
      stdev = sd, skew = ds_skewness, kurtosis = ds_kurtosis,
      coeff_var = ds_cvar, q1 = quant1, q3 = quant3, iqrange = IQR),
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
