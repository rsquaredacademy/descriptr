#' Multiple variable statistics
#'
#' Descriptive statistics for multiple variables.
#'
#' @param data A \code{tibble} or a \code{data.frame}.
#' @param ... Columns in \code{x}.
#'
#' @return A tibble.
#'
#' @examples
#' ds_tidy_stats(mtcarz)
#' ds_tidy_stats(mtcarz, mpg, disp, hp)
#'
#' @section Deprecated Functions:
#' \code{ds_multi_stats()} have been deprecated. Instead use \code{ds_tidy_stats()}.
#'
#' @export
#'
ds_tidy_stats <- function(data, ...) {

  check_df(data)

  vars <- rlang::quos(...)

  if (length(vars) < 1) {
    is_num <- sapply(data, is.numeric)
    if (!any(is_num == TRUE)) {
      rlang::abort("Data has no continuous variables.")
    }
    data <- data[, is_num]
  } else {
    data %<>%
      dplyr::select(!!! vars)
  }

  data %>%
    tidyr::drop_na() %>%
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
#' @rdname ds_tidy_stats
#' @usage NULL
#'
ds_multi_stats <- function(data, ...) {
  .Deprecated("ds_tidy_stats()")
  ds_tidy_stats(data, ...)
}