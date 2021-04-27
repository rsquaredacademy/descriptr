#' Tidy descriptive statistics
#'
#' Descriptive statistics for multiple variables.
#'
#' @param data A \code{tibble} or a \code{data.frame}.
#' @param ... Columns in \code{x}.
#'
#' @return A tibble.
#'
#' @examples
#' # all columns
#' ds_tidy_stats(mtcarz)
#'
#' # multiple columns
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
      stop("Data has no continuous variables.", call. = FALSE)
    }
    data <- data[, is_num]
  } else {
    data %<>%
      dplyr::select(!!! vars)
  }

  data %>%
    na.omit() %>%
    tidyr::gather(vars, values) %>%
    dplyr::group_by(vars) %>%
    dplyr::summarise_all(
      list(
        min       = min,
        max       = max,
        mean      = mean,
        t_mean    = trimmed_mean,
        median    = median,
        mode      = ds_mode,
        range     = ds_range,
        variance  = var,
        stdev     = sd,
        skew      = ds_skewness,
        kurtosis  = ds_kurtosis,
        coeff_var = ds_cvar,
        q1        = quant1,
        q3        = quant3,
        iqrange   = IQR
      )
    )
}

#' @export
#' @rdname ds_tidy_stats
#' @usage NULL
#'
ds_multi_stats <- function(data, ...) {
  .Deprecated("ds_tidy_stats()")
  ds_tidy_stats(data, ...)
}
