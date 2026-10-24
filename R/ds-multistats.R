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
    data <- data[is_num]
  } else {
    data %<>%
      dplyr::select(!!! vars)
  }

  data %>%
    tidyr::gather(vars, values) %>%
    dplyr::group_by(vars) %>%
    dplyr::summarise_all(
      list(
        min       = ~ num_or_na(., min),
        max       = ~ num_or_na(., max),
        mean      = ~ num_or_na(., mean),
        t_mean    = ~ trimmed_mean(., na.rm = TRUE),
        median    = ~ median(., na.rm = TRUE),
        mode      = ds_mode,
        range     = ds_range,
        variance  = ~ var(., na.rm = TRUE),
        stdev     = ~ sd(., na.rm = TRUE),
        skew      = ds_skewness,
        kurtosis  = ds_kurtosis,
        coeff_var = ds_cvar,
        q1        = ~ quant1(., na.rm = TRUE),
        q3        = ~ quant3(., na.rm = TRUE),
        iqrange   = ~ IQR(., na.rm = TRUE)
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
