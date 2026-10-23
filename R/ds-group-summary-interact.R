#' Category wise descriptive statistics
#'
#' Descriptive statistics of a continuous variable for the combination of levels
#' of two or more categorical variables.
#'
#' @param data A \code{data.frame} or a \code{tibble}.
#' @param col Column in \code{data}; continuous variable.
#' @param ... Columns in \code{data}; categorical variables.
#'
#' @details Character grouping columns are accepted and treated as factors.
#' Factor levels may contain any character, including \code{.}.
#'
#' @examples
#' ds_group_summary_interact(mtcarz, mpg, cyl, gear)
#'
#' @seealso \code{\link{ds_group_summary}}
#'
#' @export
#'
ds_group_summary_interact <- function(data, col, ...) {

  check_df(data)

  cvar_name <- deparse(substitute(col))
  c_var <- rlang::enquo(col)
  check_numeric(data, !! c_var, cvar_name)

  g_var <- rlang::quos(...)
  gdata <- dplyr::select(data, !!! g_var)

  # grouping columns must be categorical; character columns are accepted and
  # treated as factors rather than being silently dropped
  is_cat   <- vapply(gdata, function(x) is.factor(x) || is.character(x), logical(1))
  non_type <- names(gdata)[!is_cat]

  error_message <- paste0("Below grouping variables are not categorical: \n",
                    paste("-", non_type, collapse = "\n"))

  if (length(non_type) > 0) {
        stop(error_message, call. = FALSE)
  }

  cnames <- names(gdata)

  data %>%
    dplyr::select(!!c_var, !!! g_var) %>%
    na.omit() %>%
    dplyr::mutate(
      Levels = interaction(!!! g_var, sep = "\r", drop = TRUE)
    ) %>%
    dplyr::select(Levels, !! c_var) %>%
    dplyr::group_by(Levels) %>%
    dplyr::summarise_all(list(
      min = min, max = max, mean = mean, t_mean = trimmed_mean,
      median = median, mode = ds_mode, range = ds_range,
      variance = var, stdev = sd, skew = ds_skewness,
      kurtosis = ds_kurtosis, coeff_var = ds_cvar, q1 = quant1,
      q3 = quant3, iqrange = IQR)) %>%
    tidyr::separate(Levels, into = cnames, sep = "\r")

}

