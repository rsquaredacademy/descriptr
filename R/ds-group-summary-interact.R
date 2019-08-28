#' Category wise descriptive statistics
#'
#' Descriptive statistics of a continuous variable for the combination of levels
#' of two or more categorical variables.
#'
#' @param data A \code{data.frame} or a \code{tibble}.
#' @param cvar Column in \code{data}; continuous variable.
#' @param ... Columns in \code{data}; categorical variables.
#'
#' @examples
#' ds_group_summary_interact(mtcarz, mpg, cyl, gear)
#'
#' @seealso \code{\link{ds_group_summary}}
#'
#' @export
#'
ds_group_summary_interact <- function(data, cvar, ...) {

  check_df(data)

  cvar_name <- deparse(substitute(cvar))
  c_var <- rlang::enquo(cvar)
  check_numeric(data, !! c_var, cvar_name)

  g_var <- rlang::quos(...)
  non_type <-
    data %>%
    dplyr::select(!!! g_var) %>%
    purrr::keep(purrr::negate(is.factor)) %>%
    colnames()

  error_message <- paste0("Below grouping variables are not categorical: \n",
                    paste("-", non_type, collapse = "\n"))

  if (length(non_type) > 0) {
  	rlang::abort(error_message)
  }

  cnames <-
    data %>%
    dplyr::select(!!! g_var) %>%
    purrr::keep(is.factor) %>%
    colnames()


  data %>%
    dplyr::select(!!c_var, !!! g_var) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(
      Levels = interaction(!!! g_var)
    ) %>%
    dplyr::select(Levels, !! c_var) %>%
    dplyr::group_by(Levels) %>%
    dplyr::summarise_all(list(
      min = min, max = max, mean = mean, t_mean = trimmed_mean,
      median = stats::median, mode = ds_mode, range = ds_range,
      variance = stats::var, stdev = stats::sd, skew = ds_skewness,
      kurtosis = ds_kurtosis, coeff_var = ds_cvar, q1 = quant1,
      q3 = quant3, iqrange = stats::IQR), na.rm = TRUE) %>%
    tidyr::separate(Levels, into = cnames)

}

