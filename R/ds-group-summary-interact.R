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
  gdata <- dplyr::select(data, !!! g_var)
  nums  <- unlist(lapply(gdata, is.numeric))
  non_type <- colnames(gdata[nums])

  error_message <- paste0("Below grouping variables are not categorical: \n",
                    paste("-", non_type, collapse = "\n"))

  if (length(non_type) > 0) {
        stop(error_message, call. = FALSE)
  }

  cats   <- unlist(lapply(gdata, is.factor))
  cnames <- colnames(gdata[cats])

  data %>%
    dplyr::select(!!c_var, !!! g_var) %>%
    na.omit() %>%
    dplyr::mutate(
      Levels = interaction(!!! g_var)
    ) %>%
    dplyr::select(Levels, !! c_var) %>%
    dplyr::group_by(Levels) %>%
    dplyr::summarise_all(list(
      min = min, max = max, mean = mean, t_mean = trimmed_mean,
      median = median, mode = ds_mode, range = ds_range,
      variance = var, stdev = sd, skew = ds_skewness,
      kurtosis = ds_kurtosis, coeff_var = ds_cvar, q1 = quant1,
      q3 = quant3, iqrange = IQR), na.rm = TRUE) %>%
    tidyr::separate(Levels, into = cnames)

}

