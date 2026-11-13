#' @title Descriptive statistics
#'
#' @description Range of descriptive statistics for continuous data.
#'
#' @param data An object of type \code{numeric} or \code{data.frame}.
#' @param ... Column(s) in \code{data}.
#'
#' @examples
#' # numeric data
#' ds_summary_stats(mtcarz$mpg)
#'
#' # single variable
#' ds_summary_stats(mtcarz, mpg)
#'
#' # multiple variables
#' ds_summary_stats(mtcarz, mpg, disp, hp)
#'
#' # all variables
#' ds_summary_stats(mtcarz)
#'
#' @importFrom rlang !!
#' @importFrom stats na.omit
#'
#' @seealso \code{\link[base]{summary}}
#' \code{\link{ds_freq_table}} \code{\link{ds_cross_table}}
#'
#' @export
#'
ds_summary_stats <- function(data, ...) {
  if (is_grouped(data)) data <- dplyr::ungroup(data)

  if (is.numeric(data)) {
    print(ds_summary(data))
  } else {

    check_df(data)
    var <- rlang::quos(...)

    if (length(var) < 1) {
      is_num <- sapply(data, is.numeric)
      if (!any(is_num == TRUE)) {
        stop("Data has no continuous variables.", call. = FALSE)
      }
      data <- data[is_num]
    } else {
      data %<>%
        dplyr::select(!!! var)
      is_num <- sapply(data, is.numeric)
      if (!any(is_num == TRUE)) {
        stop("Data has no continuous variables.", call. = FALSE)
      }
    }

    col_names <- names(data)

    for (i in col_names) {
      check_numeric(data, !! rlang::sym(i), i)
    }

    for (i in col_names) {
      ds_rule(paste0('Variable: ', i))
      cat('\n\n')
      print(ds_summary(data, i))
      cat('\n\n\n')
    }
  }
}

ds_summary <- function(data, variable) UseMethod("ds_summary")

ds_summary.default <- function(data, variable) {
  if (is_grouped(data)) data <- dplyr::ungroup(data)

  if (is.numeric(data)) {
    odata <- data
    sdata <- na.omit(data)
  } else {
    check_df(data)
    vary     <- rlang::enquo(variable)
    var_name <- deparse(substitute(variable))
    check_numeric(data, !! vary, var_name)

    odata <- dplyr::pull(data, !! vary)

    sdata <-
      data %>%
      dplyr::pull(!! vary) %>%
      na.omit()
  }

  pairs <- sort_pairs(odata)
  n     <- length(pairs$value)
  k     <- min(5, n)

  # a variable with no non-missing observations has missing metrics rather
  # than the infinities/NaN produced by min(), max() or mean() of zero values
  metric <- function(value) if (n == 0L) NA_real_ else value

  if (n == 0) {
    low <- high <- numeric(0)
    low_val <- high_val <- integer(0)
  } else {
    low_k    <- seq_len(k)
    high_k   <- rev(seq.int(n - k + 1L, n))
    low      <- round(pairs$value[low_k], 2)
    high     <- round(pairs$value[high_k], 2)
    low_val  <- pairs$index[low_k]
    high_val <- pairs$index[high_k]
  }

  result <-
    list(obs      = length(odata),
         missing  = sum(is.na(odata)),
         avg      = metric(mean(sdata)),
         tavg     = metric(mean(sdata, trim = 0.05)),
         stdev    = metric(sd(sdata)),
         variance = metric(var(sdata)),
         skew     = metric(ds_skewness(sdata)),
         kurtosis = metric(ds_kurtosis(sdata)),
         uss      = metric(stat_uss(sdata)),
         css      = metric(ds_css(sdata)),
         cvar     = metric(ds_cvar(sdata)),
         sem      = metric(ds_std_error(sdata)),
         median   = metric(median(sdata)),
         mode     = metric(ds_mode(sdata)),
         range    = metric(ds_range(sdata)),
         min      = metric(min(sdata)),
         Max      = metric(max(sdata)),
         iqrange  = metric(IQR(sdata)),
         per99    = metric(quantile(sdata, 0.99)),
         per90    = metric(quantile(sdata, 0.90)),
         per95    = metric(quantile(sdata, 0.95)),
         per75    = metric(quantile(sdata, 0.75)),
         per25    = metric(quantile(sdata, 0.25)),
         per10    = metric(quantile(sdata, 0.10)),
         per5     = metric(quantile(sdata, 0.05)),
         per1     = metric(quantile(sdata, 0.01)),
         lowobs   = low,
         highobs  = high,
         lowobsi  = low_val,
         highobsi = high_val)

  class(result) <- "ds_summary"
  return(result)
}

print.ds_summary <- function(x, ...) {
  print_stats(x)
}

