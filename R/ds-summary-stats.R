#' @title Descriptive statistics
#'
#' @description Range of descriptive statistics for continuous data.
#'
#' @param data A \code{data.frame} or \code{tibble}.
#' @param ... Column(s) in \code{data}.
#'
#' @examples
#' ds_summary_stats(mtcarz, mpg)
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

  check_df(data)

  var <- rlang::quos(...)

  if (length(var) < 1) {
    is_num <- sapply(data, is.numeric)
    if (!any(is_num == TRUE)) {
      stop("Data has no continuous variables.", call. = FALSE)
    }
    data <- data[, is_num]
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
    ds_rule(paste0('Variable: ', i))
    cat('\n\n')
    print(ds_summary(data, i))
    cat('\n\n\n')
  }

}

ds_summary <- function(data, variable) UseMethod("ds_summary")

ds_summary.default <- function(data, variable) {

  check_df(data)
  vary  <- rlang::enquo(variable)
  var_name <- deparse(substitute(variable))
  check_numeric(data, !! vary, var_name)

  odata <- dplyr::pull(data, !! vary)

  sdata <-
    data %>%
    dplyr::pull(!! vary) %>%
    na.omit()

  low      <- ds_tailobs(sdata, 5, "low")
  high     <- ds_tailobs(sdata, 5, "high")
  low_val  <- ds_rindex(sdata, low)
  high_val <- ds_rindex(sdata, high)

  result <- 
      list(obs      = length(odata),
           missing  = sum(is.na(odata)),
           avg      = mean(sdata),
           tavg     = mean(sdata, trim = 0.05),
           stdev    = sd(sdata),
           variance = var(sdata),
           skew     = ds_skewness(sdata),
           kurtosis = ds_kurtosis(sdata),
           uss      = stat_uss(sdata),
           css      = ds_css(sdata),
           cvar     = ds_cvar(sdata),
           sem      = ds_std_error(sdata),
           median   = median(sdata),
           mode     = ds_mode(sdata),
           range    = ds_range(sdata),
           min      = min(sdata),
           Max      = max(sdata),
           iqrange  = IQR(sdata),
           per99    = quantile(sdata, 0.99),
           per90    = quantile(sdata, 0.90),
           per95    = quantile(sdata, 0.95),
           per75    = quantile(sdata, 0.75),
           per25    = quantile(sdata, 0.25),
           per10    = quantile(sdata, 0.10),
           per5     = quantile(sdata, 0.05),
           per1     = quantile(sdata, 0.01),
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

