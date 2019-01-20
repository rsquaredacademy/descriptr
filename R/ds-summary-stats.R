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
#'
#' @seealso \code{\link[base]{summary}} \code{\link{ds_freq_cont}}
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
      rlang::abort("Data has no continuous variables.")
    }
    data <- data[, is_num]
  } else {
    data %<>%
      dplyr::select(!!! var)
    is_num <- sapply(data, is.numeric)
    if (!any(is_num == TRUE)) {
      rlang::abort("Data has no continuous variables.")
    }  
  }
  
  col_names <- names(data)
  for (i in col_names) {
    cat(cli::rule(center = paste0('Variable: ', i), 
                  width = options()$width))
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
    stats::na.omit()

  low      <- ds_tailobs(sdata, 5, "low")
  high     <- ds_tailobs(sdata, 5, "high")
  low_val  <- ds_rindex(sdata, low)
  high_val <- ds_rindex(sdata, high)

  result <- list(obs      = length(odata),
    			 missing  = sum(is.na(odata)),
			     avg      = mean(sdata),
			     tavg     = mean(sdata, trim = 0.05),
			     stdev    = stats::sd(sdata),
			     variance = stats::var(sdata),
			     skew     = ds_skewness(sdata),
			     kurtosis = ds_kurtosis(sdata),
			     uss      = stat_uss(sdata),
			     css      = ds_css(sdata),
			     cvar     = ds_cvar(sdata),
			     sem      = ds_std_error(sdata),
			     median   = stats::median(sdata),
			     mode     = ds_mode(sdata),
			     range    = ds_range(sdata),
			     min      = min(sdata), 
			     Max      = max(sdata),
			     iqrange  = stats::IQR(sdata),
			     per99    = stats::quantile(sdata, 0.99),
			     per90    = stats::quantile(sdata, 0.90),
			     per95    = stats::quantile(sdata, 0.95),
			     per75    = stats::quantile(sdata, 0.75),
			     per25    = stats::quantile(sdata, 0.25),
			     per10    = stats::quantile(sdata, 0.10),
			     per5     = stats::quantile(sdata, 0.05),
			     per1     = stats::quantile(sdata, 0.01),
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

