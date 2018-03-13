#' @title Descriptive statistics
#'
#' @description Range of descriptive statistics for continuous data.
#'
#' @param data A \code{data.frame} or \code{tibble}.
#' @param variable Column in \code{data}.
#'
#' @return \code{ds_summary_stats()} returns an object of class
#' \code{"ds_summary_stats"}. An object of class \code{"ds_summary_stats"}
#' is a list containing the following components
#'
#' \item{obs}{Number of observations.}
#' \item{missing}{Number of missing observations.}
#' \item{avg}{Mean.}
#' \item{tavg}{5 percent trimmed mean.}
#' \item{stdev}{Standard deviation.}
#' \item{variance}{Variance.}
#' \item{skew}{Skewness.}
#' \item{kurtosis}{Kurtosis.}
#' \item{uss}{Uncorrected sum of squares.}
#' \item{css}{Corrected sum of squares.}
#' \item{cvar}{Coefficient of variation.}
#' \item{sem}{Standard error of mean.}
#' \item{median}{Median.}
#' \item{mode}{Mode.}
#' \item{range}{Range.}
#' \item{min}{Minimum value.}
#' \item{iqrange}{Inter quartile range.}
#' \item{per99}{99th percentile.}
#' \item{per95}{95th percentile.}
#' \item{per90}{90th percentile.}
#' \item{per75}{75th percentile.}
#' \item{per25}{25th percentile.}
#' \item{per10}{10th percentile.}
#' \item{per5}{5th percentile.}
#' \item{per1}{1st percentile.}
#' \item{lowobs}{Five lowest observations.}
#' \item{highobs}{Five highest observations.}
#' \item{lowobsi}{Index of five lowest observations.}
#' \item{highobsi}{Index of five highest observations.}
#'
#' @section Deprecated function:
#' \code{summary_stats()} has been deprecated. Instead use
#' \code{ds_summary_stats()}.
#'
#' @examples
#' ds_summary_stats(mtcarz, mpg)
#'
#' @importFrom stats quantile
#' @importFrom rlang enquo !!
#'
#' @seealso \code{\link[base]{summary}} \code{\link{ds_freq_cont}}
#' \code{\link{ds_freq_table}} \code{\link{ds_cross_table}}
#'
#' @export
#'
ds_summary_stats <- function(data, variable) UseMethod("ds_summary_stats")

#' @export
#'
ds_summary_stats.default <- function(data, variable) {

  vary <- enquo(variable)

  odata <-
    data %>%
    pull(!! vary)

  sdata <-
    data %>%
    pull(!! vary) %>%
    na.omit()

  if (!is.numeric(sdata)) {
    stop("data must be numeric")
  }

  low <- ds_tailobs(sdata, 5, "low")
  high <- ds_tailobs(sdata, 5, "high")
  low_val <- ds_rindex(sdata, low)
  high_val <- ds_rindex(sdata, high)

  result <- list(
    obs = length(odata),
    missing = sum(is.na(odata)),
    avg = mean(sdata),
    tavg = mean(sdata, trim = 0.05),
    stdev = sd(sdata),
    variance = var(sdata),
    skew = ds_skewness(sdata),
    kurtosis = ds_kurtosis(sdata),
    uss = stat_uss(sdata),
    css = ds_css(sdata),
    cvar = ds_cvar(sdata),
    sem = std_error(sdata),
    median = median(sdata),
    mode = ds_mode(sdata),
    range = ds_range(sdata),
    min = min(sdata), Max = max(sdata),
    iqrange = IQR(sdata),
    per99 = quantile(sdata, 0.99),
    per90 = quantile(sdata, 0.90),
    per95 = quantile(sdata, 0.95),
    per75 = quantile(sdata, 0.75),
    per25 = quantile(sdata, 0.25),
    per10 = quantile(sdata, 0.10),
    per5 = quantile(sdata, 0.05),
    per1 = quantile(sdata, 0.01),
    lowobs = low,
    highobs = high,
    lowobsi = low_val,
    highobsi = high_val
  )

  class(result) <- "ds_summary_stats"
  return(result)
}

#' @export
#' @rdname ds_summary_stats
#' @usage NULL
#'
summary_stats <- function(data) {
  .Deprecated("ds_summary_stats()")
}

#' @export
print.ds_summary_stats <- function(x, ...) {
  print_stats(x)
}
