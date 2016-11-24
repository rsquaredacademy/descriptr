#' @title Descriptive Statistics
#' @description \code{summary_stats} returns a whole range of descriptive
#' statistics for continuous data.
#' @param data a numeric vector
#' @return \code{summary_stats} returns an object of class 
#' \code{"summary_stats"}. An object of class \code{"summary_stats"} 
#' is a list containing the following components
#' 
#' \item{obs}{number of observations}
#' \item{missing}{number of missing observations}
#' \item{avg}{mean}
#' \item{tavg}{5 percent trimmed mean}
#' \item{stdev}{standard deviation}
#' \item{variance}{variance}
#' \item{skew}{skewness}
#' \item{kurtosis}{kurtosis}
#' \item{uss}{uncorrected sum of squares}
#' \item{css}{corrected sum of squares}
#' \item{cvar}{coefficient of variation}
#' \item{sem}{standard error of mean}
#' \item{median}{median}
#' \item{mode}{mode}
#' \item{range}{range}
#' \item{min}{minimum value}
#' \item{iqrange}{inter quartile range}
#' \item{per99}{99th percentile}
#' \item{per95}{95th percentile}
#' \item{per90}{90th percentile}
#' \item{per75}{75th percentile}
#' \item{per25}{25th percentile}
#' \item{per10}{10th percentile}
#' \item{per5}{5th percentile}
#' \item{per1}{1st percentile}
#' \item{lowobs}{five lowest observations}
#' \item{highobs}{five highest observations}
#' \item{lowobsi}{index of five lowest observations}
#' \item{highobsi}{index of five highest observations}
#' @examples
#' summary_stats(mtcars$mpg)
#' @seealso \code{\link[stats](summary)} \code{\link{freq_cont}}
#' \code{\link{freq_table}} \code{\link{cross_table}}
#' @export
#'
summary_stats <- function(data) UseMethod('summary_stats')

#' @rdname summary_stats
#' @export
summary_stats.default <- function(data) {

    if(!is.numeric(data)) {
      stop('data must be numeric')
    }

    data <- na.omit(data)
    low <- tailobs(data, 5, 'low')
    high <- tailobs(data, 5, 'high')
    low_val <- rindex(data, low)
    high_val <- rindex(data, high)

    result <- list(
        obs = length(data),
        missing = sum(is.na(data)),
        avg = mean(data),
        tavg = mean(data, trim = 0.05),
        stdev = sd(data),
        variance = var(data),
        skew = skewness(data),
        kurtosis = kurtosis(data),
        uss = stat_uss(data),
        css = stat_css(data),
        cvar = stat_cvar(data),
        sem = std_error(data),
        median = median(data),
        mode = stat_mode(data),
        range = stat_range(data),
        min = min(data), Max = max(data),
        iqrange = IQR(data),
        per99 = quantile(data, 0.99),
        per90 = quantile(data, 0.95),
        per95 = quantile(data, 0.90),
        per75 = quantile(data, 0.75),
        per25 = quantile(data, 0.25),
        per10 = quantile(data, 0.10),
        per5 = quantile(data, 0.05),
        per1 = quantile(data, 0.01),
        lowobs = low,
        highobs = high,
        lowobsi = low_val,
        highobsi = high_val
    )

    class(result) <- 'summary_stats'
    return(result)
}


print.summary_stats <- function(x) {
  print_stats(x)
}
