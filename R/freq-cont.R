#' @title Frequency Distribution of Continuous Data
#' @description \code{freq_cont} returns the frequency distribution of
#' continuous by splitting the data into equidistant intervals created based on
#' the number of bins specified.
#' @param data numeric vector
#' @param bins number of intervals into which the data must be split
#' @return \code{freq_cont} returns an object of class \code{"freq_cont"}.
#' An object of class \code{"freq_cont"} is a list containing the
#' following components
#'
#' \item{breaks}{lower/upper boundaries of intervals}
#' \item{frequency}{frequecy of the intervals}
#' \item{cumulative}{cumulative frequency}
#' \item{percent}{frequency as percent}
#' \item{cum_percent}{cumulative frequency as percent}
#' \item{bins}{bins}
#' \item{data}{data}
#' \item{varname}{name of the data}
#' @examples
#' freq_cont(mtcars$mpg, 4)
#' @seealso \code{link{freq_table}} \code{link{cross_table}}
#' @export
#'
freq_cont <- function(data, bins = 5) UseMethod("freq_cont")

#' @rdname freq_cont
#' @export
freq_cont.default <- function(data, bins = 5) {

  if(!is.numeric(data)) {
    stop('data must be numeric')
  }

  if(!is.numeric(bins)) {
    stop('bins must be integer value')
  }

  if(is.numeric(bins)) {
    bins <- as.integer(bins)
  }

  var_name <- l(deparse(substitute(data)))
  data <- na.omit(data)
  n_bins <- bins
  inta <- intervals(data, bins)
  result <- freq(data, bins, inta)
  data_len <- length(data)
  cum <- cumsum(result)
  per <- percent(result, data_len)
  cum_per <- percent(cum, data_len)
  out <- list(breaks = inta,
              frequency = result,
              cumulative = cum,
              percent = per,
              cum_percent = cum_per,
              bins = n_bins,
              data = data,
              varname = var_name)

  class(out) <- "freq_cont"
  return(out)
}


#' @export
print.freq_cont <- function(x, ...) {
  print_fcont(x)
}


#' @importFrom graphics hist
#' @title Frequency Table Histogram
#' @description \code{hist.freq_cont} creates histogram
#' for the frequency table created using \code{freq_cont}
#' @param x an object of class \code{freq_cont}
#' @param col color of the bars
#' @param ... further arguments to be passed to or from methods
#' @examples
#' k <- freq_cont(mtcars$mpg, 4)
#' hist(k)
#' @export
#'
hist.freq_cont <- function(x, col = 'blue', ...) {

  ymax <- max(x$frequency) + 2
  h <-  hist(x$data, breaks = x$breaks,
        main = paste('Histogram of', x$varname),
        xlab = x$varname, ylab = 'Frequency', ylim = c(0, ymax), col = col)
  text(h$mids, h$counts + 1, labels = h$counts, adj = 0.5, pos = 1)

}


