#' @importFrom graphics hist
#' @importFrom magrittr use_series multiply_by add
#' @title Frequency Distribution of Continuous Data
#' @description \code{ds_freq_cont} returns the frequency distribution of
#' continuous by splitting the data into equidistant intervals created based on
#' the number of bins specified. \code{hist.ds_freq_cont} creates histogram
#' for the frequency table created using \code{ds_freq_cont}
#' @param data a \code{data.frame} or a \code{tibble}
#' @param variable numeric; column in \code{data}
#' @param bins number of intervals into which the data must be split
#' @param x an object of class \code{ds_freq_cont}
#' @param col color of the bars
#' @param ... further arguments to be passed to or from methods
#' @return \code{ds_freq_cont} returns an object of class \code{"ds_freq_cont"}
#' An object of class \code{"ds_freq_cont"} is a list containing the
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
#' @section Deprecated Functions:
#' \code{freq_cont()} has been deprecated. Instead use \code{ds_freq_cont()}.
#' @examples
#' # frequency table
#' ds_freq_cont(mtcarz, mpg, 4)
#'
#' # histogram
#' k <- ds_freq_cont(mtcarz, mpg, 4)
#' hist(k)
#' @seealso \code{link{ds_freq_table}} \code{link{ds_cross_table}}
#' @export
#'
ds_freq_cont <- function(data, variable, bins = 5) UseMethod("ds_freq_cont")


#' @export
ds_freq_cont.default <- function(data, variable,  bins = 5) {

  varyable <- enquo(variable)

  fdata <-
    data %>%
    pull(!! varyable) %>%
    na.omit

  if(!is.numeric(fdata)) {
    stop('variable must be numeric')
  }

  if(!is.numeric(bins)) {
    stop('bins must be integer value')
  }

  if(is.numeric(bins)) {
    bins <- as.integer(bins)
  }

  var_name <-
    data %>%
    select(!! varyable) %>%
    names

  n_bins <- bins
  inta <- intervals(fdata, bins)
  result <- freq(fdata, bins, inta)
  data_len <- length(fdata)
  cum <- cumsum(result)
  per <- percent(result, data_len)
  cum_per <- percent(cum, data_len)
  out <- list(breaks = inta,
              frequency = result,
              cumulative = cum,
              percent = per,
              cum_percent = cum_per,
              bins = n_bins,
              data = fdata,
              varname = var_name)

  class(out) <- "ds_freq_cont"
  return(out)
}

#' @export
#' @rdname ds_freq_cont
#' @usage NULL
#'
freq_cont <- function(data, bins = 5) {

  .Deprecated("ds_freq_cont()")

}


#' @export
print.ds_freq_cont <- function(x, ...) {
  print_fcont(x)
}

#' @rdname ds_freq_cont
#' @export
#'
hist.ds_freq_cont <- function(x, col = 'blue', ...) {

  ymax <- max(x$frequency) + 2
  h <-  hist(x$data, breaks = x$breaks,
        main = paste('Histogram of', x$varname),
        xlab = x$varname, ylab = 'Frequency', ylim = c(0, ymax), col = col)
  text(h$mids, h$counts + 1, labels = h$counts, adj = 0.5, pos = 1)

}
