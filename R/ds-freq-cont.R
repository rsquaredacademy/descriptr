#' Frequency distribution of continuous data
#'
#' Frequency distribution of continuous data by splitting into equidistant
#' intervals created based on the number of bins specified.
#' \code{hist.ds_freq_cont()} creates histogram for the frequency table
#' created using \code{ds_freq_cont()}.
#'
#' @param data A \code{data.frame} or a \code{tibble}.
#' @param variable Column in \code{data}.
#' @param bins Number of intervals into which the data must be split.
#' @param x An object of class \code{ds_freq_cont}.
#' @param ... Further arguments to be passed to or from methods.
#'
#' @return \code{ds_freq_cont()} returns an object of class \code{"ds_freq_cont"}
#' An object of class \code{"ds_freq_cont"} is a list containing the
#' following components
#'
#' \item{breaks}{Lower/upper boundaries of intervals.}
#' \item{frequency}{Frequecy of the intervals.}
#' \item{cumulative}{Cumulative frequency.}
#' \item{percent}{Frequency as percent.}
#' \item{cum_percent}{Cumulative frequency as percent.}
#' \item{bins}{Number of bins.}
#' \item{data}{Data.}
#' \item{varname}{Name of the variable.}
#'
#' @section Deprecated functions:
#' \code{freq_cont()} has been deprecated. Instead use \code{ds_freq_cont()}.
#'
#' @importFrom graphics hist
#' @importFrom magrittr use_series multiply_by add
#'
#' @examples
#' # frequency table
#' ds_freq_cont(mtcarz, mpg, 4)
#'
#' # histogram
#' k <- ds_freq_cont(mtcarz, mpg, 4)
#' plot(k)
#'
#' @seealso \code{\link{ds_freq_table}} \code{\link{ds_cross_table}}
#'
#' @export
#'
ds_freq_cont <- function(data, variable, bins = 5) UseMethod("ds_freq_cont")


#' @export
ds_freq_cont.default <- function(data, variable, bins = 5) {

  varyable <- enquo(variable)

  fdata <-
    data %>%
    pull(!! varyable) %>%
    na.omit()

  if (!is.numeric(fdata)) {
    stop("variable must be numeric")
  }

  if (!is.numeric(bins)) {
    stop("bins must be integer value")
  }

  if (is.numeric(bins)) {
    bins <- as.integer(bins)
  }

  var_name <-
    data %>%
    select(!! varyable) %>%
    names()

  n_bins   <- bins
  inta     <- intervals(fdata, bins)
  result   <- freq(fdata, bins, inta)
  data_len <- length(fdata)
  cum      <- cumsum(result)
  per      <- percent(result, data_len)
  cum_per  <- percent(cum, data_len)

  na_count <-
    data %>%
    pull(!! varyable) %>%
    is.na() %>%
    sum()

  if (na_count > 0) {
    na_freq <- na_count
  } else {
    na_freq <- 0
  }

  n_obs <-
    data %>%
    pull(!! varyable) %>%
    length()

  lower_n <- n_bins + 1

  freq_data <-
    tibble(lower = inta[-lower_n],
           upper = inta[-1],
           frequency = result,
           cumulative = cum,
           freq_percent = per,
           cum_percent = cum_per)

  out <- list(
    freq_data = freq_data,
    breaks = inta,
    frequency = result,
    cumulative = cum,
    percent = per,
    cum_percent = cum_per,
    bins = n_bins,
    data = fdata,
    na_count = na_freq,
    n = n_obs,
    varname = var_name
  )

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

#' @importFrom tibble add_column
#' @importFrom ggplot2 geom_col
#' @rdname ds_freq_cont
#' @export
#'
plot.ds_freq_cont <- function(x, ...) {
  x_lab <-
    x %>%
    use_series(varname)

  k <-
    x %>%
    use_series(varname) %>%
    extract(1) %>%
    sym()

  bins <-
    x %>%
    use_series(frequency) %>%
    length()

  p <-
    x %>%
    use_series(frequency) %>%
    as_tibble() %>%
    add_column(x = seq_len(bins), .before = 1) %>%
    ggplot() +
    geom_col(
      aes(x = x, y = value), width = 0.999,
      fill = "blue", color = "black"
    ) +
    xlab(x_lab) + ylab("Count") +
    ggtitle(paste("Histogram of", x_lab))

  print(p)

  result <- list(plot = p)
  invisible(result)
}
