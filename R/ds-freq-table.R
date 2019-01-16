#' Frequency table
#'
#' Frequency table for factor data and returns the frequency, cumulative
#' frequency, frequency percent and cumulative frequency percent.
#' \code{barplot.ds_freq_table()} creates bar plot for the
#' frequency table created using \code{ds_freq_table()}.
#'
#' @param data A \code{data.frame} or a \code{tibble}.
#' @param variable Column in \code{data}.
#' @param x An object of class \code{ds_freq_table}.
#' @param ... Further arguments to be passed to or from methods.
#'
#' @return \code{ds_freq_table} returns an object of class \code{"ds_freq_table"}.
#' An object of class \code{"ds_freq_table"} is a list containing the
#' following components:
#'
#' \item{ftable}{Frequency table.}
#'
#' @examples
#' # frequency table
#' ds_freq_table(mtcarz, cyl)
#'
#' # barplot
#' k <- ds_freq_table(mtcarz, cyl)
#' plot(k)
#'
#' @seealso \code{\link{ds_freq_cont}} \code{\link{ds_cross_table}}
#'
#' @export
#'
ds_freq_table <- function(data, variable) UseMethod("ds_freq_table")

#' @export
ds_freq_table.default <- function(data, variable) {

  varyable <- rlang::enquo(variable)

  fdata <-
    data %>%
    dplyr::pull(!! varyable) %>%
    stats::na.omit()

  if (!is.factor(fdata)) {
    stop("variable must be categorical/qualitative")
  }

  var_name <-
    data %>%
    dplyr::select(!! varyable) %>%
    names()

  level_names <- levels(fdata)
  data_len    <- length(fdata)
  cq          <- forcats::fct_unique(fdata)

  result <-
    fdata %>%
    forcats::fct_count() %>%
    dplyr::pull(2)

  len     <- length(result)
  cum     <- cumsum(result)
  per     <- percent(result, data_len)
  cum_per <- percent(cum, data_len)

  ftable <- tibble::tibble(
    Levels          = level_names,
    Frequency       = result,
    `Cum Frequency` = cum,
    Percent         = per,
    `Cum Percent`   = cum_per
  )

  na_count <-
    data %>%
    dplyr::pull(!! varyable) %>%
    is.na() %>%
    sum()

  if (na_count > 0) {
    na_freq <-
      data %>%
      dplyr::pull(!! varyable) %>%
      forcats::fct_count() %>%
      dplyr::pull(n) %>%
      dplyr::last()
  } else {
    na_freq <- 0
  }

  n_obs <-
    data %>%
    dplyr::pull(!! varyable) %>%
    length()

  result <- list(
    ftable   = ftable,
    varname  = var_name,
    data     = data,
    na_count = na_freq,
    n        = n_obs
  )

  class(result) <- "ds_freq_table"
  return(result)
}

#' @export
print.ds_freq_table <- function(x, ...) {
  print_ftable(x)
}

#' @rdname ds_freq_table
#' @export
#'
plot.ds_freq_table <- function(x, ...) {
  x_lab <-
    x %>%
    magrittr::use_series(varname) %>%
    magrittr::extract(1)

  k <-
    x %>%
    magrittr::use_series(varname) %>%
    magrittr::extract(1) %>%
    rlang::sym()

  p <-
    x %>%
    magrittr::use_series(data) %>%
    dplyr::select(x = !! k) %>%
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = x), fill = "blue") +
    ggplot2::xlab(x_lab) + ggplot2::ylab("Count") +
    ggplot2::ggtitle(paste("Bar plot of", x_lab))

  print(p)

  result <- list(plot = p)
  invisible(result)
}
