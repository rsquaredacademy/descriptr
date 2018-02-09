#' @importFrom grDevices topo.colors
#' @importFrom tibble tibble
#' @importFrom dplyr pull
#' @title Frequency Table: Categorical Data
#' @description \code{ds_freq_table} creates frequency table for factor data and
#' returns the frequency, cumulative frequency, frequency percent and cumulative
#' frequency percent. \code{barplot.ds_freq_table} creates bar plot for the
#' frequency table created using \code{ds_freq_table}
#' @param data a \code{data.frame} or a \code{tibble}
#' @param variable column in \code{data}
#' @param x an object of class \code{ds_freq_table}
#' @param ... further arguments to be passed to or from methods
#' @return \code{ds_freq_table} returns an object of class \code{"ds_freq_table"}.
#' An object of class \code{"ds_freq_table"} is a list containing the
#' following components
#'
#' \item{ftable}{frequency table}
#' \item{varname}{name of the data}
#' @section Deprecated Function:
#' \code{freq_table()} has been deprecated. Instead use \code{ds_freq_table()}.
#' @examples
#' # frequency table
#' ds_freq_table(mtcarz, cyl)
#'
#' # barplot
#' k <- ds_freq_table(mtcarz, cyl)
#' plot(k)
#' @seealso \code{link{ds_freq_cont}} \code{link{ds_cross_table}}
#' @export
#'
ds_freq_table <- function(data, variable) UseMethod("ds_freq_table")

#' @export
ds_freq_table.default <- function(data, variable) {
  varyable <- enquo(variable)

  fdata <-
    data %>%
    pull(!! varyable) %>%
    na.omit()

  if (!is.factor(fdata)) {
    stop("variable must be categorical/qualitative")
  }

  var_name <-
    data %>%
    select(!! varyable) %>%
    names()

  level_names <- levels(fdata)
  data_len <- length(fdata)

  # unique values in the input
  cq <- forcats::fct_unique(fdata)

  # count of unique values in the input
  result <- fdata %>%
    fct_count() %>%
    pull(2)

  # length of result
  len <- length(result)

  # cumulative frequency
  cum <- cumsum(result)

  # percent
  per <- percent(result, data_len)

  # cumulative percent
  cum_per <- percent(cum, data_len)

  # matrix
  ftable <- tibble(
    Levels = level_names,
    Frequency = result,
    `Cum Frequency` = cum,
    Percent = per,
    `Cum Percent` = cum_per
  )


  result <- list(
    ftable = ftable,
    varname = var_name,
    data = data
  )

  class(result) <- "ds_freq_table"
  return(result)
}

#' @export
#' @rdname ds_freq_table
#' @usage NULL
#'
freq_table <- function(data) {
  .Deprecated("ds_freq_table()")
}

#' @export
print.ds_freq_table <- function(x, ...) {
  print_ftable(x)
}


#' @importFrom ggplot2 ylab
#' @rdname ds_freq_table
#' @export
#'
plot.ds_freq_table <- function(x, ...) {
  x_lab <-
    x %>%
    use_series(varname) %>%
    extract(1)

  k <-
    x %>%
    use_series(varname) %>%
    extract(1) %>%
    sym()

  p <-
    x %>%
    use_series(data) %>%
    select(x = !! k) %>%
    ggplot() +
    geom_bar(aes(x = x), fill = "blue") +
    xlab(x_lab) + ylab("Count") +
    ggtitle(paste("Bar plot of", x_lab))

  print(p)

  result <- list(plot = p)
  invisible(result)
}
