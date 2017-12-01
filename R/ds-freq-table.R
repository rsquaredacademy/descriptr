#' @importFrom grDevices topo.colors
#' @importFrom tibble tibble
#' @importFrom dplyr pull
#' @title Frequency Table: Categorical Data
#' @description \code{ds_freq_table} creates frequency table for factor data and
#' returns the frequency, cumulative frequency, frequency percent and cumulative
#' frequency percent. \code{barplot.ds_freq_table} creates bar plot for the
#' frequency table created using \code{ds_freq_table}
#' @param data numeric or factor vector
#' @param height an object of class \code{ds_freq_table}
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
#' mt <- mtcars
#' mt$cyl <- as.factor(mt$cyl)
#' ds_freq_table(mt$cyl)
#'
#' # barplot
#' mt <- mtcars
#' mt$cyl <- as.factor(mt$cyl)
#' k <- ds_freq_table(mt$cyl)
#' barplot(k)
#' @seealso \code{link{ds_freq_cont}} \code{link{ds_cross_table}}
#' @export
#'
ds_freq_table <- function(data) UseMethod("ds_freq_table")

#' @export
ds_freq_table.default <- function(data) {

  if (!is.factor(data)) {
    stop('data must be categorical/qualitative')
  }

  var_name = l(deparse(substitute(data)))
  data <- na.omit(data)
  level_names <- levels(data)
  data_len <- length(data)

  # unique values in the input
  cq <- forcats::fct_unique(data)

  # count of unique values in the input
  result <- data %>%
    fct_count %>%
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
  ftable <- tibble(Levels = level_names,
                   Frequency = result,
                   `Cum Frequency` = cum,
                   Percent = per,
                   `Cum Percent` = cum_per)


  result <- list(
    ftable = ftable,
    varname = var_name
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
  ds_freq_table(data)

}

#' @export
print.ds_freq_table <- function(x, ...) {
  print_ftable(x)
}



#' @rdname freq_table
#' @export
#'
barplot.ds_freq_table <- function(height, ...) {
    j <- as.numeric(height$ftable[[2]])
    h <- j
    ymax <- max(h)
    cols <- length(j)
    x_names <- height$ftable[[1]]
    k <- barplot(j, col = topo.colors(cols),
                 main = paste('Bar Plot of', height$varname),
                 xlab = height$varname,
                 ylab = 'Frequency',
                 ylim = c(0, ymax[1]),
                 names.arg = x_names)
    graphics::text(k, h, labels = j, adj = 0.5, pos = 1)
}
