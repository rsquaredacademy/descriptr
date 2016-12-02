#' @importFrom grDevices topo.colors
#' @title Frequency Table: Categorical Data
#' @description \code{freq_table} creates frequency table for factor data and
#' returns the frequency, cumulative frequency, frequency percent and cumulative
#' frequency percent. \code{barplot.freq_table} creates bar plot for the
#' frequency table created using \code{freq_table}
#' @param data numeric or factor vector
#' @param height an object of class \code{freq_table}
#' @param ... further arguments to be passed to or from methods
#' @return \code{freq_table} returns an object of class \code{"freq_table"}.
#' An object of class \code{"freq_table"} is a list containing the
#' following components
#'
#' \item{ftable}{frequency table}
#' \item{varname}{name of the data}
#' @examples
#' # frequency table
#' freq_table(mtcars$cyl)
#' freq_table(as.factor(mtcars$cyl))
#'
#' # barplot
#' k <- freq_table(mtcars$cyl)
#' barplot(k)
#' @seealso \code{link{freq_cont}} \code{link{cross_table}}
#' @export
#'
freq_table <- function(data) UseMethod("freq_table")

#' @export
freq_table.default <- function(data) {

  if ((!is.numeric(data)) & (!is.factor(data))) {
    stop('data must be either numeric or factor')
  }

  var_name = l(deparse(substitute(data)))
  data <- na.omit(data)

  if (is.factor(data)) {
    level_names <- levels(data)
  }

  data1 <- data
  data <- as.numeric(data)
  data_len <- length(data)
  cq <- unique(sort(data))
  result <- as.vector(table(data))
  len <- length(result)
  cum <- cumsum(result)
  per <- percent(result, data_len)
  cum_per <- percent(cum, data_len)

  if (is.factor(data1)) {
    ftable <- cbind(level_names, result, cum, per, cum_per)
  } else {
    ftable <- cbind(cq, result, cum, per, cum_per)
  }

  colnames(ftable) <- c("Levels", "Frequency", "Cum Frequency",
                        "Percent", "Cum Percent")

  result <- list(
    ftable = ftable,
    varname = var_name
  )

  class(result) <- "freq_table"
  return(result)

}

#" @export
print.freq_table <- function(data) {
  print_ftable(data)
}



#' @rdname freq_table
#' @export
#'
barplot.freq_table <- function(height, ...) {
    j <- as.numeric(height$ftable[, 2])
    h <- j
    ymax <- max(h)
    cols <- length(j)
    x_names <- height$ftable[, 1]
    k <- barplot(j, col = topo.colors(cols),
                 main = paste('Bar Plot of', height$varname),
                 xlab = height$varname,
                 ylab = 'Frequency',
                 ylim = c(0, ymax[1]),
                 names.arg = x_names)
    graphics::text(k, h, labels = j, adj = 0.5, pos = 1)
}
