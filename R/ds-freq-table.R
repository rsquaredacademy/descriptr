#' Frequency table
#'
#' Frequency table for categorical and continuous data and returns the
#' frequency, cumulative frequency, frequency percent and cumulative frequency
#' percent. \code{plot.ds_freq_table()} creates bar plot for the categorical
#' data and histogram for continuous data.
#'
#' @param data A \code{data.frame} or a \code{tibble}.
#' @param col Column in \code{data}.
#' @param x An object of class \code{ds_freq_table}.
#' @param bins Number of intervals into which the data must be split.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#' @param ... Further arguments to be passed to or from methods.
#'
#' @examples
#' # categorical data
#' ds_freq_table(mtcarz, cyl)
#'
#' # barplot
#' k <- ds_freq_table(mtcarz, cyl)
#' plot(k)
#'
#' # continuous data
#' ds_freq_table(mtcarz, mpg)
#'
#' # barplot
#' k <- ds_freq_table(mtcarz, mpg)
#' plot(k)
#'
#' @seealso \code{\link{ds_cross_table}}
#'
#' @export
#'
ds_freq_table <- function(data, col, bins = 5) UseMethod("ds_freq_table")

#' @export
#'
ds_freq_table.default <- function(data, col, bins = 5) {

	varyable <- rlang::enquo(col)
	data %<>%
      dplyr::select(!! varyable)
  is_num    <- sapply(data, is.numeric)
  is_factor <- sapply(data, is.factor)

  if (is_num) {
  	result <- ds_freq_numeric(data, !! rlang::enquo(col), bins)
  } else if (is_factor) {
  	result <- ds_freq_factor(data, !! rlang::enquo(col))
  } else {
  	var_name <- deparse(substitute(col))
  	stop(paste0(var_name, "is neither continuous nor categorical."), call. = FALSE)
  }

  class(result) <- "ds_freq_table"
  return(result)

}

#' @export
#'
print.ds_freq_table <- function(x, ...) {

	nx <- length(x)
	if (nx == 5) {
		print_ftable(x)
	} else if (nx == 11) {
		print_fcont(x)
	} else {
		NULL
	}

}

#' @rdname ds_freq_table
#' @export
#'
plot.ds_freq_table <- function(x, print_plot = TRUE, ...) {

	nx <- length(x)
	if (nx == 5) {
		p <- plot_ds_freq_factor(x)
		if (print_plot) {
		  print(p)
		} else {
		  return(p)
		}
	} else if (nx == 11) {
		p <- plot_ds_freq_numeric(x)
		if (print_plot) {
		  print(p)
		} else {
		  return(p)
		}
	} else {
		NULL
	}

}

#' @export
#' @rdname ds_freq_table
#' @usage NULL
#'
ds_freq_cont <- function(data, variable, bins) {
  .Deprecated("ds_freq_table()")
  ds_freq_table(data, variable, bins)
}
