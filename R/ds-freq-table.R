#' Frequency table
#'
#' Frequency table for categorical and continuous data and returns the 
#' frequency, cumulative frequency, frequency percent and cumulative frequency 
#' percent. \code{plot.ds_freq_table()} creates bar plot for the categorical 
#' data and histogram for continuous data.
#' 
#' @param data A \code{data.frame} or a \code{tibble}.
#' @param variable Column in \code{data}.
#' @param x An object of class \code{ds_freq_table}.
#' @param bins Number of intervals into which the data must be split.
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
ds_freq_table <- function(data, variable, bins = 5) UseMethod("ds_freq_table")

#' @export
#' 
ds_freq_table.default <- function(data, variable, bins = 5) {

	varyable <- rlang::enquo(variable)
	data %<>%
      dplyr::select(!! varyable)
  is_num <- sapply(data, is.numeric)
  is_factor <- sapply(data, is.factor)

  if (is_num) {
  	result <- ds_freq_numeric(data, !! rlang::enquo(variable), bins)
  } else if (is_factor) {
  	result <- ds_freq_factor(data, !! rlang::enquo(variable))
  } else {
  	var_name <- deparse(substitute(variable))
  	rlang::abort(paste0(var_name, "is neither continuous nor categorical."))
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

#' @export
#'
plot.ds_freq_table <- function(x, ...) {

	nx <- length(x)
	if (nx == 5) {
		plot_ds_freq_factor(x)
	} else if (nx == 11) {
		plot_ds_freq_numeric(x)
	} else {
		NULL
	}

}
