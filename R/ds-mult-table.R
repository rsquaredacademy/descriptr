#' @title Multiple One & Two Way Tables
#' @description \code{ds_auto_freq_table} creates multiple one way tables by creating
#' a frequency table for each categorical variable in a data frame.
#' \code{ds_auto_cross_table} creates multiple two way tables by creating a cross
#' table for each unique pair of categorical variables in a data frame.
#' @param data A \code{data.frame} or \code{tibble}.
#' @param ... Column(s) in \code{data}.
#' @details \code{ds_auto_freq_table} is a extension of the \code{ds_freq_table}
#' function. It creates a frequency table for each categorical variable in the
#' dataframe. \code{ds_auto_cross_table} is a extension of the \code{ds_cross_table}
#' function. It creates a two way table for each unique pair of categorical
#' variables in the dataframe.
#' @section Deprecated Functions:
#' \code{ds_oway_tables()} and \code{ds_tway_tables()} have been deprecated.
#' Instead use \code{ds_auto_freq_table()} and \code{ds_auto_cross_table()}.
#' @examples
#' # frequency table for all columns
#' ds_auto_freq_table(mtcarz)
#'
#' # frequency table for multiple columns
#' ds_auto_freq_table(mtcarz, cyl, gear)
#'
#' # cross table for all columns
#' ds_auto_cross_table(mtcarz)
#'
#' # cross table for multiple columns
#' ds_auto_cross_table(mtcarz, cyl, gear, am)
#'
#' @seealso \code{link{ds_freq_table}} \code{link{ds_cross_table}}
#' @export
#'
ds_auto_freq_table <- function(data, ...) {

  check_df(data)
  var <- rlang::quos(...)
  is_factor <- sapply(data, is.factor)

  if (length(var) < 1) {
    if (!any(is_factor == TRUE)) {
      stop("Data has no categorical variables.", call. = FALSE)
    }
    plot_data <- data[is_factor]
  } else {
    data %<>%
      dplyr::select(!!! var)
    is_factor <- sapply(data, is.factor)
    if (!any(is_factor == TRUE)) {
      stop("Data has no categorical variables.", call. = FALSE)
    }
    plot_data <- data[is_factor]
  }

  factor_var <- names(plot_data)
  n          <- length(factor_var)

  for (i in seq_len(n)) {
    k <- freq_table2(plot_data[i], factor_var[i])
    print(k)
  }

}



#' @rdname ds_auto_freq_table
#' @export
#'
ds_auto_cross_table <- function(data, ...) {

  check_df(data)
  var <- rlang::quos(...)
  is_factor <- sapply(data, is.factor)

  if (length(var) < 1) {
    if (!any(is_factor == TRUE)) {
      stop("Data has no categorical variables.", call. = FALSE)
    }
    plot_data <- data[is_factor]
  } else {
    data %<>%
      dplyr::select(!!! var)
    is_factor <- sapply(data, is.factor)
    if (!any(is_factor == TRUE)) {
      stop("Data has no categorical variables.", call. = FALSE)
    }
    if (length(is_factor) < 2) {
      stop("Two way table requires at least 2 categorical variables.", call. = FALSE)
    } else {
      plot_data <- data[is_factor]
    }
  }

  factor_var    <- names(plot_data)
  factor_start  <- combn(factor_var, 2)
  n             <- dim(factor_start)[2]

  cat(
    formatter("    Cell Contents\n"),
    "|---------------|\n",
    "|", formatter("Frequency"), "|\n",
    "|", formatter("Percent"), "|\n",
    "|", formatter("Row Pct"), "|\n",
    "|", formatter("Col Pct"), "|\n",
    "|---------------|\n\n",
    "Total Observations: ", nrow(data), "\n\n"
  )

  for (i in seq_len(n)) {
    k <- cross_table2(plot_data[[factor_start[, i][1]]], plot_data[[factor_start[, i][2]]],
      factor_start[, i][1], factor_start[, i][2])
    print(k)
  }
}

#' @export
#' @rdname ds_auto_freq_table
#' @usage NULL
#'
ds_tway_tables <- function(data) {
  .Deprecated("ds_auto_cross_table()")
  ds_auto_cross_table(data)
}

#' @export
#' @rdname ds_auto_freq_table
#' @usage NULL
#'
ds_oway_tables <- function(data) {
  .Deprecated("ds_auto_freq_table()")
  ds_auto_freq_table(data)
}
