#' @title Multiple One & Two Way Tables
#' @description \code{ds_oway_tables} creates multiple one way tables by creating
#' a frequency table for each categorical variable in a data frame.
#' \code{ds_tway_tables} creates multiple two way tables by creating a cross
#' table for each unique pair of categorical variables in a data frame.
#' @param data a data frame
#' @details \code{ds_oway_tables} is a extension of the \code{ds_freq_table}
#' function. It creates a frequency table for each categorical variable in the
#' dataframe. \code{ds_tway_tables} is a extension of the \code{ds_cross_table}
#' function. It creates a two way table for each unique pair of categorical
#' variables in the dataframe.
#' @section Deprecated Functions:
#' \code{oway_tables()} and \code{tway_tables()} have been deprecated. Instead
#' use \code{ds_oway_tables()} and \code{ds_tway_tables()}.
#' @examples
#' # multiple one way tables
#' ds_oway_tables(mtcarz)
#'
#' # multiple two way tables
#' ds_tway_tables(mtcarz)
#' @seealso \code{link{ds_freq_table}} \code{link{ds_cross_table}}
#' @export
#'
ds_oway_tables <- function(data) {

  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  is.fact <- sapply(data, is.factor)

  if (!any(is.fact == TRUE)) {
    stop("data has no factor variables")
  }

  factors.df <- data[, is.fact]
  nam        <- names(factors.df)
  nc         <- ncol(factors.df)

  for (i in seq_len(nc)) {
    k <- freq_table2(factors.df[i], nam[i])
    print(k)
  }

}

#' @export
#' @rdname ds_oway_tables
#' @usage NULL
#'
oway_tables <- function(data) {
  .Deprecated("ds_oway_tables()")
  ds_oway_tables(data)
}


#' @rdname ds_oway_tables
#' @export
#'
ds_tway_tables <- function(data) {

  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  is.fact <- sapply(data, is.factor)

  if (sum(is.fact) < 2) {
    stop("data must have at least two factor variables")
  }

  factors.df <- data[, is.fact]
  nc         <- ncol(factors.df)
  nam        <- names(factors.df)
  n          <- nc - 1

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
    p <- i + 1
    for (j in p:nc) {
      k <- cross_table2(factors.df[[i]], factors.df[[j]], nam[i], nam[j])
      print(k)
    }
  }
}

#' @export
#' @rdname ds_oway_tables
#' @usage NULL
#'
tway_tables <- function(data) {
  .Deprecated("ds_tway_tables()")
  ds_tway_tables(data)
}
