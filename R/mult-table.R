#' @title Multiple One & Two Way Tables
#' @description \code{oway_tables} creates multiple one way tables by creating
#' a frequency table for each categorical variable in a data frame.
#' \code{tway_tables} creates multiple two way tables by creating a cross
#' table for each unique pair of categorical variables in a data frame.
#' @param data a data frame
#' @details \code{oway_tables} is a extension of the \code{freq_table}
#' function. It creates a frequency table for each categorical variable in the
#' dataframe. \code{tway_tables} is a extension of the \code{cross_table}
#' function. It creates a two way table for each unique pair of categorical
#' variables in the dataframe.
#' @examples
#' mt <- mtcars
#' mt[, c(2, 8, 9)] <- lapply(mt[, c(2, 8, 9)], factor)
#' # multiple one way tables
#' oway_tables(mt)
#'
#' # multiple two way tables
#' tway_tables(mt)
#' @seealso \code{link{freq_table}} \code{link{cross_table}}
#' @export
#'
oway_tables <- function(data) {

    if (!is.data.frame(data)) {
        stop('data must be a data frame')
    }

    is.fact <- sapply(data, is.factor)

    if (!any(is.fact == TRUE)) {
        stop('data has no factor variables')
    }

    factors.df <- data[, is.fact]
    nam <- names(factors.df)
    nc <- ncol(factors.df)
    for (i in seq_len(nc)) {
        k <- freq_table2(factors.df[i], nam[i])
        print(k)
    }

}


#' @rdname oway_tables
#' @export
#'
tway_tables <- function(data) {

    if (!is.data.frame(data)) {
        stop('data must be a data frame')
    }

    is.fact <- sapply(data, is.factor)

    if (sum(is.fact) < 2) {
        stop('data must have at least two factor variables')
    }

    factors.df <- data[, is.fact]
    nc <- ncol(factors.df)
    nam <- names(factors.df)
    n <- nc - 1

    cat(formatter("    Cell Contents\n"),
    "|---------------|\n",
    "|", formatter("Frequency"), "|\n",
    "|", formatter("Percent"), "|\n",
    "|", formatter("Row Pct"), "|\n",
    "|", formatter("Col Pct"), "|\n",
    "|---------------|\n\n",
    "Total Observations: ", nrow(data), "\n\n")

    for (i in seq_len(n)) {
        p <- i + 1
        for (j in p:nc) {
            k <- cross_table2(factors.df[[i]], factors.df[[j]], nam[i], nam[j])
            print(k)
        }
    }

}
