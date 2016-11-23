#' @title Screen Data Frames
#' @description screener will screen data frames and return details such
#' as variable names, class, levels and missing values.
#' @param x a data frame
#' @details screener is a s3 generic function
#' @return \code{screeneer} returns an object of class \code{"screener"}.
#' An object of class \code{"screener"} is a list containing the
#' following components
#'
#' \item{Rows}{number of rows in the data frame}
#' \item{Columns}{number of columns in the data frame}
#' \item{Variables}{names of the variables in the data frame}
#' \item{Types}{class of the variables in the data frame}
#' \item{Count}{length of the variables in the data frame}
#' \item{nlevels}{number of levels of a factor variable}
#' \item{levels}{levels of factor variables in the data frame}
#' \item{Missing}{number of missing observations in each variable}
#' \item{MissingPer}{Percent of missing observations in each variable}
#' \item{MissingTotal}{total number of missing observations in the data frame}
#' \item{MissingTotPer}{total percent of missing observations in the data frame}
#' \item{MissingRows}{total number of rows with missing observations in the
#' data frame}
#' \item{MissingCols}{total number of columns with missing observations in the
#' data frame}
#' @examples
#' mt <- mtcars
#' mt[, c(2, 8:11)] <- lapply(mt[, c(2, 8:11)], factor)
#' mt[sample(1:n, 12), sample(1:cl, 6)] <- NA
#' screener(mt)
#' @export
#'
screener <- function(x) UseMethod('screener')

#' @export
#' @rdname screener
screener.default <- function(x) {

    if (!is.data.frame(x)) {
        stop('x must be a data frame')
    }

    rows <- nrow(x)
    cols <- ncol(x)
    varnames <- names(sapply(x, colnames))
    datatype <- sapply(x, class)
    counts <- sapply(x, length)
    nlev <- lapply(x, nlevels)
    lev <- lapply(x, levels)
    for (i in seq_len(length(lev))) {
        if (is.null(lev[[i]])) {
            lev[[i]] <- NA
        }
    }
    mvalues <- sapply(x, function(y) sum(is.na(y)))
    mvaluesper <- round((mvalues / counts) * 100, 2)

    mtotal <- sum(is.na(x))
    mtotalper <- round((mtotal / sum(counts)) * 100, 2)
    mrows <- sum(!complete.cases(x))
    mcols <- sum(mvalues != 0)

    result <- list(Rows = rows, Columns = cols, Variables = varnames,
                   Types = datatype, Count = counts, nlevels = nlev,
                   levels = lev, Missing = mvalues,
                   MissingPer = mvaluesper, MissingTotal = mtotal,
                   MissingTotPer = mtotalper, MissingRows = mrows,
                   MissingCols = mcols)

    class(result) <- 'screener'

    return(result)
}


print.screener <- function(x, ...) {

    print_screen(x)

}

#' @importFrom graphics barplot
#' @importFrom grDevices heat.colors
#' @title Visualize Missing Values
#' @description \code{plot.screener} creates bar plots to visualize % of missing
#' observations for each variable in a data frame
#' @param x an object of class \code{screener}
#' @param ... further arguments to be passed to or from methods
#' @examples
#' mt <- mtcars
#' mt[, c(2, 8:11)] <- lapply(mt[, c(2, 8:11)], factor)
#' mt[sample(1:n, 12), sample(1:cl, 6)] <- NA
#' k <- screener(mt)
#' plot(k)
#' @export
#'
plot.screener <- function(x, ...) {

    dat <- x$MissingPer
    ymax <- max(dat) + 3
    h <- barplot(dat, main = "Missing Values (%)",
                 xlab = 'Column Names', ylab = 'Percentage',
                 col = heat.colors(length(dat)), ylim = c(0, ymax))
    line_data <- cbind(h, as.vector(dat))
    text(line_data[, 1], line_data[, 2] + 2, as.vector(dat))
}
