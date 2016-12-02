#' @importFrom graphics legend
#' @importFrom stats complete.cases
#' @title Screen Data Frames
#' @description screener will screen data frames and return details such
#' as variable names, class, levels and missing values. \code{plot.screener}
#' creates bar plots to visualize % of missing observations for each variable
#' in a data frame.
#' @param y a data frame
#' @param x an object of class \code{screener}
#' @param ... further arguments to be passed to or from methods
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
#' # screen data
#' mt <- mtcars
#' mt[, c(2, 8:11)] <- lapply(mt[, c(2, 8:11)], factor)
#' mt[sample(1:nrow(mt), 12), sample(1:ncol(mt), 6)] <- NA
#' screener(mt)
#'
#' # visualize missing data
#' k <- screener(mt)
#' plot(k)
#' @export
#'
screener <- function(y) UseMethod('screener')

#' @export
#'
screener.default <- function(y) {

    if (!is.data.frame(y)) {
        stop('y must be a data frame')
    }

    rows <- nrow(y)
    cols <- ncol(y)
    varnames <- names(sapply(y, colnames))
    datatype <- sapply(y, class)
    counts <- sapply(y, length)
    nlev <- lapply(y, nlevels)
    lev <- lapply(y, levels)
    for (i in seq_len(length(lev))) {
        if (is.null(lev[[i]])) {
            lev[[i]] <- NA
        }
    }
    mvalues <- sapply(y, function(z) sum(is.na(z)))
    mvaluesper <- round((mvalues / counts) * 100, 2)

    mtotal <- sum(is.na(y))
    mtotalper <- round((mtotal / sum(counts)) * 100, 2)
    mrows <- sum(!complete.cases(y))
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


#' @export
print.screener <- function(x, ...) {
    print_screen(x)
}



#' @rdname screener
#' @export
#'
plot.screener <- function(x, ...) {

    dat <- x$MissingPer
    ymax <- max(dat) * 1.5
    cols <- c("green", "red")[(dat > 10) + 1]
    h <- barplot(dat, main = "Missing Values (%)",
                 xlab = 'Column Names', ylab = 'Percentage',
                 col = cols, ylim = c(0, ymax))
    legend('top', legend = c('> 10%', '<= 10%'), fill = c('red', 'green'),
        horiz = TRUE, title = '% Missing', cex = 0.5, text.width = 0.7)
    line_data <- cbind(h, as.vector(dat))
    text(line_data[, 1], line_data[, 2] + 2, as.vector(dat))
}
