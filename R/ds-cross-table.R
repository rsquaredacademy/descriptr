#' @importFrom graphics barplot mosaicplot
#' @importFrom grDevices rainbow
#' @title Two Way Tables
#' @description \code{ds_cross_tablecross_table} creates two way tables of categorical
#' variables. The tables created can be visualized as barplots and mosaicplots.
#' @param var1 First categorical variable
#' @param var2 Second categorical variable
#' @param x An object of class cross_table
#' @param beside a logical value. If FALSE, the columns of height are portrayed
#' as stacked bars, and if TRUE the columns are portrayed as juxtaposed bars.
#' @param proportional a logical value. If TRUE, the height of the bars is
#' proportional
#' @param ... further arguments to be passed to or from methods.
#' @return \code{ds_cross_table} returns an object of class \code{"ds_cross_table"}.
#' An object of class \code{"ds_cross_table"} is a list containing at least the
#' following components:
#'
#' \item{obs}{obs number of observations}
#' \item{var2_levels}{levels of the second categorical variable}
#' \item{var1_levels}{levels of the first categorical variable}
#' \item{varnames}{names of the variables}
#' \item{twowaytable}{table of the variables}
#' \item{percent_table}{table of percentages}
#' \item{row_percent}{table of row percentages}
#' \item{col_percent}{table of column percentages}
#' \item{column_totals}{total of columns}
#' \item{percent_column}{total of columns as a percentage}
#' @section Deprecated Function:
#' \code{ds_cross_table()} has been deprecated. Instead use \code{ds_cross_table()}.
#' @examples
#' k <- ds_cross_table(mtcars$cyl, mtcars$am)
#' k
#'
#' # bar plots
#' plot(k)
#' plot(k, beside = TRUE)
#' plot(k, proportional = TRUE)
#'
#' # mosaic plots
#' mosaicplot(k)
#' @export
#'
ds_cross_table <- function(var1, var2) UseMethod("ds_cross_table")

#' @export
ds_cross_table <- function(var1, var2) {

    var_1 <- l(deparse(substitute(var1)))
    var_2 <- l(deparse(substitute(var2)))
    var_names <- c(var_1, var_2)
    x <- as.matrix(table(var1, var2))
    rownames(x) <- NULL
    n <- length(var1)
    if (is.factor(var1)) {
        row_name <- levels(var1)
    } else {
        row_name <- unique(sort(var1))
    }
    per_mat <- round(x/n, 3)
    row_pct <- apply(per_mat, 1, sum)
    col_pct <- apply(per_mat, 2, sum)
    per_mat <- cbind(per_mat, row_pct)
    per_mat <- suppressWarnings(rbind(per_mat, col_pct))
    d <- dim(per_mat)
    per_mat[d[1], d[2]] <- 1
    rowtotal <- apply(x, 1, sum)
    coltotal <- apply(x, 2, sum)
    rcent <- row_pct(x, rowtotal)
    rcent <- cbind(rcent, row_pct)
    rcent <- apply(rcent, c(1, 2), rounda)
    ccent <- col_pct(x, coltotal)
    ccent <- apply(ccent, c(1, 2), rounda)
    x <- cbind(x, rowtotal)
    x <- cbind(unname(row_name), x)
    if (is.factor(var2)) {
        col_name <- levels(var2)
    } else {
        col_name <- unique(sort(var2))
    }

    result <- list(obs = n, var2_levels = col_name, var1_levels = row_name, varnames = var_names,
        twowaytable = x, percent_table = per_mat, row_percent = rcent, column_percent = ccent,
        column_totals = coltotal, percent_column = col_pct)


    class(result) <- "ds_cross_table"
    return(result)
}

#' @export
#' @rdname ds_cross_table
#' @usage NULL
#'
cross_table <- function(var1, var2) {

  .Deprecated("ds_cross_table()")
  ds_cross_table(var1, var2)

}

#' @export
print.ds_cross_table <- function(x, ...) {
    print_cross(x)
}



#' @export
#' @rdname ds_cross_table
#'
plot.ds_cross_table <- function(x, beside = FALSE, proportional = FALSE, ...) {
    i_data <- x$twowaytable
    nb <- ncol(i_data)
    bdata <- i_data[, c(-1, -nb)]
    ln <- length(x$var2_levels)
    bardata <- matrix(as.numeric(bdata), ncol = ln)
    cols <- nrow(bardata)


    # proportional stacked bar plots
    if (proportional == TRUE) {

        colbar <- colSums(bardata)
        nh <- nrow(bardata)
        h <- rep(colbar, nh)
        hichka <- matrix(h, nrow = nh, byrow = T)
        propo_data <- round((bardata/hichka) * 100, 2)
        barplot(propo_data, col = rainbow(cols), main = paste(x$varnames[1],
            "by", x$varnames[2]), xlab = x$varnames[2], ylab = x$varnames[1],
            legend.text = T)
        result <- list(data = propo_data)

    } else {

        barplot(bardata, col = rainbow(cols), beside = beside,
                main = paste(x$varnames[1], "by", x$varnames[2]),
                xlab = x$varnames[2], ylab = x$varnames[1],
                legend.text = T)
        result <- list(data = bardata)
    }

    invisible(result)
}


#' @export
#' @rdname ds_cross_table
#'
mosaicplot.ds_cross_table <- function(x, ...) {
    i_data <- x$twowaytable
    nb <- ncol(i_data)
    mdata <- i_data[, c(-1, -nb)]
    ln <- length(x$var2_levels)
    modata <- matrix(as.numeric(mdata), ncol = ln)
    colnames(modata) <- x$var2_levels
    rownames(modata) <- x$var1_levels
    cols <- nrow(modata)
    mosaicplot(modata, col = rainbow(cols), off = 5,
               xlab = x$varnames[1], ylab = x$varnames[2],
               main = paste(x$varnames[1], "by", x$varnames[2]))

    # x axis position
    cpr <- ncol(x$row_percent)
    rp <- x$row_percent[, cpr]
    crp <- cumsum(rp)
    lcrp <- length(crp)
    f1 <- (crp[-lcrp] + diff(cumsum(rp)) / 2)
    xpos <- c(rp[1] / 2, f1)

    # y axis position
    nr <- nrow(x$row_percent)
    for (i in seq_len(nr)) {
        rpy <- rev(x$row_percent[i, -cpr])
        crpy <- cumsum(rpy)
        lcrpy <- length(crpy)
        f1 <- (crpy[-lcrpy] + diff(cumsum(rpy)) / 2)
        ypos <- c(rpy[1] / 2, f1)
        text(x = xpos[i], y = ypos, labels = paste(rpy * 100, '%'), cex = 0.6)
    }

    result <- list(data = modata)
    invisible(result)
}
