#' @importFrom stats median sd var IQR
#' @importFrom graphics boxplot
#' @title Descriptive Statistics By Group
#' @description \code{group_summary} returns descriptive statistics of a
#' continuous variable for the different levels of a categorical variable.
#' \code{boxplot.group_summary} creates boxplots of the continuous variable
#' for the different levels of the categorical variable.
#' @param fvar a factor variable
#' @param cvar a continuous variable
#' @param x an object of the class \code{group_summary}
#' @param ... further arguments to be passed to or from methods
#' @return \code{group_summary} returns an object of class \code{"group_summary"}.
#' An object of class \code{"group_summary"} is a list containing the
#' following components:
#'
#' \item{stats}{a data frame containing descriptive statistics for the different
#' levels of the factor variable}
#' \item{plotdata}{data for boxplot method}
#' \item{xvar}{name of the categorical variable}
#' \item{yvar}{name of the continuous variable}
#' @examples
#' # group summary
#' mt <- mtcars
#' mt$cyl <- as.factor(mt$cyl)
#' group_summary(mt$cyl, mt$mpg)
#'
#' # boxplot
#' k <- group_summary(mt$cyl, mt$mpg)
#' boxplot(k)
#' @seealso \code{link{summary_stats}}
#' @export
#'
group_summary <- function(fvar, cvar) UseMethod('group_summary')




#' @export
#'
group_summary.default <- function(x, y) {

    if (!is.factor(fvar)) {
        stop('fvar must be an object of type factor')
    }

    if (!is.numeric(cvar)) {
        stop('cvar must be numeric')
    }

    if (length(fvar) != length(cvar)) {
        stop('fvar and cvar must be of the same length')
    }

    xname <- l(deparse(substitute(fvar)))
    yname <- l(deparse(substitute(cvar)))


    split_dat <- tapply(cvar, list(fvar), function(fvar) {
                      c(length(fvar), min(x), max(fvar), mean(fvar),
                      median(fvar), stat_mode(fvar), sd(fvar), var(fvar),
                      skewness(fvar), kurtosis(fvar), stat_uss(fvar),
                      stat_css(fvar), stat_cvar(fvar), std_error(fvar),
                      stat_range(fvar), IQR(fvar))
                 })

    splito <- sapply(split_dat, round, 2)

    rnames <- c('Obs', 'Maximum', 'Minimum', 'Mean', 'Median', 'Mode',
                'Std. Deviation', 'Variance', 'Skewness', 'Kurtosis',
                'Uncorrected SS', 'Corrected SS', 'Coeff Variation',
                'Std. Error Mean', 'Range', 'Interquartile Range')

    out <- data.frame(rnames, splito)
    names(out) <- c('Statistic/Levels', levels(fvar))

    plot_data <- data.frame(fvar, cvar)
    names(plot_data) <- c(xname, yname)

    result <- list(stats  = out,
                   plotdata = plot_data,
                   xvar  = xname,
                   yvar  = yname)

    class(result) <- 'group_summary'
    return(result)
}


#' @export
print.group_summary <- function(x, ...) {
    print_group(x)
}


#' @rdname group_summary
#' @export
#'
boxplot.group_summary <- function(x, ...) {
    n <- nlevels(factor(x$plotdata[[1]]))
    boxplot(x$plotdata[[2]] ~ x$plotdata[[1]],
        col = rainbow(n), xlab = x$xvar,
        ylab = x$yvar,
        main = paste('Box Plot of', x$yvar, 'by', x$xvar))
}
