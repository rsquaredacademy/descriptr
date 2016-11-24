#' @title Descriptive Statistics By Group
#' @description \code{group_summary} returns descriptive statistics of a
#' continuous variable for the different levels of a categorical variable.
#' @param x a factor variable
#' @param y a continuous variable
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
#' mt <- mtcars
#' mt$cyl <- as.factor(mt$cyl)
#' group_summary(mt$cyl, mt$mpg)
#' @seealso \code{link{summary_stats}}
#' @export
#'
group_summary <- function(x, y) UseMethod('group_summary')


#' @rdname group_summary
#' @export
#'
group_summary.default <- function(x, y) {

    if (!is.factor(x)) {
        stop('x must be an object of type factor')
    }

    if (!is.numeric(y)) {
        stop('y must be numeric')
    }

    if (length(x) != length(y)) {
        stop('x and y must be of the same length')
    }

    xname <- l(deparse(substitute(x)))
    yname <- l(deparse(substitute(y)))


    split_dat <- tapply(y, list(x), function(x) {
                      c(length(x), min(x), max(x), mean(x), median(x),
                      stat_mode(x), sd(x), var(x), skewness(x),
                      kurtosis(x), stat_uss(x),
                      stat_css(x), stat_cvar(x),
                      std_error(x), stat_range(x), IQR(x))
                 })

    splito <- sapply(split_dat, round, 2)

    rnames <- c('Obs', 'Maximum', 'Minimum', 'Mean', 'Median', 'Mode',
                'Std. Deviation', 'Variance', 'Skewness', 'Kurtosis',
                'Uncorrected SS', 'Corrected SS', 'Coeff Variation',
                'Std. Error Mean', 'Range', 'Interquartile Range')

    out <- data.frame(rnames, splito)
    names(out) <- c('Statistic/Levels', levels(x))

    plot_data <- data.frame(x, y)
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


#' @importFrom graphics boxplot
#' @importFrom grDevices rainbow
#' @title Group Summary Box Plot
#' @description \code{boxplot.group_summary} creates boxplots of the continuous
#' variable for the different levels of the categorical variable.
#' @param x an object of the class \code{group_summary}
#' @examples
#' mt <- mtcars
#' mt$cyl <- as.factor(mt$cyl)
#' k <- group_summary(mt$cyl, mt$mpg)
#' boxplot(k)
#' @export
#'
boxplot.group_summary <- function(x, ...) {
    n <- nlevels(factor(x$plotdata[[1]]))
    boxplot(x$plotdata[[2]] ~ x$plotdata[[1]],
        col = rainbow(n), xlab = x$xvar,
        ylab = x$yvar,
        main = paste('Box Plot of', x$yvar, 'by', x$xvar))
}
