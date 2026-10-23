#' Groupwise descriptive statistics
#'
#' Descriptive statistics of a continuous variable for the different levels of
#' a categorical variable. \code{boxplot.group_summary()} creates boxplots of
#' the continuous variable for the different levels of the categorical variable.
#'
#' @param data A \code{data.frame} or a \code{tibble}.
#' @param group_by Column in \code{data}.
#' @param cols Column in \code{data}.
#' @param x An object of the class \code{ds_group_summary}.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#' @param ... Further arguments to be passed to or from methods.
#'
#' @details Groups with missing values in the continuous variable are summarised
#' using the non-missing observations (\code{na.rm = TRUE}), and the number of
#' missing observations is reported in the \code{Missing} row of \code{stats}.
#' Unused levels of the grouping factor are dropped.
#'
#' @return \code{ds_group_summary()} returns an object of class \code{"ds_group_summary"}.
#' An object of class \code{"ds_group_summary"} is a list containing the
#' following components:
#'
#' \item{stats}{A data frame containing descriptive statistics for the different
#' levels of the factor variable.}
#' \item{tidy_stats}{A tibble containing descriptive statistics for the different
#' levels of the factor variable.}
#' \item{plotdata}{Data for boxplot method.}
#'
#' @examples
#' # ds_group summary
#' ds_group_summary(mtcarz, cyl, mpg)
#'
#' # boxplot
#' k <- ds_group_summary(mtcarz, cyl, mpg)
#' plot(k)
#'
#' # tibble
#' k$tidy_stats
#'
#' @seealso \code{\link{ds_summary_stats}}
#'
#' @export
#'
ds_group_summary <- function(data, group_by, cols) UseMethod("ds_group_summary")

#' @export
#'
ds_group_summary.default <- function(data, group_by, cols) {

  check_df(data)
  gvar_name <- deparse(substitute(group_by))
  cvar_name <- deparse(substitute(cols))

  g_var <- rlang::enquo(group_by)
  c_var <- rlang::enquo(cols)

  check_numeric(data, !! c_var, cvar_name)
  check_factor(data, !! g_var, gvar_name)

  gvar  <- dplyr::pull(data, !! g_var)
  cvar  <- dplyr::pull(data, !! c_var)

  xname <-
    data %>%
    dplyr::select(!! g_var) %>%
    names()

  yname <-
    data %>%
    dplyr::select(!! c_var) %>%
    names()


  # drop unused levels so that empty groups do not produce NULL statistics
  gvar <- droplevels(gvar)

  split_dat <- lapply(split(cvar, gvar, drop = TRUE), function(x) {
    n       <- sum(!is.na(x))
    missing <- sum(is.na(x))
    x       <- x[!is.na(x)]

    if (n == 0) {
      stats <- rep(NA_real_, 15)
    } else {
      stats <- c(
        min(x), max(x), mean(x), median(x), ds_mode(x), sd(x), var(x),
        ds_skewness(x), ds_kurtosis(x), stat_uss(x), ds_css(x), ds_cvar(x),
        ds_std_error(x), ds_range(x), IQR(x)
      )
    }

    c(Obs = n, Missing = missing, stats)
  })

  splito <- round(do.call(cbind, split_dat), 2)

  rnames <- c(
    "Obs", "Missing", "Minimum", "Maximum", "Mean", "Median", "Mode",
    "Std. Deviation", "Variance", "Skewness", "Kurtosis",
    "Uncorrected SS", "Corrected SS", "Coeff Variation",
    "Std. Error Mean", "Range", "Interquartile Range"
  )

  out              <- data.frame(rnames, splito, check.names = FALSE)
  names(out)       <- c("Statistic/Levels", names(split_dat))
  row.names(out)   <- NULL
  plot_data        <- data.frame(gvar, cvar)
  names(plot_data) <- c(xname, yname)

  # return NA instead of Inf/NaN/errors for a group whose values are all missing
  stat_if <- function(x, fun) {
    if (all(is.na(x))) NA_real_ else fun(x)
  }

  tidystats <-
    data %>%
    dplyr::select(!! g_var, !! c_var) %>%
    dplyr::filter(!is.na(!! g_var)) %>%
    dplyr::group_by(!! g_var) %>%
    dplyr::summarise(length = sum(!is.na(!! c_var)),
              missing = sum(is.na(!! c_var)),
              min = stat_if(!! c_var, function(x) min(x, na.rm = TRUE)),
              max = stat_if(!! c_var, function(x) max(x, na.rm = TRUE)),
              mean = stat_if(!! c_var, function(x) mean(x, na.rm = TRUE)),
              median = stat_if(!! c_var, function(x) median(x, na.rm = TRUE)),
              mode = stat_if(!! c_var, ds_mode),
              sd = stat_if(!! c_var, function(x) sd(x, na.rm = TRUE)),
              variance = stat_if(!! c_var, function(x) var(x, na.rm = TRUE)),
              skewness = stat_if(!! c_var, ds_skewness),
              kurtosis = stat_if(!! c_var, ds_kurtosis),
              coeff_var = stat_if(!! c_var, ds_cvar),
              std_error = stat_if(!! c_var, function(x) {
                sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
              }),
              range = stat_if(!! c_var, ds_range),
              iqr = stat_if(!! c_var, function(x) IQR(x, na.rm = TRUE)))

  result <- list(stats      = out,
                 tidy_stats = tidystats,
                 plot_data  = plot_data)

  class(result) <- "ds_group_summary"
  return(result)
}

#' @export
print.ds_group_summary <- function(x, ...) {
  print_group(x)
}

#' @rdname ds_group_summary
#' @export
#'
plot.ds_group_summary <- function(x, print_plot = TRUE, ...) {

  xy    <- names(x$plot_data)
  x_lab <- xy[1]
  y_lab <- xy[2]

  p <-
    x %>%
    use_series(plot_data) %>%
    ggplot() +
    geom_boxplot(aes(x = .data[[x_lab]], y = .data[[y_lab]]), fill = "blue") +
    xlab(x_lab) + ylab(y_lab) +
    ggtitle(paste(y_lab, "by", x_lab))

  if (print_plot) {
    print(p)
  } else {
    return(p)
  }

}
