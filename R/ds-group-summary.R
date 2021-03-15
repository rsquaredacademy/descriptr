#' Groupwise descriptive statistics
#'
#' Descriptive statistics of a continuous variable for the different levels of
#' a categorical variable. \code{boxplot.group_summary()} creates boxplots of
#' the continuous variable for the different levels of the categorical variable.
#'
#' @param data A \code{data.frame} or a \code{tibble}.
#' @param gvar Column in \code{data}.
#' @param cvar Column in \code{data}.
#' @param x An object of the class \code{ds_group_summary}.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#' @param ... Further arguments to be passed to or from methods.
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
ds_group_summary <- function(data, gvar, cvar) UseMethod("ds_group_summary")

#' @export
#'
ds_group_summary.default <- function(data, gvar, cvar) {

  check_df(data)
  gvar_name <- deparse(substitute(gvar))
  cvar_name <- deparse(substitute(cvar))

  g_var <- rlang::enquo(gvar)
  c_var <- rlang::enquo(cvar)

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


  split_dat <- tapply(cvar, list(gvar), function(gvar) {
    c(
      length(gvar), min(gvar), max(gvar), mean(gvar),
      median(gvar), ds_mode(gvar), sd(gvar), var(gvar),
      ds_skewness(gvar), ds_kurtosis(gvar), stat_uss(gvar),
      ds_css(gvar), ds_cvar(gvar), ds_std_error(gvar),
      ds_range(gvar), IQR(gvar)
    )
  })

  splito <- sapply(split_dat, round, 2)

  rnames <- c(
    "Obs", "Minimum", "Maximum", "Mean", "Median", "Mode",
    "Std. Deviation", "Variance", "Skewness", "Kurtosis",
    "Uncorrected SS", "Corrected SS", "Coeff Variation",
    "Std. Error Mean", "Range", "Interquartile Range"
  )

  out              <- data.frame(rnames, splito)
  names(out)       <- c("Statistic/Levels", levels(gvar))
  plot_data        <- data.frame(gvar, cvar)
  names(plot_data) <- c(xname, yname)

  tidystats <-
    data %>%
    dplyr::select(!! g_var, !! c_var) %>%
    na.omit() %>%
    dplyr::group_by(!! g_var) %>%
    dplyr::summarise(length = length(!! c_var), min = min(!! c_var),
              max = max(!! c_var), mean  = mean(!! c_var),
              median= median(!! c_var), mode = ds_mode(!! c_var),
              sd = sd(!! c_var), variance = var(!! c_var),
              skewness = ds_skewness(!! c_var), kurtosis = ds_kurtosis(!! c_var),
              coeff_var = ds_cvar(!! c_var), std_error = ds_std_error(!! c_var),
              range = ds_range(!! c_var), iqr = IQR(!! c_var))

  result <- list(stats      = out,
                 tidy_stats = tidystats,
                 plotdata   = plot_data,
                 xvar       = xname,
                 yvar       = yname,
                 data       = data
  )

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

  x_lab <- use_series(x, xvar)
  y_lab <- use_series(x, yvar)

  k <-
    x %>%
    use_series(xvar) %>%
    rlang::sym()

  j <-
    x %>%
    use_series(yvar) %>%
    rlang::sym()

  p <-
    x %>%
    use_series(data) %>%
    dplyr::select(x = !! k, y = !! j) %>%
    ggplot() +
    geom_boxplot(aes(x = x, y = y), fill = "blue") +
    xlab(x_lab) + ylab(y_lab) +
    ggtitle(paste(y_lab, "by", x_lab))

  if (print_plot) {
    print(p)
  } else {
    return(p)
  }

}
