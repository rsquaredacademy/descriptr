#' Groupwise descriptive statistics
#'
#' Descriptive statistics of a continuous variable for the different levels of
#' a categorical variable. \code{plot.ds_group_summary()} creates boxplots of
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
#' Observations with a missing grouping label are excluded from the summary and
#' from the boxplot. Unused levels of the grouping factor are dropped.
#'
#' The grouping key in \code{tidy_stats} keeps the class of the grouping
#' variable (including ordered factors) and the order of the observed levels.
#' If the grouping column is named like one of the statistic columns, such as
#' \code{length}, its column in \code{tidy_stats} is suffixed with
#' \code{_group} so that every column name is unique.
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
  data <- as_categorical(data)
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

  # summarise the observed groups only
  keep <- !is.na(gvar)
  gvar <- gvar[keep]
  cvar <- cvar[keep]

  if (length(gvar) == 0) {
    stop("No data to summarise. The data has no observations with a non-missing grouping value.", call. = FALSE)
  }

  # plot and summary must use the same observations: a missing grouping label
  # is not a group
  plot_data        <- data.frame(gvar, cvar)
  names(plot_data) <- c(xname, yname)

  split_dat <- split(cvar, gvar, drop = TRUE)

  stat_names <- c(
    "obs", "missing", "min", "max", "mean", "median", "mode", "sd",
    "variance", "skewness", "kurtosis", "uss", "css", "coeff_var",
    "std_error", "range", "iqr"
  )

  # each group is summarised from the original vector, in a helper, so that a
  # variable named like one of the statistics (missing, min, mean, ...) cannot
  # overwrite the values used by the summaries that follow
  group_stats <- function(x) {

    n       <- sum(!is.na(x))
    missing <- sum(is.na(x))
    x       <- x[!is.na(x)]

    if (n == 0) {
      values    <- rep(NA_real_, length(stat_names))
      values[1] <- n
      values[2] <- missing
      return(stats::setNames(values, stat_names))
    }

    stats::setNames(
      c(
        n, missing,
        min(x), max(x), mean(x), median(x), ds_mode(x), sd(x), var(x),
        ds_skewness(x), ds_kurtosis(x), stat_uss(x), ds_css(x), ds_cvar(x),
        ds_std_error(x), ds_range(x), IQR(x)
      ),
      stat_names
    )
  }

  stat_mat <- vapply(split_dat, group_stats, numeric(length(stat_names)))
  rownames(stat_mat) <- stat_names

  rnames <- c(
    "Obs", "Missing", "Minimum", "Maximum", "Mean", "Median", "Mode",
    "Std. Deviation", "Variance", "Skewness", "Kurtosis",
    "Uncorrected SS", "Corrected SS", "Coeff Variation",
    "Std. Error Mean", "Range", "Interquartile Range"
  )

  out              <- data.frame(rnames, round(stat_mat, 2), check.names = FALSE)
  names(out)       <- c("Statistic/Levels", names(split_dat))
  row.names(out)   <- NULL

  tidy_names <- c(
    "length", "missing", "min", "max", "mean", "median", "mode", "sd",
    "variance", "skewness", "kurtosis", "coeff_var", "std_error", "range",
    "iqr"
  )

  tidy_cols <- stats::setNames(
    lapply(c(
      "obs", "missing", "min", "max", "mean", "median", "mode", "sd",
      "variance", "skewness", "kurtosis", "coeff_var", "std_error", "range",
      "iqr"
    ), function(nm) as.numeric(stat_mat[nm, ])),
    tidy_names
  )

  tidy_list <- c(
    list(group_key_vector(gvar, names(split_dat))),
    tidy_cols
  )
  names(tidy_list) <- c(unique_group_name(xname, tidy_names), tidy_names)

  tidystats <- dplyr::as_tibble(
    data.frame(tidy_list, check.names = FALSE),
    .name_repair = "minimal"
  )

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
