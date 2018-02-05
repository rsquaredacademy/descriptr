source('helper/utils.R')
source('helper/output.R')
source('helper/describe.R')

ds_group_summary <- function(data, gvar, cvar) UseMethod('ds_group_summary')

ds_group_summary.default <- function(data, gvar, cvar) {

  xname <-
    data %>%
    select(!! sym(gvar)) %>%
    names

  yname <-
    data %>%
    select(!! sym(cvar)) %>%
    names

  gvar <-
    data %>%
    pull(!! sym(gvar))

  cvar <-
    data %>%
    pull(!! sym(cvar))

  if (!is.factor(gvar)) {
    stop('gvar must be an object of type factor')
  }

  if (!is.numeric(cvar)) {
    stop('cvar must be numeric')
  }

  if (length(gvar) != length(cvar)) {
    stop('gvar and cvar must be of the same length')
  }

  split_dat <- tapply(cvar, list(gvar), function(gvar) {
    c(length(gvar), min(gvar), max(gvar), mean(gvar),
      median(gvar), ds_mode(gvar), sd(gvar), var(gvar),
      ds_skewness(gvar), ds_kurtosis(gvar), stat_uss(gvar),
      ds_css(gvar), ds_cvar(gvar), std_error(gvar),
      ds_range(gvar), IQR(gvar))
  })

  splito <- sapply(split_dat, round, 2)

  rnames <- c('Obs', 'Minimum', 'Maximum', 'Mean', 'Median', 'Mode',
              'Std. Deviation', 'Variance', 'Skewness', 'Kurtosis',
              'Uncorrected SS', 'Corrected SS', 'Coeff Variation',
              'Std. Error Mean', 'Range', 'Interquartile Range')

  out <- data.frame(rnames, splito)
  names(out) <- c('Statistic/Levels', levels(gvar))

  plot_data <- data.frame(gvar, cvar)
  names(plot_data) <- c(xname, yname)

  result <- list(stats  = out,
                 plotdata = plot_data,
                 xvar  = xname,
                 yvar  = yname,
                 data = data)

  class(result) <- 'ds_group_summary'
  return(result)
}

print.ds_group_summary <- function(x, ...) {
  print_group(x)
}

plot.ds_group_summary <- function(x, ...) {

  x_lab <-
    x %>%
    use_series(xvar)

  y_lab <-
    x %>%
    use_series(yvar)

  k <-
    x %>%
    use_series(xvar) %>%
    sym

  j <-
    x %>%
    use_series(yvar) %>%
    sym

  p <-
    x %>%
    use_series(data) %>%
    select(x = !!k, y = !!j) %>%
    ggplot() +
    geom_boxplot(aes(x = x, y = y), fill = "blue") +
    xlab(x_lab) + ylab(y_lab) +
    ggtitle(paste(y_lab , "by", x_lab))

  print(p)

  result <- list(plot = p)
  invisible(result)


}
