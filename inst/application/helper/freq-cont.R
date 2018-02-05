source('helper/utils.R')
source('helper/output.R')

ds_freq_cont <- function(data, variable, bins = 5) UseMethod("ds_freq_cont")

ds_freq_cont.default <- function(data, variable,  bins = 5) {

  fdata <-
    data %>%
    pull(!! sym(variable)) %>%
    na.omit

  if(!is.numeric(fdata)) {
    stop('variable must be numeric')
  }

  if(!is.numeric(bins)) {
    stop('bins must be integer value')
  }

  if(is.numeric(bins)) {
    bins <- as.integer(bins)
  }

  var_name <-
    data %>%
    select(!! sym(variable)) %>%
    names

  n_bins <- bins
  inta <- intervals(fdata, bins)
  result <- freq(fdata, bins, inta)
  data_len <- length(fdata)
  cum <- cumsum(result)
  per <- percent(result, data_len)
  cum_per <- percent(cum, data_len)
  out <- list(breaks = inta,
              frequency = result,
              cumulative = cum,
              percent = per,
              cum_percent = cum_per,
              bins = n_bins,
              data = fdata,
              varname = var_name)

  class(out) <- "ds_freq_cont"
  return(out)
}

print.ds_freq_cont <- function(x, ...) {
  print_fcont(x)
}

plot.ds_freq_cont <- function(x, ...) {

  x_lab <-
    x %>%
    use_series(varname)

  k <-
    x %>%
    use_series(varname) %>%
    extract(1) %>%
    sym

  bins <-
    x %>%
    use_series(frequency) %>%
    length

  p <-
    x %>%
    use_series(frequency) %>%
    as_tibble %>%
    add_column(x = seq_len(bins), .before = 1) %>%
    ggplot() +
    geom_col(aes(x = x, y = value), width = 0.999,
             fill = "blue", color = "black") +
    xlab(x_lab) + ylab("Count") +
    ggtitle(paste("Histogram of", x_lab))

  print(p)

  result <- list(plot = p)
  invisible(result)

}
