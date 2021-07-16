ds_freq_numeric <- function(data, variable, bins = 5) {

  check_df(data)
  var_name <- deparse(substitute(variable))
  varyable <- rlang::enquo(variable)
  check_numeric(data, !! varyable, var_name)

  fdata <-
    data %>%
    dplyr::pull(!! varyable) %>%
    na.omit()

  if (!is.numeric(bins)) {
    stop("bins must be integer value", call. = FALSE)
  }

  if (is.numeric(bins)) {
    bins <- as.integer(bins)
  }

  var_name <-
    data %>%
    dplyr::select(!! varyable) %>%
    names()

  n_bins   <- bins
  inta     <- intervals(fdata, bins)
  result   <- freq(fdata, bins, inta)
  data_len <- length(fdata)
  cum      <- cumsum(result)
  per      <- percent(result, data_len)
  cum_per  <- percent(cum, data_len)

  na_count <-
    data %>%
    dplyr::pull(!! varyable) %>%
    is.na() %>%
    sum()

  if (na_count > 0) {
    na_freq <- na_count
  } else {
    na_freq <- 0
  }

  n_obs <-
    data %>%
    dplyr::pull(!! varyable) %>%
    length()

  lower_n <- n_bins + 1

  freq_data <-
    data.frame(lower        = inta[-lower_n],
                   upper        = inta[-1],
                   frequency    = result,
                   cumulative   = cum,
                   freq_percent = per,
                   cum_percent  = cum_per)

  utility <- list(breaks      = inta,
                  frequency   = result,
                  cumulative  = cum,
                  percent     = per,
                  cum_percent = cum_per,
                  bins        = n_bins,
                  data        = fdata,
                  na_count    = na_freq,
                  n           = n_obs,
                  varname     = var_name)

  out <- list(ftable  = freq_data,
              utility = utility)

  return(out)
}

plot_ds_freq_numeric <- function(x, ...) {

  x_lab <-
    x %>%
    use_series(utility) %>%
    use_series(varname)

  k <-
    x %>%
    use_series(utility) %>%
    use_series(varname) %>%
    extract(1) %>%
    rlang::sym()

  bins <-
    x %>%
    use_series(utility) %>%
    use_series(frequency) %>%
    length()

  p <-
    data.frame(x = seq_len(x$utility$bins), value = x$utility$frequency) %>%
    ggplot() +
    geom_col(
      aes(x = x, y = value), width = 0.999,
      fill = "blue", color = "black"
    ) +
    xlab(x_lab) + ylab("Count") +
    ggtitle(paste("Histogram of", x_lab))

  return(p)

}
