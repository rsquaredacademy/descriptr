source('helper/describe.R')
source('helper/utils.R')
source('helper/output.R')

ds_summary_stats <- function(data, variable) UseMethod('ds_summary_stats')

ds_summary_stats.default <- function(data, variable) {

  sdata <-
    data %>%
    pull(!! sym(variable)) %>%
    na.omit

  if(!is.numeric(sdata)) {
    stop('data must be numeric')
  }

  low <- ds_tailobs(sdata, 5, 'low')
  high <- ds_tailobs(sdata, 5, 'high')
  low_val <- ds_rindex(sdata, low)
  high_val <- ds_rindex(sdata, high)

  result <- list(
    obs = length(sdata),
    missing = sum(is.na(sdata)),
    avg = mean(sdata),
    tavg = mean(sdata, trim = 0.05),
    stdev = sd(sdata),
    variance = var(sdata),
    skew = ds_skewness(sdata),
    kurtosis = ds_kurtosis(sdata),
    uss = stat_uss(sdata),
    css = ds_css(sdata),
    cvar = ds_cvar(sdata),
    sem = std_error(sdata),
    median = median(sdata),
    mode = ds_mode(sdata),
    range = ds_range(sdata),
    min = min(sdata), Max = max(sdata),
    iqrange = IQR(sdata),
    per99 = quantile(sdata, 0.99),
    per90 = quantile(sdata, 0.95),
    per95 = quantile(sdata, 0.90),
    per75 = quantile(sdata, 0.75),
    per25 = quantile(sdata, 0.25),
    per10 = quantile(sdata, 0.10),
    per5 = quantile(sdata, 0.05),
    per1 = quantile(sdata, 0.01),
    lowobs = low,
    highobs = high,
    lowobsi = low_val,
    highobsi = high_val
  )

  class(result) <- 'ds_summary_stats'
  return(result)
}

print.ds_summary_stats <- function(x, ...) {
  print_stats(x)
}
