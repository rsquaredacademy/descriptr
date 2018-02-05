source('helper/utils.R')
source('helper/output.R')

ds_freq_table <- function(data, variable) UseMethod("ds_freq_table")

ds_freq_table.default <- function(data, variable) {

  fdata <-
    data %>%
    pull(!! sym(variable)) %>%
    na.omit

  if (!is.factor(fdata)) {
    stop('variable must be categorical/qualitative')
  }

  var_name <-
    data %>%
    select(!! sym(variable)) %>%
    names

  level_names <- levels(fdata)
  data_len <- length(fdata)

  # unique values in the input
  cq <- forcats::fct_unique(fdata)

  # count of unique values in the input
  result <- fdata %>%
    fct_count %>%
    pull(2)

  # length of result
  len <- length(result)

  # cumulative frequency
  cum <- cumsum(result)

  # percent
  per <- percent(result, data_len)

  # cumulative percent
  cum_per <- percent(cum, data_len)

  # matrix
  ftable <- tibble(Levels = level_names,
                   Frequency = result,
                   `Cum Frequency` = cum,
                   Percent = per,
                   `Cum Percent` = cum_per)


  result <- list(
    ftable = ftable,
    varname = var_name,
    data = data
  )

  class(result) <- "ds_freq_table"
  return(result)

}

print.ds_freq_table <- function(x, ...) {
  print_ftable(x)
}

plot.ds_freq_table <- function(x, ...) {

  x_lab <-
    x %>%
    use_series(varname) %>%
    extract(1)

  k <-
    x %>%
    use_series(varname) %>%
    extract(1) %>%
    sym

  p <-
    x %>%
    use_series(data) %>%
    select(x = !!k) %>%
    ggplot() +
    geom_bar(aes(x = x), fill = "blue") +
    xlab(x_lab) + ylab("Count") +
    ggtitle(paste("Bar plot of", x_lab))

  print(p)

  result <- list(plot = p)
  invisible(result)
}
