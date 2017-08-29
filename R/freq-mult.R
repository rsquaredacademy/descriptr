#' @importFrom forcats fct_unique fct_count
freq_table2 <- function(data, name) UseMethod("freq_table2")

freq_table2.default <- function(data, name) {

  var_name <- name

  # exclude missing data
  dat <- data %>%
    select(name) %>%
    pull(1) %>%
    na.omit()

  if (!is.factor(dat)) {
      stop('data must be categorical/qualitative')
  }

  # levels
  level_names <- levels(dat)

  # length of input
  data_len <- length(dat)

  # unique values in the input
  cq <- forcats::fct_unique(dat)

  # count of unique values in the input
  result <- dat %>%
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
    varname = var_name
  )


  class(result) <- "freq_table2"
  return(result)

}


print.freq_table2 <- function(x, ...) {
  print_ftable2(x)
}
