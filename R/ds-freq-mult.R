#' @importFrom forcats fct_unique fct_count
freq_table2 <- function(data, name) UseMethod("freq_table2")

freq_table2.default <- function(data, name) {

  var_name <- name

  dat <-
    data %>%
    select(name) %>%
    pull(1) %>%
    na.omit()

  if (!is.factor(dat)) {
    stop("data must be categorical/qualitative")
  }

  level_names <- levels(dat)
  data_len    <- length(dat)
  cq          <- forcats::fct_unique(dat)

  result <-
    dat %>%
    fct_count() %>%
    pull(2)

  len     <- length(result)
  cum     <- cumsum(result)
  per     <- percent(result, data_len)
  cum_per <- percent(cum, data_len)

  ftable <- tibble(
    Levels          = level_names,
    Frequency       = result,
    `Cum Frequency` = cum,
    Percent         = per,
    `Cum Percent`   = cum_per
  )

  na_count <-
    data %>%
    pull(name) %>%
    is.na() %>%
    sum()

  if (na_count > 0) {
    na_freq <-
      data %>%
      pull(name) %>%
      fct_count() %>%
      pull(n) %>%
      last()
  } else {
    na_freq <- 0
  }

  n_obs <-
    data %>%
    pull(name) %>%
    length()

  result <- list(
    ftable   = ftable,
    varname  = var_name,
    na_count = na_freq,
    n        = n_obs
  )

  class(result) <- "freq_table2"
  return(result)
}


print.freq_table2 <- function(x, ...) {
  print_ftable2(x)
}
