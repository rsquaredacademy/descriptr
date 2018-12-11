freq_table2 <- function(data, name) UseMethod("freq_table2")

freq_table2.default <- function(data, name) {

  var_name <- name

  dat <-
    data %>%
    dplyr::select(name) %>%
    dplyr::pull(1) %>%
    stats::na.omit()

  if (!is.factor(dat)) {
    stop("data must be categorical/qualitative")
  }

  level_names <- levels(dat)
  data_len    <- length(dat)
  cq          <- forcats::fct_unique(dat)

  result <-
    dat %>%
    forcats::fct_count() %>%
    dplyr::pull(2)

  len     <- length(result)
  cum     <- cumsum(result)
  per     <- percent(result, data_len)
  cum_per <- percent(cum, data_len)

  ftable <- tibble::tibble(
    Levels          = level_names,
    Frequency       = result,
    `Cum Frequency` = cum,
    Percent         = per,
    `Cum Percent`   = cum_per
  )

  na_count <-
    data %>%
    dplyr::pull(name) %>%
    is.na() %>%
    sum()

  if (na_count > 0) {
    na_freq <-
      data %>%
      dplyr::pull(name) %>%
      forcats::fct_count() %>%
      dplyr::pull(n) %>%
      dplyr::last()
  } else {
    na_freq <- 0
  }

  n_obs <-
    data %>%
    dplyr::pull(name) %>%
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
