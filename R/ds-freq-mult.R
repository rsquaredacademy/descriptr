freq_table2 <- function(data, name) UseMethod("freq_table2")

freq_table2.default <- function(data, name) {

  var_name <- name

  dat <-
    data %>%
    dplyr::select(name) %>%
    dplyr::pull(1) %>%
    na.omit()

  if (!is.factor(dat)) {
    stop("Data must be categorical/qualitative.", call. = FALSE)
  }

  cq          <- unique(sort(dat))
  result      <- as.vector(table(dat))
  level_names <- levels(dat)
  data_len    <- length(dat)
  len         <- length(result)
  cum         <- cumsum(result)
  per         <- percent(result, data_len)
  cum_per     <- percent(cum, data_len)

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

  n_obs <-
    data %>%
    dplyr::pull(name) %>%
    length()

  if (na_count > 0) {
    na_data <- dplyr::pull(data, var_name)

    var_count <-
      na_data %>%
      table() %>%
      as.vector() %>%
      sum()

    na_freq <- n_obs - var_count
  } else {
    na_freq <- 0
  }

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
