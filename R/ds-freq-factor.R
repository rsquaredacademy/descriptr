ds_freq_factor <- function(data, variable) {

  check_df(data)
  var_name <- deparse(substitute(variable))
  varyable <- rlang::enquo(variable)
  check_factor(data, !! varyable, var_name)

  fdata <-
    data %>%
    dplyr::pull(!! varyable) %>%
    stats::na.omit()

  var_name <-
    data %>%
    dplyr::select(!! varyable) %>%
    names()

  cq <-
    fdata %>%
    sort() %>%
    unique()

  result <-
    fdata %>%
    table() %>%
    as.vector()

  level_names <- levels(fdata)
  data_len    <- length(fdata)
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
    dplyr::pull(!! varyable) %>%
    is.na() %>%
    sum()

  n_obs <-
    data %>%
    dplyr::pull(!! varyable) %>%
    length()

  if (na_count > 0) {
    na_data <- pull(data, !! varyable)

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
    data     = data,
    na_count = na_freq,
    n        = n_obs
  )

  return(result)
}


plot_ds_freq_factor <- function(x, ...) {

  x_lab <-
    x %>%
    magrittr::use_series(varname) %>%
    magrittr::extract(1)

  k <-
    x %>%
    magrittr::use_series(varname) %>%
    magrittr::extract(1) %>%
    rlang::sym()

  p <-
    x %>%
    magrittr::use_series(data) %>%
    dplyr::select(x = !! k) %>%
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = x), fill = "blue") +
    ggplot2::xlab(x_lab) + ggplot2::ylab("Count") +
    ggplot2::ggtitle(paste("Bar plot of", x_lab))

  print(p)

  result <- list(plot = p)
  invisible(result)
}
