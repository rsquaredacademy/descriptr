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

  level_names <- levels(fdata)
  data_len    <- length(fdata)
  cq          <- forcats::fct_unique(fdata)

  result <-
    fdata %>%
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
    dplyr::pull(!! varyable) %>%
    is.na() %>%
    sum()

  if (na_count > 0) {
    na_freq <-
      data %>%
      dplyr::pull(!! varyable) %>%
      forcats::fct_count() %>%
      dplyr::pull(n) %>%
      dplyr::last()
  } else {
    na_freq <- 0
  }

  n_obs <-
    data %>%
    dplyr::pull(!! varyable) %>%
    length()

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
