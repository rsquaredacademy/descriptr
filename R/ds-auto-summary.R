#' Descriptive statistics and frquency tables
#'
#' Generate summary statistics & frequency table for all continuous variables in data.
#'
#' @param data A \code{data.frame} or \code{tibble}.
#' @param ... Column(s) in \code{data}.
#'
#' @examples
#' ds_auto_summary_stats(mtcarz)
#' ds_auto_summary_stats(mtcarz, disp, hp)
#'
#' @export
#'
ds_auto_summary_stats <- function(data, ...) {

  check_df(data)
  var <- rlang::quos(...)
  is_num <- sapply(data, is.numeric)

  if (length(var) < 1) {
    if (!any(is_num == TRUE)) {
      stop("Data has no continuous variables.", call. = FALSE)
    }
    plot_data <- data[is_num]
  } else {
    data %<>%
      dplyr::select(!!! var)
    is_num <- sapply(data, is.numeric)
    if (!any(is_num == TRUE)) {
      stop("Data has no continuous variables.", call. = FALSE)
    }
    plot_data <- data[is_num]
  }

  if (ncol(plot_data) < 1) {
    stop("Data has no continuous variables.", call. = FALSE)
  }

  num_var <- names(plot_data)

  for (i in num_var) {

    ds_rule(paste0('Variable: ', i))
    cat('\n\n')
    ds_rule(paste0('Summary Statistics'))
    cat('\n\n')
    print(ds_summary_stats(data, i))
    cat('\n\n')
    ds_rule(paste0('Frequency Distribution'))
    cat('\n\n')
    print(ds_freq_table(data, i))
    cat('\n\n\n')
  }

}

#' Tabulation
#'
#' Generate summary statistics for all continuous variables in data.
#'
#' @param data A \code{data.frame} or \code{tibble}.
#' @param ... Column(s) in \code{data}.
#'
#' @examples
#' ds_auto_group_summary(mtcarz, cyl, gear, mpg, disp)
#'
#' @export
#'
ds_auto_group_summary <- function(data, ...) {

  check_df(data)
  var <- rlang::quos(...)

  is_num    <- sapply(data, is.numeric)
  is_factor <- sapply(data, is.factor)

  if (length(var) < 1) {
    if (!any(is_factor == TRUE)) {
      stop("Data has no categorical variables.", call. = FALSE)
    }
    if (!any(is_num == TRUE)) {
      stop("Data has no continuous variables.", call. = FALSE)
    }
    plot_data  <- cbind(data[is_factor] , data[is_num] )
  } else {
    data %<>%
      dplyr::select(!!! var)
    is_num    <- sapply(data, is.numeric)
    if (!any(is_num == TRUE)) {
      stop("Data has no continuous variables.", call. = FALSE)
    }
    is_factor <- sapply(data, is.factor)
    if (!any(is_factor == TRUE)) {
      stop("Data has no categorical variables.", call. = FALSE)
    }
    plot_data <- cbind(data[is_factor], data[is_num])
  }

  if (ncol(data) < 1) {
    stop("Data should include at least one categorical and one continuous variable.", call. = FALSE)
  }

  is_num    <- sapply(plot_data, is.numeric)
  is_factor <- sapply(plot_data, is.factor)
  num_data  <- plot_data[is_num]
  fact_data <- plot_data[is_factor]
  fact_var  <- names(fact_data)
  num_var   <- names(num_data)
  combs     <- expand.grid(fact_var, num_var)
  myplots   <- list()
  n         <- nrow(combs)

  for (i in seq_len(n)) {
    print(ds_group_summary(data, !! sym(as.character(combs[i, 1])), !! sym(as.character(combs[i, 2]))))
    cat('\n\n\n')
  }
}
