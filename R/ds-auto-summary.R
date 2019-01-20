#' Summary statistics
#'
#' Generate summary statistics for all continuous variables in data.
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
      rlang::abort("Data has no continuous variables.")
    }
    plot_data <- data[is_num]
  } else {
    data %<>%
      dplyr::select(!!! var)
    is_num <- sapply(data, is.numeric)
    if (!any(is_num == TRUE)) {
      rlang::abort("Data has no continuous variables.")
    }
    plot_data <- data[is_num]
  }

  if (ncol(plot_data) < 1) {
    rlang::abort("Data has no continuous variables.")
  }

  num_var <- names(plot_data)

  for (i in num_var) {
    cat(cli::rule(center = paste0('Variable: ', i),
                  width = options()$width))
    cat('\n\n')
    cat(cli::rule(center = paste0('Summary Statistics'),
                  width = options()$width))
    cat('\n\n')
    print(ds_summary_stats(data, i))
    cat('\n\n')
    cat(cli::rule(center = paste0('Frequency Distribution'),
                  width = options()$width))
    cat('\n\n')
    print(ds_freq_cont(data, i))
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
      rlang::abort("Data has no categorical variables.")
    }
    if (!any(is_num == TRUE)) {
      rlang::abort("Data has no continuous variables.")
    }
    plot_data  <- cbind(data[is_factor] , data[is_num] )
  } else {
    data %<>%
      dplyr::select(!!! var)
    is_num    <- sapply(data, is.numeric)
    if (!any(is_num == TRUE)) {
      rlang::abort("Data has no continuous variables.")
    }
    is_factor <- sapply(data, is.factor)
    if (!any(is_factor == TRUE)) {
      rlang::abort("Data has no categorical variables.")
    }
    plot_data <- cbind(data[is_factor], data[is_num])
  }

  if (ncol(data) < 1) {
    rlang::abort("Data should include at least one categorical and one continuous variable.")
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
