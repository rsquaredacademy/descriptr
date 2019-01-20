#' Generate scatter plots
#'
#' Creates scatter plots if the data has continuous variables.
#'
#' @param data A \code{data.frame} or \code{tibble}.
#' @param ... Column(s) in \code{data}.
#'
#' @examples
#' ds_plot_scatter(mtcarz)
#' ds_plot_scatter(mtcarz, mpg, disp, hp)
#'
#' @importFrom rlang sym
#' @importFrom utils combn
#'
#' @export
#'
ds_plot_scatter <- function(data, ...) {

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
    if (length(is_num) < 2) {
      rlang::abort("Scatter plot requires 2 continuous variables.")
    } else {
      plot_data <- data[is_num]
    }
  }

  if (ncol(plot_data) < 1) {
    rlang::abort("Data has no continuous variables.")
  }

  num_var   <- names(plot_data)
  num_start <- combn(num_var, 2)
  num_num   <- cbind(num_start, rbind(num_start[2, ], num_start[1, ]))
  myplots   <- list()
  n         <- dim(num_num)[2]

  for (i in seq_len(n)) {
    x <- num_num[, i][1]
    y <- num_num[, i][2]
    p <-
      ggplot2::ggplot(data = plot_data) +
      ggplot2::geom_point(ggplot2::aes(x = !! rlang::sym(x), y = !! rlang::sym(y)))
    myplots[[i]] <- p
  }

  result <- gridExtra::marrangeGrob(myplots, nrow = 2, ncol = 2)
  result

}

#' Generate histograms
#'
#' Creates histograms if the data has continuous variables.
#'
#' @param data A \code{data.frame} or \code{tibble}.
#' @param ... Column(s) in \code{data}.
#' @param bins Number of bins in the histogram.
#' @param fill Color of the histogram.
#'
#' @examples
#' ds_plot_histogram(mtcarz)
#' ds_plot_histogram(mtcarz, mpg)
#' ds_plot_histogram(mtcarz, mpg, disp, hp)
#'
#' @export
#'
ds_plot_histogram <- function(data, ..., bins = 5, fill = 'blue') {

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

  num_var   <- names(plot_data)
  myplots   <- list()
  n         <- length(num_var)

  for (i in seq_len(n)) {
    x <- num_var[i]
    p <-
      ggplot2::ggplot(data = plot_data) +
      ggplot2::geom_histogram(ggplot2::aes(x = !! rlang::sym(x)), bins = bins, 
        fill = fill)
    myplots[[i]] <- p
  }

  result <- gridExtra::marrangeGrob(myplots, nrow = 2, ncol = 2)
  result

}


#' Generate density plots
#'
#' Creates density plots if the data has continuous variables.
#'
#' @param data A \code{data.frame} or \code{tibble}.
#' @param ... Column(s) in \code{data}.
#' @param color Color of the plot.
#'
#' @examples
#' ds_plot_density(mtcarz)
#' ds_plot_density(mtcarz, mpg)
#' ds_plot_density(mtcarz, mpg, disp, hp)
#'
#' @export
#'
ds_plot_density <- function(data, ..., color = 'blue') {

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

  num_var   <- names(plot_data)
  myplots   <- list()
  n         <- length(num_var)

  for (i in seq_len(n)) {
    x <- num_var[i]
    p <-
      ggplot2::ggplot(data = plot_data) +
      ggplot2::geom_density(ggplot2::aes(x = !! rlang::sym(x)), color = color)
    myplots[[i]] <- p
  }

  result <- gridExtra::marrangeGrob(myplots, nrow = 2, ncol = 2)
  result

}

#' Generate bar plots
#'
#' Creates bar plots if the data has categorical variables.
#'
#' @param data A \code{data.frame} or \code{tibble}.
#' @param ... Column(s) in \code{data}.
#' @param fill Color of the bars.
#'
#' @examples
#' ds_plot_bar(mtcarz)
#' ds_plot_bar(mtcarz, cyl)
#' ds_plot_bar(mtcarz, cyl, gear)
#'
#' @export
#'
ds_plot_bar <- function(data, ..., fill = 'blue') {

  check_df(data)
  var <- rlang::quos(...)
  is_factor <- sapply(data, is.factor)

  if (length(var) < 1) {
    if (!any(is_factor == TRUE)) {
      rlang::abort("Data has no categorical variables.")
    }
    plot_data <- data[is_factor]
  } else {
    data %<>%
      dplyr::select(!!! var)
    is_factor <- sapply(data, is.factor)
    if (!any(is_factor == TRUE)) {
      rlang::abort("Data has no categorical variables.")
    }
    plot_data <- data[is_factor]
  }

  if (ncol(plot_data) < 1) {
    rlang::abort("Data has no categorical variables.")
  }

  factor_var <- names(plot_data)
  myplots    <- list()
  n          <- length(factor_var)

  for (i in seq_len(n)) {
    x <- factor_var[i]
    p <-
      ggplot2::ggplot(data = plot_data) +
      ggplot2::geom_bar(ggplot2::aes(x = !! rlang::sym(x)), fill = fill)
    myplots[[i]] <- p
  }

  result <- gridExtra::marrangeGrob(myplots, nrow = 2, ncol = 2)
  result

}


#' Generate box plots
#'
#' Creates box plots if the data has continuous variables.
#'
#' @param data A \code{data.frame} or \code{tibble}.
#' @param ... Column(s) in \code{data}.
#'
#' @examples
#' ds_plot_box_single(mtcarz)
#' ds_plot_box_single(mtcarz, mpg)
#' ds_plot_box_single(mtcarz, mpg, disp, hp)
#'
#' @export
#'
ds_plot_box_single <- function(data, ...) {

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

  num_var   <- names(plot_data)
  myplots   <- list()
  n         <- length(num_var)

  for (i in seq_len(n)) {
    x <- num_var[i]
    p <-
      ggplot2::ggplot(data = plot_data) +
      ggplot2::geom_boxplot(ggplot2::aes(x = factor(1), y = !! rlang::sym(x))) +
      ggplot2::labs(x = ' ')
    myplots[[i]] <- p
  }

  result <- gridExtra::marrangeGrob(myplots, nrow = 2, ncol = 2)
  result

}


#' Generate stacked bar plots
#'
#' Creates stacked bar plots if the data has categorical variables.
#'
#' @param data A \code{data.frame} or \code{tibble}.
#' @param ... Column(s) in \code{data}.
#'
#' @examples
#' mt <- dplyr::select(mtcarz, cyl, gear, am)
#' ds_plot_bar_stacked(mt)
#' ds_plot_bar_stacked(mtcarz, cyl, gear)
#'
#' @export
#'
ds_plot_bar_stacked <- function(data, ...) {

  check_df(data)
  var <- rlang::quos(...)
  is_factor <- sapply(data, is.factor)

  if (length(var) < 1) {
    if (!any(is_factor == TRUE)) {
      rlang::abort("Data has no categorical variables.")
    }
    plot_data <- data[is_factor]
  } else {
    data %<>%
      dplyr::select(!!! var)
    is_factor <- sapply(data, is.factor)
    if (!any(is_factor == TRUE)) {
      rlang::abort("Data has no categorical variables.")
    }
    if (length(is_factor) < 2) {
      rlang::abort("Stacked bar plot requires 2 categorical variables.")
    } else {
      plot_data <- data[is_factor]
    }
  }

  if (ncol(plot_data) < 1) {
    rlang::abort("Data has no categorical variables.")
  }

  factor_var    <- names(plot_data)
  factor_start  <- combn(factor_var, 2)
  fact_fact     <- cbind(factor_start, rbind(factor_start[2, ], factor_start[1, ]))
  myplots       <- list()
  n             <- dim(fact_fact)[2]

  for (i in seq_len(n)) {
    x <- fact_fact[, i][1]
    y <- fact_fact[, i][2]
    p <-
      ggplot2::ggplot(data = plot_data) +
      ggplot2::geom_bar(ggplot2::aes(x = !! rlang::sym(x), fill = !! rlang::sym(y)))
    myplots[[i]] <- p
  }

  result <- gridExtra::marrangeGrob(myplots, nrow = 2, ncol = 2)
  result

}

#' Generate grouped bar plots
#'
#' Creates grouped bar plots if the data has categorical variables.
#'
#' @param data A \code{data.frame} or \code{tibble}.
#' @param ... Column(s) in \code{data}.
#'
#' @examples
#' mt <- dplyr::select(mtcarz, cyl, gear, am)
#' ds_plot_bar_grouped(mt)
#' ds_plot_bar_grouped(mtcarz, cyl, gear)
#'
#' @export
#'
ds_plot_bar_grouped <- function(data, ...) {

  check_df(data)
  var <- rlang::quos(...)
  is_factor <- sapply(data, is.factor)

  if (length(var) < 1) {
    if (!any(is_factor == TRUE)) {
      rlang::abort("Data has no categorical variables.")
    }
    plot_data <- data[is_factor]
  } else {
    data %<>%
      dplyr::select(!!! var)
    is_factor <- sapply(data, is.factor)
    if (!any(is_factor == TRUE)) {
      rlang::abort("Data has no categorical variables.")
    }
    if (length(is_factor) < 2) {
      rlang::abort("Grouped bar plot requires 2 categorical variables.")
    } else {
      plot_data <- data[is_factor]
    }
  }

  if (ncol(plot_data) < 1) {
    rlang::abort("Data has no categorical variables.")
  }

  factor_var    <- names(plot_data)
  factor_start  <- combn(factor_var, 2)
  fact_fact     <- cbind(factor_start, rbind(factor_start[2, ], factor_start[1, ]))
  myplots       <- list()
  n             <- dim(fact_fact)[2]

  for (i in seq_len(n)) {
    x <- fact_fact[, i][1]
    y <- fact_fact[, i][2]
    p <-
      ggplot2::ggplot(data = plot_data) +
      ggplot2::geom_bar(ggplot2::aes(x = !! rlang::sym(x), fill = !! rlang::sym(y)),
        position = 'dodge')
    myplots[[i]] <- p
  }

  result <- gridExtra::marrangeGrob(myplots, nrow = 2, ncol = 2)
  result

}


#' Compate distributions
#'
#' Creates box plots if the data has both categorical & continuous variables.
#'
#' @param data A \code{data.frame} or \code{tibble}.
#' @param ... Column(s) in \code{data}.
#'
#' @examples
#' mt <- dplyr::select(mtcarz, cyl, disp, mpg)
#' ds_plot_box_group(mt)
#' ds_plot_box_group(mtcarz, cyl, gear, mpg)
#'
#' @export
#'
ds_plot_box_group <- function(data, ...) {

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
    x <- as.character(combs[i, 1])
    y <- as.character(combs[i, 2])
    p <-
      ggplot2::ggplot(data = plot_data) +
      ggplot2::geom_boxplot(ggplot2::aes(x = !! rlang::sym(x), y = !! rlang::sym(y)))
    myplots[[i]] <- p
  }

  result <- gridExtra::marrangeGrob(myplots, nrow = 2, ncol = 2)
  result

}