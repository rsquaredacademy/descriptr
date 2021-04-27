#' Generate scatter plots
#'
#' Creates scatter plots if the data has continuous variables.
#'
#' @param data A \code{data.frame} or \code{tibble}.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#' @param ... Column(s) in \code{data}.
#'
#' @examples
#' # plot select variables
#' ds_plot_scatter(mtcarz, mpg, disp)
#'
#' # plot all variables
#' ds_plot_scatter(mtcarz)
#'
#' @importFrom rlang sym
#' @importFrom utils combn
#'
#' @export
#'
ds_plot_scatter <- function(data, ..., print_plot = TRUE) {

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
    if (length(is_num) < 2) {
      stop("Scatter plot requires 2 continuous variables.", call. = FALSE)
    } else {
      plot_data <- data[is_num]
    }
  }

  if (ncol(plot_data) < 1) {
    stop("Data has no continuous variables.", call. = FALSE)
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
      ggplot(data = plot_data) +
      geom_point(aes(x = !! rlang::sym(x), y = !! rlang::sym(y)))
    myplots[[i]] <- p
    names(myplots)[[i]] <- paste(y, "v", x)
  }

  if (print_plot) {
    check_suggests('gridExtra')
    gridExtra::marrangeGrob(myplots, nrow = 2, ncol = 2)
  } else {
    return(myplots)
  }

}

#' Generate histograms
#'
#' Creates histograms if the data has continuous variables.
#'
#' @param data A \code{data.frame} or \code{tibble}.
#' @param ... Column(s) in \code{data}.
#' @param bins Number of bins in the histogram.
#' @param fill Color of the histogram.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#'
#' @examples
#' # plot single variable
#' ds_plot_histogram(mtcarz, mpg)
#'
#' # plot multiple variables
#' ds_plot_histogram(mtcarz, mpg, disp, hp)
#'
#' # plot all variables
#' ds_plot_histogram(mtcarz)
#'
#' @export
#'
ds_plot_histogram <- function(data, ..., bins = 5, fill = 'blue',
                              print_plot = TRUE) {

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

  num_var   <- names(plot_data)
  myplots   <- list()
  n         <- length(num_var)

  for (i in seq_len(n)) {
    x <- num_var[i]
    p <-
      ggplot(data = plot_data) +
      geom_histogram(aes(x = !! rlang::sym(x)), bins = bins,
        fill = fill)
    myplots[[i]] <- p
    names(myplots)[[i]] <- x
  }

  if (print_plot) {
    check_suggests('gridExtra')
    gridExtra::marrangeGrob(myplots, nrow = 2, ncol = 2)
  } else {
    return(myplots)
  }

}


#' Generate density plots
#'
#' Creates density plots if the data has continuous variables.
#'
#' @param data A \code{data.frame} or \code{tibble}.
#' @param ... Column(s) in \code{data}.
#' @param color Color of the plot.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#'
#' @examples
#' # plot single variable
#' ds_plot_density(mtcarz, mpg)
#'
#' # plot multiple variables
#' ds_plot_density(mtcarz, mpg, disp, hp)
#'
#' # plot all variables
#' ds_plot_density(mtcarz)
#'
#' @export
#'
ds_plot_density <- function(data, ..., color = 'blue', print_plot = TRUE) {

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

  num_var   <- names(plot_data)
  myplots   <- list()
  n         <- length(num_var)

  for (i in seq_len(n)) {
    x <- num_var[i]
    p <-
      ggplot(data = plot_data) +
      geom_density(aes(x = !! rlang::sym(x)), color = color)
    myplots[[i]] <- p
    names(myplots)[[i]] <- x
  }

  if (print_plot) {
    check_suggests('gridExtra')
    gridExtra::marrangeGrob(myplots, nrow = 2, ncol = 2)
  } else {
    return(myplots)
  }

}

#' Generate bar plots
#'
#' Creates bar plots if the data has categorical variables.
#'
#' @param data A \code{data.frame} or \code{tibble}.
#' @param ... Column(s) in \code{data}.
#' @param fill Color of the bars.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#'
#' @examples
#' # plot single variable
#' ds_plot_bar(mtcarz, cyl)
#'
#' # plot multiple variables
#' ds_plot_bar(mtcarz, cyl, gear)
#'
#' # plot all variables
#' ds_plot_bar(mtcarz)
#'
#' @export
#'
ds_plot_bar <- function(data, ..., fill = 'blue', print_plot = TRUE) {

  check_df(data)
  var <- rlang::quos(...)
  is_factor <- sapply(data, is.factor)

  if (length(var) < 1) {
    if (!any(is_factor == TRUE)) {
      stop("Data has no categorical variables.", call. = FALSE)
    }
    plot_data <- data[is_factor]
  } else {
    data %<>%
      dplyr::select(!!! var)
    is_factor <- sapply(data, is.factor)
    if (!any(is_factor == TRUE)) {
      stop("Data has no categorical variables.", call. = FALSE)
    }
    plot_data <- data[is_factor]
  }

  if (ncol(plot_data) < 1) {
    stop("Data has no categorical variables.", call. = FALSE)
  }

  factor_var <- names(plot_data)
  myplots    <- list()
  n          <- length(factor_var)

  for (i in seq_len(n)) {
    x <- factor_var[i]
    p <-
      ggplot(data = plot_data) +
      geom_bar(aes(x = !! rlang::sym(x)), fill = fill)
    myplots[[i]] <- p
    names(myplots)[[i]] <- x
  }

  if (print_plot) {
    check_suggests('gridExtra')
    gridExtra::marrangeGrob(myplots, nrow = 2, ncol = 2)
  } else {
    return(myplots)
  }

}


#' Generate box plots
#'
#' Creates box plots if the data has continuous variables.
#'
#' @param data A \code{data.frame} or \code{tibble}.
#' @param ... Column(s) in \code{data}.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#'
#' @examples
#' # plot single variable
#' ds_plot_box_single(mtcarz, mpg)
#'
#' # plot multiple variables
#' ds_plot_box_single(mtcarz, mpg, disp, hp)
#'
#' # plot all variables
#' ds_plot_box_single(mtcarz)
#'
#' @export
#'
ds_plot_box_single <- function(data, ..., print_plot = TRUE) {

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

  num_var   <- names(plot_data)
  myplots   <- list()
  n         <- length(num_var)

  for (i in seq_len(n)) {
    x <- num_var[i]
    p <-
      ggplot(data = plot_data) +
      geom_boxplot(aes(x = factor(1), y = !! rlang::sym(x))) +
      labs(x = ' ')
    myplots[[i]] <- p
    names(myplots)[[i]] <- x
  }

  if (print_plot) {
    check_suggests('gridExtra')
    gridExtra::marrangeGrob(myplots, nrow = 2, ncol = 2)
  } else {
    return(myplots)
  }

}


#' Generate stacked bar plots
#'
#' Creates stacked bar plots if the data has categorical variables.
#'
#' @param data A \code{data.frame} or \code{tibble}.
#' @param ... Column(s) in \code{data}.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#'
#' @examples
#' # subset data
#' mt <- dplyr::select(mtcarz, cyl, gear, am)
#'
#' # stacked bar plot
#' ds_plot_bar_stacked(mtcarz, cyl, gear)
#'
#' # plot all variables
#' ds_plot_bar_stacked(mt)
#'
#' @export
#'
ds_plot_bar_stacked <- function(data, ..., print_plot = TRUE) {

  check_df(data)
  var <- rlang::quos(...)
  is_factor <- sapply(data, is.factor)

  if (length(var) < 1) {
    if (!any(is_factor == TRUE)) {
      stop("Data has no categorical variables.", call. = FALSE)
    }
    plot_data <- data[is_factor]
  } else {
    data %<>%
      dplyr::select(!!! var)
    is_factor <- sapply(data, is.factor)
    if (!any(is_factor == TRUE)) {
      stop("Data has no categorical variables.", call. = FALSE)
    }
    if (length(is_factor) < 2) {
      stop("Stacked bar plot requires 2 categorical variables.", call. = FALSE)
    } else {
      plot_data <- data[is_factor]
    }
  }

  if (ncol(plot_data) < 1) {
    stop("Data has no categorical variables.", call. = FALSE)
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
      ggplot(data = plot_data) +
      geom_bar(aes(x = !! rlang::sym(x), fill = !! rlang::sym(y)))
    myplots[[i]] <- p
    names(myplots)[[i]] <- paste(y, "v", x)
  }

  if (print_plot) {
    check_suggests('gridExtra')
    gridExtra::marrangeGrob(myplots, nrow = 2, ncol = 2)
  } else {
    return(myplots)
  }

}

#' Generate grouped bar plots
#'
#' Creates grouped bar plots if the data has categorical variables.
#'
#' @param data A \code{data.frame} or \code{tibble}.
#' @param ... Column(s) in \code{data}.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#'
#' @examples
#' # subset data
#' mt <- dplyr::select(mtcarz, cyl, gear, am)
#'
#' # grouped bar plot
#' ds_plot_bar_grouped(mtcarz, cyl, gear)
#'
#' # plot all variables
#' ds_plot_bar_grouped(mt)
#'
#' @export
#'
ds_plot_bar_grouped <- function(data, ..., print_plot = TRUE) {

  check_df(data)
  var <- rlang::quos(...)
  is_factor <- sapply(data, is.factor)

  if (length(var) < 1) {
    if (!any(is_factor == TRUE)) {
      stop("Data has no categorical variables.", call. = FALSE)
    }
    plot_data <- data[is_factor]
  } else {
    data %<>%
      dplyr::select(!!! var)
    is_factor <- sapply(data, is.factor)
    if (!any(is_factor == TRUE)) {
      stop("Data has no categorical variables.", call. = FALSE)
    }
    if (length(is_factor) < 2) {
      stop("Grouped bar plot requires 2 categorical variables.", call. = FALSE)
    } else {
      plot_data <- data[is_factor]
    }
  }

  if (ncol(plot_data) < 1) {
    stop("Data has no categorical variables.", call. = FALSE)
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
      ggplot(data = plot_data) +
      geom_bar(aes(x = !! rlang::sym(x), fill = !! rlang::sym(y)),
        position = 'dodge')
    myplots[[i]] <- p
    names(myplots)[[i]] <- paste(y, "v", x)
  }

  if (print_plot) {
    check_suggests('gridExtra')
    gridExtra::marrangeGrob(myplots, nrow = 2, ncol = 2)
  } else {
    return(myplots)
  }

}


#' Compare distributions
#'
#' Creates box plots if the data has both categorical & continuous variables.
#'
#' @param data A \code{data.frame} or \code{tibble}.
#' @param ... Column(s) in \code{data}.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#'
#' @examples
#' # subset data
#' mt <- dplyr::select(mtcarz, cyl, disp, mpg)
#'
#' # plot select variables
#' ds_plot_box_group(mtcarz, cyl, gear, mpg)
#'
#' # plot all variables
#' ds_plot_box_group(mt)
#'
#' @export
#'
ds_plot_box_group <- function(data, ..., print_plot = TRUE) {

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
    x <- as.character(combs[i, 1])
    y <- as.character(combs[i, 2])
    p <-
      ggplot(data = plot_data) +
      geom_boxplot(aes(x = !! rlang::sym(x), y = !! rlang::sym(y)))
    myplots[[i]] <- p
    names(myplots)[[i]] <- paste(y, "v", x)
  }

  if (print_plot) {
    check_suggests('gridExtra')
    gridExtra::marrangeGrob(myplots, nrow = 2, ncol = 2)
  } else {
    return(myplots)
  }

}
