#' Measures of location
#'
#' Returns the measures of location such as mean, median & mode.
#'
#' @param data A \code{data.frame} or \code{tibble}.
#' @param ... Column(s) in \code{data}.
#' @param trim The fraction of values to be trimmed before computing
#'   the mean.
#'
#' @examples
#' ds_measures_location(mtcarz)
#' ds_measures_location(mtcarz, mpg)
#' ds_measures_location(mtcarz, mpg, disp)
#'
#' @export
#'
ds_measures_location <- function(data, ..., trim = 0.05) {

  var <- rlang::quos(...)

  if (length(var) < 1) {
    is_num <- sapply(data, is.numeric)
    if (!any(is_num == TRUE)) {
      rlang::abort("Data has no continuous variables.")
    }
    data <- data[, is_num]
  } else {
    data %<>%
      dplyr::select(!!! var)
  }

  data %>%
    tidyr::drop_na() %>%
    tidyr::gather(var, values) %>%
    dplyr::group_by(var) %>%
    dplyr::summarise_all(dplyr::funs(mean = mean, 
                                     trim_mean = mean(., trim = trim),
                                     median = stats::median, 
                                     mode = ds_mode)) %>%
    tibble::as_tibble()

}

#' Measures of variation
#'
#' Returns the measures of location such as range, variance and standard
#'   deviation.
#'
#' @param data A \code{data.frame} or \code{tibble}.
#' @param ... Column(s) in \code{data}.
#'
#' @examples
#' ds_measures_variation(mtcarz)
#' ds_measures_variation(mtcarz, mpg)
#' ds_measures_variation(mtcarz, mpg, disp)
#'
#' @export
#'
ds_measures_variation <- function(data, ...) {

  var <- rlang::quos(...)

  if (length(var) < 1) {
    is_num <- sapply(data, is.numeric)
    if (!any(is_num == TRUE)) {
      rlang::abort("Data has no continuous variables.")
    }
    data <- data[, is_num]
  } else {
    data %<>%
      dplyr::select(!!! var)
  }

  data %>%
    tidyr::drop_na() %>%
    tidyr::gather(var, values) %>%
    dplyr::group_by(var) %>%
    dplyr::summarise_all(dplyr::funs(range = ds_range, iqr = stats::IQR, 
                                     variance = stats::var, sd = stats::sd, 
                                     coeff_var = ds_cvar, 
                                     std_error = ds_std_error)) %>%
    tibble::as_tibble()
}

#' Measures of symmetry
#'
#' Returns the measures of symmetry such as skewness and kurtosis.
#'
#' @param data A \code{data.frame} or \code{tibble}.
#' @param ... Column(s) in \code{data}.
#'
#' @examples
#' ds_measures_symmetry(mtcarz)
#' ds_measures_symmetry(mtcarz, mpg)
#' ds_measures_symmetry(mtcarz, mpg, disp)
#'
#' @export
#'
ds_measures_symmetry <- function(data, column) {

  var <- rlang::quos(...)

  if (length(var) < 1) {
    is_num <- sapply(data, is.numeric)
    if (!any(is_num == TRUE)) {
      rlang::abort("Data has no continuous variables.")
    }
    data <- data[, is_num]
  } else {
    data %<>%
      dplyr::select(!!! var)
  }

  data %>%
    tidyr::drop_na() %>%
    tidyr::gather(var, values) %>%
    dplyr::group_by(var) %>%
    dplyr::summarise_all(dplyr::funs(skewness = ds_skewness,
                                     kurtosis = ds_kurtosis)) %>%
    tibble::as_tibble()
}


#' Percentiles
#'
#' Returns the percentiles
#'
#' @param data A \code{data.frame} or \code{tibble}.
#' @param ... Column(s) in \code{data}.
#'
#' @examples
#' ds_percentiles(mtcarz)
#' ds_percentiles(mtcarz, mpg)
#' ds_percentiles(mtcarz, mpg, disp)
#'
#' @export
#'
ds_percentiles <- function(data, column) {

  var <- rlang::quos(...)

  if (length(var) < 1) {
    is_num <- sapply(data, is.numeric)
    if (!any(is_num == TRUE)) {
      rlang::abort("Data has no continuous variables.")
    }
    data <- data[, is_num]
  } else {
    data %<>%
      dplyr::select(!!! var)
  }

  data %>%
    tidyr::drop_na() %>%
    tidyr::gather(var, values) %>%
    dplyr::group_by(var) %>%
    dplyr::summarise_all(dplyr::funs(min    = min,
                       per1   = stats::quantile(., 0.01),
                       per5   = stats::quantile(., 0.05),
                       per10  = stats::quantile(., 0.10),
                       q1     = stats::quantile(., 0.25),
                       median = stats::median,
                       q3     = stats::quantile(., 0.75),
                       per95  = stats::quantile(., 0.95),
                       per90  = stats::quantile(., 0.90),
                       per99  = stats::quantile(., 0.99),
                       max    = max)) %>%
    tibble::as_tibble()
}

#' Extreme observations
#'
#' Returns the most extreme observations.
#'
#' @param data A \code{data.frame} or \code{tibble}.
#' @param column Column in \code{data}.
#'
#' @examples
#' ds_extreme_obs(mtcarz, mpg)
#'
#' @export
#'
ds_extreme_obs <- function(data, column) {

  var <- rlang::enquo(column)

  na_data <-
    data %>%
    dplyr::select(!! var) %>%
    tidyr::drop_na() %>%
    dplyr::pull(1)

  tibble::tibble(type = c(rep("high", 5), rep("low", 5)),
         value = c(ds_tailobs(na_data, 5, "high"),
                   ds_tailobs(na_data, 5, "low")),
         index = ds_rindex(na_data, value))


}

#' @importFrom magrittr %>%
#' @title Tail Observations
#' @description Returns the n highest/lowest observations from a numeric vector.
#' @param data a numeric vector
#' @param n number of observations to be returned
#' @param type if \code{low}, the \code{n} lowest observations are returned, else the
#' highest \code{n} obervations are returned
#' @details Any NA values are stripped from \code{data} before computation
#' takes place.
#' @return \code{n} highest/lowest observations from \code{data}
#' @section Deprecated function:
#' \code{tailobs()} has been deprecated. Instead use \code{ds_tailobs()}.
#' @examples
#' ds_tailobs(mtcarz$mpg, 5)
#' ds_tailobs(mtcarz$mpg, 5, type = "high")
#' @export
#' @seealso \code{\link[dplyr]{top_n}}
#'
ds_tailobs <- function(data, n, type = c("low", "high")) {

  if (!is.numeric(data)) {
    stop("data must be numeric")
  }

  if (!is.numeric(n)) {
    stop("n must be numeric")
  }

  if (n > length(data)) {
    stop("n must be less than the length of data")
  }

  method <- match.arg(type)

  if (method == "low") {
    result <-
      data %>%
      stats::na.omit() %>%
      sort() %>%
      `[`(1:n)
  } else {
    result <-
      data %>%
      stats::na.omit() %>%
      sort(decreasing = TRUE) %>%
      `[`(1:n)
  }

  return(result)

}

#' @export
#' @rdname ds_tailobs
#' @usage NULL
#'
tailobs <- function(data, n, type = c("low", "high")) {
  .Deprecated("ds_tailobs()")
  ds_tailobs(data, n, type)
}

#' @title Geometric Mean
#' @description Compute the geometric mean
#' @param x a numeric vector containing the values whose geometric mean is to be
#' computed
#' @param na.rm a logical value indicating whether NA values should be stripped before the computation proceeds.
#' @param ... further arguments passed to or from other methods
#' #' @details Any NA values are stripped from \code{x} before computation
#' takes place.
#' @return Returns the geometric mean of \code{x}
#' @section Deprecated function:
#' \code{gmean()} has been deprecated. Instead use \code{ds_gmean()}.
#' @examples
#' ds_gmean(mtcars$mpg)
#' @export
#' @seealso \code{\link{ds_hmean}} \code{\link[base]{mean}}
#'
ds_gmean <- function(x, na.rm = FALSE, ...) {

  if (!is.numeric(x)) {
    stop("x must be numeric")
  }

  if (na.rm) {
    x <- stats::na.omit(x)
  }

  prod(x) ^ (1 / length(x))

}

#' @export
#' @rdname ds_gmean
#' @usage NULL
#'
gmean <- function(x, ...) {
  .Deprecated("ds_gmean()")
  ds_gmean(x, ...)
}


#' @title Harmonic Mean
#' @description Compute the harmonic mean
#' @param x a numeric vector containing the values whose harmonic mean is to be
#' computed
#' @param na.rm a logical value indicating whether NA values should be stripped before the computation proceeds.
#' @param ... further arguments passed to or from other methods
#' #' @details Any NA values are stripped from \code{x} before computation
#' takes place.
#' @return Returns the harmonic mean of \code{x}
#' @section Deprecated function:
#' \code{hmean()} has been deprecated. Instead use \code{ds_hmean()}.
#' @examples
#' ds_hmean(mtcars$mpg)
#' @export
#' @seealso \code{\link{ds_gmean}} \code{\link[base]{mean}}
#'
ds_hmean <- function(x, na.rm = FALSE, ...) {

  if (!is.numeric(x)) {
    stop("x must be numeric")
  }

  if (na.rm) {
    x <- stats::na.omit(x)
  }

  length(x) / sum(sapply(x, div_by))

}

#' @export
#' @rdname ds_hmean
#' @usage NULL
#'
hmean <- function(x, ...) {
  .Deprecated("ds_hmean()")
  ds_hmean(x, ...)
}

#' @title Mode
#' @description Compute the sample mode
#' @param x a numeric vector containing the values whose mode is to be computed
#' @param na.rm a logical value indicating whether NA values should be stripped before the computation proceeds.
#' @details Any NA values are stripped from \code{x} before computation
#' takes place.
#' @return Mode of \code{x}
#' @section Deprecated Function:
#' `stat_mode()` has been deprecated. Instead use `ds_mode()`.
#' @examples
#' ds_mode(mtcars$mpg)
#' ds_mode(mtcars$cyl)
#' @seealso \code{\link[base]{mean}} \code{\link[stats]{median}}
#' @export
#'
ds_mode <- function(x, na.rm = FALSE) {

  if (!is.numeric(x)) {
    stop("x must be numeric")
  }

  if (na.rm) {
    x <- stats::na.omit(x)
  }

  Freq <- NULL

  x %>%
    table() %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    dplyr::arrange(dplyr::desc(Freq)) %>%
    dplyr::filter(Freq == max(Freq)) %>%
    dplyr::select(dplyr::contains(".")) %>%
    unlist() %>%
    as.numeric() %>%
    min()

}

#' @export
#' @rdname ds_mode
#' @usage NULL
#'
stat_mode <- function(x) {
  .Deprecated("ds_mode()")
  ds_mode(x)
}



#' @title Range
#' @description Compute the range of a numeric vector
#' @param x a numeric vector
#' @param na.rm a logical value indicating whether NA values should be stripped before the computation proceeds.
#' @return Range of \code{x}
#' @section Deprecated Function:
#' \code{stat_range()} has been deprecated. Instead use \code{ds_range()}.
#' @examples
#' ds_range(mtcars$mpg)
#' @seealso \code{\link[base]{range}}
#' @export
#'
ds_range <- function(x, na.rm = FALSE) {

  if (!is.numeric(x)) {
    stop("data must be numeric")
  }

  if (na.rm) {
    x <- stats::na.omit(x)
  }

  x %>%
    range() %>%
    diff()

}

#' @export
#' @rdname ds_range
#' @usage NULL
#'
stat_range <- function(x) {
  .Deprecated("ds_range()")
  ds_range(x)
}

#' @title Kurtosis
#' @description Compute the kurtosis of a probability distribution.
#' @param x a numeric vector containing the values whose kurtosis is to be computed
#' @param na.rm a logical value indicating whether NA values should be stripped before the computation proceeds.
#' @details Any NA values are stripped from \code{x} before computation
#' takes place.
#' @return Kurtosis of \code{x}
#' @section Deprecated Function:
#' \code{kurtosis()} has been deprecated. Instead use \code{ds_kurtosis()}.
#' @examples
#' ds_kurtosis(mtcars$mpg)
#' @seealso \code{ds_skewness}
#' @references Sheskin, D.J. (2000) Handbook of Parametric and Nonparametric Statistical Procedures, Second Edition. Boca Raton, Florida: Chapman & Hall/CRC.
#' @export
#'
ds_kurtosis <- function(x, na.rm = FALSE) {

  if (!is.numeric(x)) {
    stop("x must be numeric")
  }

  if (na.rm) {
    x <- stats::na.omit(x)
  }

  n <- length(x)
  summation <- sums(x, 4)
  part1 <- (n * (n + 1)) / ((n - 1) * (n - 2) * (n - 3))
  part2 <- (3 * (n - 1) ^ 2) / ((n - 2) * (n - 3))
  (part1 * summation) - part2

}

#' @export
#' @rdname ds_kurtosis
#' @usage NULL
#'
kurtosis <- function(x) {
  .Deprecated("ds_kurtosis()")
  ds_kurtosis(x)
}


#' @title Skewness
#' @description Compute the skewness of a probability distribution.
#' @param x a numeric vector containing the values whose skewness is to be computed
#' @param na.rm a logical value indicating whether NA values should be stripped before the computation proceeds.
#' @details Any NA values are stripped from \code{x} before computation
#' takes place.
#' @return Skewness of \code{x}
#' @section Deprecated Function:
#' \code{skewness()} has been deprecated. Instead use \code{ds_skewness()}.
#' @examples
#' ds_skewness(mtcars$mpg)
#' @seealso \code{kurtosis}
#' @references Sheskin, D.J. (2000) Handbook of Parametric and Nonparametric Statistical Procedures, Second Edition. Boca Raton, Florida: Chapman & Hall/CRC.
#' @export
#'
ds_skewness <- function(x, na.rm = FALSE) {

  if (!is.numeric(x)) {
    stop("x must be numeric")
  }

  if (na.rm) {
    x <- stats::na.omit(x)
  }

  n <- length(x)
  summation <- sums(x, 3)
  (n / ((n - 1) * (n - 2))) * summation

}

#' @export
#' @rdname ds_skewness
#' @usage NULL
#'
skewness <- function(x) {
  .Deprecated("ds_skewness()")
  ds_skewness(x)
}


#' @title Mean Absolute Deviation
#' @description Compute the mean absolute deviation about the mean
#' @param x a numeric vector
#' @param na.rm a logical value indicating whether NA values should be stripped before the computation proceeds.
#' @details The \code{stat_mdev} function computes the mean absolute deviation
#' about the mean. It is different from \code{mad} in \code{stats} package as
#' the statistic used to compute the deviations is not \code{median} but
#' \code{mean}. Any NA values are stripped from \code{x} before computation
#' takes place
#' @return Mean absolute deviation of \code{x}
#' @section Deprecated Function:
#' \code{stat_mdev()} has been deprecated. Instead use \code{ds_mdev()}.
#' @examples
#' ds_mdev(mtcars$mpg)
#' @seealso \code{\link[stats]{mad}}
#' @export
#'
ds_mdev <- function(x, na.rm = FALSE) {

  if (!is.numeric(x)) {
    stop("x must be numeric")
  }

  if (na.rm) {
    x <- stats::na.omit(x)
  }

  m <- mean(x)
  sum(sapply(x, md_helper, m)) / length(x)

}

#' @export
#' @rdname ds_mdev
#' @usage NULL
#'
stat_mdev <- function(x) {
  .Deprecated("ds_mdev()")
  ds_mdev(x)
}

#' @title Coefficient of Variation
#' @description Compute the coefficient of variation
#' @param x a numeric vector containing the values whose mode is to be computed
#' @param na.rm a logical value indicating whether NA values should be stripped before the computation proceeds.
#' @details Any NA values are stripped from \code{x} before computation
#' takes place.
#' @section Deprecated Function:
#' \code{stat_cvar()} has been deprecated. Instead use \code{ds_cvar()}.
#' @examples
#' ds_cvar(mtcars$mpg)
#' @export
#'
ds_cvar <- function(x, na.rm = FALSE) {

  if (!is.numeric(x)) {
    stop("x must be numeric")
  }

  if (na.rm) {
    x <- stats::na.omit(x)
  }

  (stats::sd(x) / mean(x)) * 100

}

#' @export
#' @rdname ds_cvar
#' @usage NULL
#'
stat_cvar <- function(x) {
  .Deprecated("ds_cvar()")
  ds_cvar(x)
}

#' @title Corrected Sum of Squares
#' @description Compute the corrected sum of squares
#' @param x a numeric vector containing the values whose mode is to be computed
#' @param na.rm a logical value indicating whether NA values should be stripped before the computation proceeds.
#' @details Any NA values are stripped from \code{x} before computation
#' takes place.
#' @return Corrected sum of squares of \code{x}
#' @section  Deprecated Function:
#' \code{stat_css()} has been deprecated. Instead use \code{ds_css()}.
#' @examples
#' ds_css(mtcars$mpg)
#' @references \href{http://www.itl.nist.gov/div898/handbook/prc/section4/prc421.htm}{NIST/SEMATECH e-Handbook of Statistical Methods}
#' @export
#'
ds_css <- function(x, na.rm = FALSE) {

  if (!is.numeric(x)) {
    stop("x must be numeric")
  }

  if (na.rm) {
    x <- stats::na.omit(x)
  }

  y <- mean(x)

  x %>%
    magrittr::subtract(y) %>%
    magrittr::raise_to_power(2) %>%
    sum()

}

#' @export
#' @rdname ds_css
#' @usage NULL
#'
stat_css <- function(x) {
  .Deprecated("ds_css()")
  ds_css(x)
}


#' @title Index Values
#' @description Returns index of values.
#' @param data a numeric vector
#' @param values a numeric vector containing the values whose index is returned
#' @details Any NA values are stripped from \code{data} and \code{values} before
#' computation takes place.
#' @return Index of the \code{values} in \code{data}. In case, \code{data} does
#' not contain \code{index}, \code{NULL} is returned.
#' @section Deprecated Function:
#' \code{rindex()} has been deprecated. Instead use \code{ds_rindex()}.
#' @examples
#' ds_rindex(mtcars$mpg, 21)
#' ds_rindex(mtcars$mpg, 22)
#' @export
#'
ds_rindex <- function(data, values) {
  if (!is.numeric(data)) {
    stop("data must be numeric")
  }

  if (!is.numeric(values)) {
    stop("values must be numeric")
  }

  data   <- stats::na.omit(data)
  values <- stats::na.omit(values)
  out    <- c()
  
  for (i in seq_along(values)) {
    k <- return_pos(data, values[i])
    out <- c(out, k)
  }
  
  return(unique(out))

}

#' @export
#' @rdname ds_rindex
#' @usage NULL
#'
rindex <- function(data, values) {
  .Deprecated("ds_rindex()")
  ds_rindex(data, values)
}
