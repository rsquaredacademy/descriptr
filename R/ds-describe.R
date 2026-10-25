#' Measures of location
#'
#' Returns the measures of location such as mean, median & mode.
#'
#' @param data A \code{data.frame} or \code{tibble} or numeric vector.
#' @param ... Column(s) in \code{data} or numeric vectors.
#' @param trim The fraction of values to be trimmed before computing the mean.
#' @param decimals An option to specify the exact number of decimal places to use. The default number of decimal places is 2.
#'
#' @examples
#' # single column
#' ds_measures_location(mtcarz, mpg)
#'
#' # multiple columns
#' ds_measures_location(mtcarz, mpg, disp)
#'
#' # all columns
#' ds_measures_location(mtcarz)
#'
#' # vector
#' ds_measures_location(mtcarz$mpg)
#'
#' # vectors of different length
#' disp <- mtcarz$disp[1:10]
#' ds_measures_location(mtcarz$mpg, disp)
#'
#' # decimal places
#' ds_measures_location(mtcarz, disp, hp, decimals = 3)
#'
#' @export
#'
ds_measures_location <- function(data, ..., trim = 0.05, decimals = 2) {

  if (is.data.frame(data)) {

    var <- vapply(substitute(...()), deparse, NA_character_)

    if (length(var) < 1) {
      data <- ds_num_cols(data)
    } else {
      check_numeric_cols(data, var)
      data <- data[var]
    }

    ds_loc_prep(data, trim = trim, decimals = decimals)

  } else if (is.numeric(data)) {

    vars  <- c(deparse(substitute(data)), vapply(substitute(...()), deparse, NA_character_))

    if (all(grepl("\\$", vars))) {
      vars <- unlist(lapply(strsplit(vars, "\\$"), `[[`, 2))
    }

    data  <- list(data, ...)
    dtype <- sapply(data, is.numeric)

    ds_loc_prep(data, vars, trim, decimals)

  } else {

    stop("data must be either numeric or a `data.frame`.", call. = FALSE)

  }

}

#' Measures of variation
#'
#' Returns the measures of location such as range, variance and standard
#'   deviation.
#'
#' @param data A \code{data.frame} or \code{tibble}.
#' @param ... Column(s) in \code{data}.
#' @param decimals An option to specify the exact number of decimal places to use. The default number of decimal places is 2.
#'
#' @examples
#' # single column
#' ds_measures_variation(mtcarz, mpg)
#'
#' # multiple columns
#' ds_measures_variation(mtcarz, mpg, disp)
#'
#' # all columns
#' ds_measures_variation(mtcarz)
#'
#' # vector
#' ds_measures_variation(mtcarz$mpg)
#'
#' # vectors of different length
#' disp <- mtcarz$disp[1:10]
#' ds_measures_variation(mtcarz$mpg, disp)
#'
#' # decimal places
#' ds_measures_variation(mtcarz, disp, hp, decimals = 3)
#'
#' @export
#'
ds_measures_variation <- function(data, ..., decimals = 2) {

  if (is.data.frame(data)) {

    var <- rlang::quos(...)

    if (length(var) < 1) {
      is_num <- sapply(data, is.numeric)
      if (!any(is_num == TRUE)) {
        stop("Data has no continuous variables.", call. = FALSE)
      }
      data <- data[is_num]
    } else {
      data %<>%
        dplyr::select(!!! var)
      check_numeric_cols(data, names(data))
    }

    data %>%
      tidyr::gather(var, values) %>%
      dplyr::group_by(var) %>%
      dplyr::summarise_all(list(n         = ~ sum(!is.na(.)),
                                range     = ds_range,
                                iqr       = ~ stats::IQR(., na.rm = TRUE),
                                variance  = ~ stats::var(., na.rm = TRUE),
                                sd        = ~ stats::sd(., na.rm = TRUE),
                                coeff_var = ds_cvar,
                                std_error = ~ ds_std_error(stats::na.omit(.)))) %>%
      dplyr::mutate(dplyr::across(dplyr::where(is.double), ~ round(., decimals))) %>%
      as.data.frame()

  } else if (is.numeric(data)) {

    vars  <- c(deparse(substitute(data)),
               vapply(substitute(...()), deparse, NA_character_))

    if (all(grepl("\\$", vars))) {
      vars <- unlist(lapply(strsplit(vars, "\\$"), `[[`, 2))
    }

    data <- list(data, ...)

    data.frame(variable  = vars,
               n         = sapply(data, length),
               range     = round(sapply(data, ds_range), decimals),
               iqr       = round(sapply(data, stats::IQR), decimals),
               variance  = round(sapply(data, stats::var), decimals),
               sd        = round(sapply(data, stats::sd), decimals),
               coeff_var = round(sapply(data, ds_cvar), decimals),
               std_error = round(sapply(data, ds_std_error), decimals))

  } else {

    stop("data must be either numeric or a `data.frame`.", call. = FALSE)

  }


}

#' Measures of symmetry
#'
#' Returns the measures of symmetry such as skewness and kurtosis.
#'
#' @param data A \code{data.frame} or \code{tibble}.
#' @param ... Column(s) in \code{data}.
#' @param decimals An option to specify the exact number of decimal places to use. The default number of decimal places is 2.
#'
#' @examples
#' # single column
#' ds_measures_symmetry(mtcarz, mpg)
#'
#' # multiple columns
#' ds_measures_symmetry(mtcarz, mpg, disp)
#'
#' # all columns
#' ds_measures_symmetry(mtcarz)
#'
#' # vector
#' ds_measures_symmetry(mtcarz$mpg)
#'
#' # vectors of different length
#' disp <- mtcarz$disp[1:10]
#' ds_measures_symmetry(mtcarz$mpg, disp)
#'
#' # decimal places
#' ds_measures_symmetry(mtcarz, disp, hp, decimals = 3)
#'
#' @export
#'
ds_measures_symmetry <- function(data, ..., decimals = 2) {

  if (is.data.frame(data)) {

    var <- rlang::quos(...)

    if (length(var) < 1) {
      is_num <- sapply(data, is.numeric)
      if (!any(is_num == TRUE)) {
        stop("Data has no continuous variables.", call. = FALSE)
      }
      data <- data[is_num]
    } else {
      data %<>%
        dplyr::select(!!! var)
      check_numeric_cols(data, names(data))
    }

    data %>%
      tidyr::gather(var, values) %>%
      dplyr::group_by(var) %>%
      dplyr::summarise_all(
        list(
          n        = ~ sum(!is.na(.)),
          skewness = ds_skewness,
          kurtosis = ds_kurtosis)
      ) %>%
      dplyr::mutate(dplyr::across(dplyr::where(is.double), ~ round(., decimals))) %>%
      as.data.frame()

  } else if (is.numeric(data)) {

    vars  <- c(deparse(substitute(data)),
               vapply(substitute(...()), deparse, NA_character_))

    if (all(grepl("\\$", vars))) {
      vars <- unlist(lapply(strsplit(vars, "\\$"), `[[`, 2))
    }

    data <- list(data, ...)

    data.frame(var      = vars,
               n        = sapply(data, length),
               skewness = round(sapply(data, ds_skewness), decimals),
               kurtosis = round(sapply(data, ds_kurtosis), decimals))

  } else {

    stop("data must be either numeric or a `data.frame`.", call. = FALSE)

  }


}


#' Percentiles
#'
#' Returns the percentiles
#'
#' @param data A \code{data.frame} or \code{tibble}.
#' @param ... Column(s) in \code{data}.
#' @param decimals An option to specify the exact number of decimal places to use. The default number of decimal places is 2.
#'
#' @examples
#' # single column
#' ds_percentiles(mtcarz, mpg)
#'
#' # multiple columns
#' ds_percentiles(mtcarz, mpg, disp)
#'
#' # all columns
#' ds_percentiles(mtcarz)
#'
#' # vector
#' ds_percentiles(mtcarz$mpg)
#'
#' # vectors of different length
#' disp <- mtcarz$disp[1:10]
#' ds_percentiles(mtcarz$mpg, disp)
#'
#' # decimal places
#' ds_percentiles(mtcarz, disp, hp, decimals = 3)
#'
#' @export
#'
ds_percentiles <- function(data, ..., decimals = 2) {

  if (is.data.frame(data)) {

    var <- rlang::quos(...)

    if (length(var) < 1) {
      is_num <- sapply(data, is.numeric)
      if (!any(is_num == TRUE)) {
        stop("Data has no continuous variables.", call. = FALSE)
      }
      data <- data[is_num]
    } else {
      data %<>%
        dplyr::select(!!! var)
      check_numeric_cols(data, names(data))
    }

    data %>%
      tidyr::gather(var, values) %>%
      dplyr::group_by(var) %>%
      dplyr::summarise_all(
        list(n      = ~ sum(!is.na(.)),
             min    = ~ num_or_na(., min),
             per_1  = ~ quantile(., 0.01, na.rm = TRUE),
             per_5  = ~ quantile(., 0.05, na.rm = TRUE),
             per_10 = ~ quantile(., 0.10, na.rm = TRUE),
             q1     = ~ quantile(., 0.25, na.rm = TRUE),
             median = ~ median(., na.rm = TRUE),
             q3     = ~ quantile(., 0.75, na.rm = TRUE),
             per_90 = ~ quantile(., 0.90, na.rm = TRUE),
             per_95 = ~ quantile(., 0.95, na.rm = TRUE),
             per_99 = ~ quantile(., 0.99, na.rm = TRUE),
             max    = ~ num_or_na(., max))
      ) %>%
      dplyr::mutate(dplyr::across(dplyr::where(is.double), ~ round(., decimals))) %>%
      as.data.frame()

  } else if (is.numeric(data)) {

    vars  <- c(deparse(substitute(data)),
               vapply(substitute(...()), deparse, NA_character_))

    if (all(grepl("\\$", vars))) {
      vars <- unlist(lapply(strsplit(vars, "\\$"), `[[`, 2))
    }

    data <- list(data, ...)

    data.frame(var    = vars,
               n      = sapply(data, length),
               min    = round(sapply(data, min), decimals),
               per_1  = round(sapply(data, quantile, 0.01), decimals),
               per_5  = round(sapply(data, quantile, 0.05), decimals),
               per_10 = round(sapply(data, quantile, 0.10), decimals),
               q1     = round(sapply(data, quantile, 0.25), decimals),
               median = round(sapply(data, median), decimals),
               q3     = round(sapply(data, quantile, 0.75), decimals),
               per_90 = round(sapply(data, quantile, 0.90), decimals),
               per_95 = round(sapply(data, quantile, 0.95), decimals),
               per_99 = round(sapply(data, quantile, 0.99), decimals),
               max    = round(sapply(data, max), decimals))

  } else {

    stop("data must be either numeric or a `data.frame`.", call. = FALSE)

  }


}

#' Extreme observations
#'
#' Returns the most extreme observations and their original row indices.
#'
#' Missing values are removed before the \code{k = min(5, n)} lowest and
#' highest observations are selected. The row indices refer to the data passed
#' in, and ties are reported on distinct rows. \code{decimals} applies to both
#' vector and \code{data.frame} input.
#'
#' @param data A numeric vector or \code{data.frame} or \code{tibble}.
#' @param col Column in \code{data}.
#' @param decimals An option to specify the exact number of decimal places to use. The default number of decimal places is 2.
#'
#' @examples
#' # data.frame
#' ds_extreme_obs(mtcarz, mpg)
#'
#' # vector
#' ds_extreme_obs(mtcarz$mpg)
#'
#' # decimal places
#' ds_extreme_obs(mtcarz$mpg, decimals = 3)
#'
#' @export
#'
ds_extreme_obs <- function(data, col, decimals = 2) {

  if (is.data.frame(data)) {

    var <- rlang::enquo(col)
    var_name <- deparse(substitute(col))
    check_numeric(data, !! var, var_name)
    x <- dplyr::pull(data, !! var)

  } else if (is.numeric(data)) {

    x <- data

  } else {

    stop("data must be either a numeric vector or a `data.frame`.")

  }

  pairs <- sort_pairs(x)
  n     <- length(pairs$value)

  if (n == 0) {
    return(data.frame(type = character(0),
                      value = numeric(0),
                      index = integer(0)))
  }

  k       <- min(5, n)
  low_k   <- seq_len(k)
  high_k  <- rev(seq.int(n - k + 1L, n))
  result  <- data.frame(type  = c(rep("high", k), rep("low", k)),
                        value = round(c(pairs$value[high_k],
                                        pairs$value[low_k]), decimals),
                        index = c(pairs$index[high_k],
                                  pairs$index[low_k]))

  return(result)

}

#' @import magrittr
#' @title Tail Observations
#' @description Returns the n highest/lowest observations from a numeric vector.
#' @param data a numeric vector
#' @param n number of observations to be returned
#' @param type if \code{low}, the \code{n} lowest observations are returned, else the highest \code{n} observations are returned.
#' @param decimals An option to specify the exact number of decimal places to use. The default number of decimal places is 2.
#' @details Any NA values are stripped from \code{data} before computation takes place.
#' @return \code{n} highest/lowest observations from \code{data}
#' @examples
#' # 5 lowest observations
#' ds_tailobs(mtcarz$mpg, 5)
#'
#' # 5 highest observations
#' ds_tailobs(mtcarz$mpg, 5, type = "high")
#'
#' # specify decimal places to display
#' ds_tailobs(mtcarz$mpg, 5, decimals = 3)
#'
#' @export
#' @seealso \code{\link[dplyr]{top_n}}
#'
ds_tailobs <- function(data, n, type = c("low", "high"), decimals = 2) {

  if (!is.numeric(data)) {
    stop("data must be numeric", call. = FALSE)
  }

  if (!is.numeric(n)) {
    stop("n must be numeric", call. = FALSE)
  }

  if (n > sum(!is.na(data))) {
    stop("n must be less than or equal to the number of non-missing observations", call. = FALSE)
  }

  method <- match.arg(type)

  if (method == "low") {
    result <-
      data %>%
      na.omit() %>%
      sort() %>%
      `[`(1:n) %>%
      round(decimals)
  } else {
    result <-
      data %>%
      na.omit() %>%
      sort(decreasing = TRUE) %>%
      `[`(1:n) %>%
      round(decimals)
  }

  return(result)

}


#' @title Geometric Mean
#' @description Computes the geometric mean
#' @param data A numeric vector or \code{data.frame}.
#' @param x Column in \code{data}.
#' @details Any \code{NA} values are stripped before computation. The mean is
#' computed on the log scale, so large values or long vectors do not overflow.
#' If any value is less than or equal to zero, a warning is issued and
#' \code{NA} is returned.
#' @examples
#' # vector
#' ds_gmean(mtcars$mpg)
#'
#' # data.frame
#' ds_gmean(mtcars, mpg)
#'
#' @export
#' @seealso \code{\link{ds_hmean}} \code{\link[base]{mean}}
#'
ds_gmean <- function(data, x = NULL) {

  y <- deparse(substitute(x))

  if (y == "NULL") {
    z <- data
  } else {
    z <- data[[y]]
  }

  if (!is.numeric(z)) {
    z_class <- class(z)
    stop(paste0("Geometric mean can be calculated only for numeric data. The variable you have selected is of type ", z_class, "."), call. = FALSE)
  }

  z <- na.omit(z)

  if (length(z) == 0) {
    return(NA_real_)
  }

  if (min(z) <= 0) {
    warning("Geometric mean requires strictly positive", call. = FALSE)
    return(NA_real_)
  }

  exp(mean(log(z)))

}

#' @title Harmonic Mean
#' @description Computes the harmonic mean
#' @param data A numeric vector or \code{data.frame}.
#' @param x Column in \code{data}.
#' @details Any \code{NA} values are stripped before computation. If any value
#' is less than or equal to zero, a warning is issued and \code{NA} is
#' returned.
#' @examples
#' # vector
#' ds_hmean(mtcars$mpg)
#'
#' # data.frame
#' ds_hmean(mtcars, mpg)
#'
#' @export
#' @seealso \code{\link{ds_gmean}} \code{\link[base]{mean}}
#'
ds_hmean <- function(data, x = NULL) {

  y <- deparse(substitute(x))

  if (y == "NULL") {
    z <- data
  } else {
    z <- data[[y]]
  }

  if (!is.numeric(z)) {
    z_class <- class(z)
    stop(paste0("Harmonic mean can be calculated only for numeric data. The variable you have selected is ", z_class, "."), call. = FALSE)
  }

  z <- na.omit(z)

  if (length(z) == 0) {
    return(NA_real_)
  }

  if (min(z) <= 0) {
    warning("Harmonic mean requires strictly positive non-zero values", call. = FALSE)
    return(NA_real_)
  }

  length(z) / sum(1 / z)

}

#' @title Mode
#' @description Compute the sample mode
#' @param data A numeric vector or \code{data.frame}.
#' @param x Column in \code{data}.
#' @details Any NA values are stripped from \code{x} before computation
#' takes place.
#' @return Mode of \code{x}
#' @examples
#' # vector
#' ds_mode(mtcars$mpg)
#'
#' # data.frame
#' ds_mode(mtcars, mpg)
#'
#' @seealso \code{\link[base]{mean}} \code{\link[stats]{median}}
#' @export
#'
ds_mode <- function(data, x = NULL) {

  y <- deparse(substitute(x))

  if (y == "NULL") {
    z <- data
  } else {
    z <- data[[y]]
  }

  if (!is.numeric(z)) {
    z_class <- class(z)
    stop(paste0("Mode can be calculated only for numeric data. The variable you have selected is of type ", z_class, "."), call. = FALSE)
  }

  z <- na.omit(z)

  if (length(z) == 0L) {
    return(NA_real_)
  }

  Freq <- NULL

  z %>%
    table() %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    dplyr::arrange(dplyr::desc(Freq)) %>%
    dplyr::filter(Freq == max(Freq)) %>%
    dplyr::select(dplyr::contains(".")) %>%
    unlist() %>%
    as.numeric() %>%
    min()

}


#' @title Range
#' @description Compute the range of a numeric vector
#' @param data A numeric vector or \code{data.frame}.
#' @param x Column in \code{data}.
#' @return Range of \code{x}
#' @examples
#' # vector
#' ds_range(mtcars$mpg)
#'
#' # data.frame
#' ds_range(mtcars, mpg)
#'
#' @seealso \code{\link[base]{range}}
#' @export
#'
ds_range <- function(data, x = NULL) {

  y <- deparse(substitute(x))

  if (y == "NULL") {
    z <- data
  } else {
    z <- data[[y]]
  }

  if (!is.numeric(z)) {
    z_class <- class(z)
    stop(paste0("Range can be calculated only for numeric data. The variable you have selected is ", z_class, "."), call. = FALSE)
  }

  z <- na.omit(z)
  if (length(z) == 0L) {
    return(NA_real_)
  }
  max(z) - min(z)

}



#' @title Kurtosis
#' @description Compute the kurtosis of a probability distribution.
#' @param data A numeric vector or \code{data.frame}.
#' @param x Column in \code{data}.
#' @examples
#' # vector
#' ds_kurtosis(mtcars$mpg)
#'
#' # data.frame
#' ds_kurtosis(mtcars, mpg)
#'
#' @seealso \code{ds_skewness}
#' @references Sheskin, D.J. (2000) Handbook of Parametric and Nonparametric Statistical Procedures, Second Edition. Boca Raton, Florida: Chapman & Hall/CRC.
#' @export
#'
ds_kurtosis <- function(data, x = NULL) {

  y <- deparse(substitute(x))

  if (y == "NULL") {
    z <- data
  } else {
    z <- data[[y]]
  }

  if (!is.numeric(z)) {
    z_class <- class(z)
    stop(paste0("Kurtosis is calculated only for numeric data. The variable you have selected is of type ", z_class, "."), call. = FALSE)
  }

  z <- na.omit(z)
  if (length(z) == 0L) {
    return(NA_real_)
  }
  n <- length(z)
  summation <- sums(z, 4)
  part1 <- (n * (n + 1)) / ((n - 1) * (n - 2) * (n - 3))
  part2 <- (3 * (n - 1) ^ 2) / ((n - 2) * (n - 3))
  (part1 * summation) - part2

}

#' @title Skewness
#' @description Compute the skewness of a probability distribution.
#' @param data A numeric vector or \code{data.frame}.
#' @param x Column in \code{data}.
#' @examples
#' # vector
#' ds_skewness(mtcars$mpg)
#'
#' # data.frame
#' ds_skewness(mtcars, mpg)
#'
#' @seealso \code{kurtosis}
#' @references Sheskin, D.J. (2000) Handbook of Parametric and Nonparametric Statistical Procedures, Second Edition. Boca Raton, Florida: Chapman & Hall/CRC.
#' @export
#'
ds_skewness <- function(data, x = NULL) {

  y <- deparse(substitute(x))

  if (y == "NULL") {
    z <- data
  } else {
    z <- data[[y]]
  }

  if (!is.numeric(z)) {
    z_class <- class(z)
    stop(paste0("Skewness is calculated only for numeric data. The variable you have selected is of type ", z_class, "."), call. = FALSE)
  }

  z <- na.omit(z)
  if (length(z) == 0L) {
    return(NA_real_)
  }
  n <- length(z)
  summation <- sums(z, 3)
  (n / ((n - 1) * (n - 2))) * summation

}

#' @title Mean Absolute Deviation
#' @description Compute the mean absolute deviation about the mean
#' @param data A numeric vector or \code{data.frame}.
#' @param x Column in \code{data}.
#' @details The \code{ds_mdev} function computes the mean absolute deviation
#' about the mean. It is different from \code{mad} in \code{stats} package as
#' the statistic used to compute the deviations is not \code{median} but
#' \code{mean}. Any NA values are stripped from \code{x} before computation
#' takes place
#' @examples
#' # vector
#' ds_mdev(mtcars$mpg)
#'
#' # data.frame
#' ds_mdev(mtcars, mpg)
#'
#' @seealso \code{\link[stats]{mad}}
#' @export
#'
ds_mdev <- function(data, x = NULL) {

  y <- deparse(substitute(x))

  if (y == "NULL") {
    z <- data
  } else {
    z <- data[[y]]
  }

  if (!is.numeric(z)) {
    z_class <- class(z)
    stop(paste0("Mean absolute deviation is calculated only for numeric data. The variable you have selected is of type ", z_class, "."), call. = FALSE)
  }

  z <- na.omit(z)
  m <- mean(z)
  sum(sapply(z, md_helper, m)) / length(z)

}


#' @title Coefficient of Variation
#' @description Compute the coefficient of variation
#' @param data A numeric vector or \code{data.frame}.
#' @param x Column in \code{data}.
#' @examples
#' # vector
#' ds_cvar(mtcars$mpg)
#'
#' # data.frame
#' ds_cvar(mtcars, mpg)
#'
#' @export
#'
ds_cvar <- function(data, x = NULL) {

  y <- deparse(substitute(x))

  if (y == "NULL") {
    z <- data
  } else {
    z <- data[[y]]
  }

  if (!is.numeric(z)) {
    z_class <- class(z)
    stop(paste0("Coefficient of variation is calculated only for numeric data. The variable you have selected is of type ", z_class, "."), call. = FALSE)
  }

  z <- na.omit(z)

  if (length(z) == 0L) {
    return(NA_real_)
  }

  (sd(z) / mean(z)) * 100

}

#' @title Corrected Sum of Squares
#' @description Compute the corrected sum of squares
#' @param data A numeric vector or \code{data.frame}.
#' @param x Column in \code{data}.
#' @examples
#' # vector
#' ds_css(mtcars$mpg)
#'
#' # data.frame
#' ds_css(mtcars, mpg)
#'
#' @export
#'
ds_css <- function(data, x = NULL) {

  y <- deparse(substitute(x))

  if (y == "NULL") {
    z <- data
  } else {
    z <- data[[y]]
  }

  if (!is.numeric(z)) {
    z_class <- class(z)
    stop(paste0("Corrected sum of squares can be calculated only for numeric data. The variable you have selected is of type ", z_class, "."), call. = FALSE)
  }

  z <- na.omit(z)

  sum((z - mean(z)) ^ 2)

}

#' @title Index Values
#' @description Returns the original positions of values in a vector.
#' @param data a numeric vector
#' @param values a numeric vector containing the values whose index is returned
#' @return Original positions of the \code{values} in \code{data}. Missing
#' values are ignored, and all matching positions are returned. In case
#' \code{data} does not contain a value, \code{NULL} is returned.
#' @details Positions refer to the vector passed in; missing values are not
#' removed before the indices are computed.
#' @examples
#' # returns index of 21
#' ds_rindex(mtcars$mpg, 21)
#'
#' # returns NULL
#' ds_rindex(mtcars$mpg, 22)
#'
#' @export
#'
ds_rindex <- function(data, values) {

  if (!is.numeric(data)) {
    stop("Data must be numeric.", call. = FALSE)
  }

  if (!is.numeric(values)) {
    stop("Values must be numeric.", call. = FALSE)
  }

  values <- na.omit(values)
  out    <- unlist(lapply(values, function(value) which(data == value)))

  if (length(out) == 0) {
    return(NULL)
  }

  unique(out)

}

