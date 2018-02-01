#' @importFrom magrittr %>%
#' @importFrom stats na.omit
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
ds_tailobs <- function(data, n, type = c('low', 'high')) {

  if(!is.numeric(data)) {
    stop('data must be numeric')
  }

  if(!is.numeric(n)) {
    stop('n must be numeric')
  }

  if(n > length(data)) {
    stop('n must be less than the length of data')
  }

  method <- match.arg(type)

  if (method == 'low') {
    result <- data %>%
      na.omit() %>%
      sort() %>%
      `[`(1:n)
  } else {
    result <- data %>%
      na.omit() %>%
      sort(decreasing = TRUE) %>%
      `[`(1:n)
  }

  return(result)

}

#' @export
#' @rdname ds_tailobs
#' @usage NULL
#'
tailobs <- function(data, n, type = c('low', 'high')) {

  .Deprecated("ds_tailobs()")
  ds_tailobs(data, n, type)

}

#' @title Geometric Mean
#' @description Compute the geometric mean
#' @param x a numeric vector containing the values whose geometric mean is to be
#' computed
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
ds_gmean <- function(x, ...) {

  if(!is.numeric(x)) {
    stop('x must be numeric')
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
ds_hmean <- function(x, ...) {

    if(!is.numeric(x)) {
      stop('x must be numeric')
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




#' @importFrom dplyr arrange desc filter select contains
#' @title Mode
#' @description Compute the sample mode
#' @param x a numeric vector containing the values whose mode is to be computed
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
ds_mode <- function(x) {

    if(!is.numeric(x)) {
      stop('x must be numeric')
    }

    Freq <- NULL
    mode <- x %>%
            table() %>%
            as.data.frame(stringsAsFactors = FALSE) %>%
            arrange(desc(Freq)) %>%
            filter(Freq == max(Freq)) %>%
            select(contains(".")) %>%
            unlist() %>%
            as.numeric() %>%
            min()

    return(mode)
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
#' @return Range of \code{x}
#' @section Deprecated Function:
#' \code{stat_range()} has been deprecated. Instead use \code{ds_range()}.
#' @examples
#' ds_range(mtcars$mpg)
#' @seealso \code{\link[base]{range}}
#' @export
#'
ds_range <- function(x) {

    if(!is.numeric(x)) {
      stop('data must be numeric')
    }

    out <- x %>%
      na.omit() %>%
      range() %>%
      diff()

    return(out)
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
ds_kurtosis <- function(x) {

    if(!is.numeric(x)) {
      stop('x must be numeric')
    }
    n <- length(x)
    summation <- sums(x, 4)
    part1 <- (n * (n + 1)) / ((n - 1) * (n - 2) * (n - 3))
    part2 <- (3 * (n - 1) ^ 2) / ((n - 2) * (n -3))
    result <- (part1 * summation) - part2
    return(result)
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
ds_skewness <- function(x) {

    if(!is.numeric(x)) {
      stop('x must be numeric')
    }
    n <- length(x)
    summation <- sums(x, 3)
    result <- (n / ((n -1) * (n -2))) * summation
    return(result)
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
ds_mdev <- function(x) {

    if(!is.numeric(x)) {
      stop('x must be numeric')
    }
    x <- na.omit(x)
    m <- mean(x)
    result <- sum(sapply(x, md_helper, m)) / length(x)
    return(result)
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
#' @details Any NA values are stripped from \code{x} before computation
#' takes place.
#' @section Deprecated Function:
#' \code{stat_cvar()} has been deprecated. Instead use \code{ds_cvar()}.
#' @examples
#' ds_cvar(mtcars$mpg)
#' @seealso \code{\link[stats]{var}} \code{\link[stats]{sd}}
#' @export
#'
ds_cvar <- function(x) {

    if(!is.numeric(x)) {
      stop('x must be numeric')
    }
    (sd(x) / mean(x)) * 100

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
ds_css <- function(x) {

    if(!is.numeric(x)) {
      stop('x must be numeric')
    }

    y <- mean(x, na.rm = TRUE)
    x %>%
      na.omit() %>%
      `-`(y) %>%
      `^`(2) %>%
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

    if(!is.numeric(data)) {
      stop('data must be numeric')
    }

    if(!is.numeric(values)) {
      stop('values must be numeric')
    }

    data <- na.omit(data)
    values <- na.omit(values)
    out <- c()
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
