tailobs <- function(data, n, type = c('low', 'high')) {

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
    data <- sort(data)
    result <- data[1:n]
  } else {
    data <- sort(data, decreasing = TRUE)
    result <- data[1:n]
  }

  return(result)

}

#' @title Geometric Mean
#' @description Compute the geometric mean
#' @param x a numeric vector containing the values whose geometric mean is to be
#' computed
#' @param ... further arguments passed to or from other methods
#' @return Returns the geometric mean of \code{x}
#' @examples
#' gmean(mtcars$mpg)
#' @export
#' @seealso \code{\link{hmean}}
#'
gmean <- function(x, ...) {

    if(!is.numeric(x)) {
      stop('x must be numeric')
    }

    prod(x) ^ (1 / length(x))
}


#' @title Harmonic Mean
#' @description Compute the harmonic mean
#' @param x a numeric vector containing the values whose harmonic mean is to be
#' computed
#' @param ... further arguments passed to or from other methods
#' @return Returns the harmonic mean of \code{x}
#' @examples
#' hmean(mtcars$mpg)
#' @export
#' @seealso \code{\link{gmean}}
#'
hmean <- function(x, ...) {

    if(!is.numeric(x)) {
      stop('x must be numeric')
    }

    length(x) / sum(sapply(x, div_by))
}

#' @title Mode
#' @description Compute the sample mode
#' @param x a numeric vector containing the values whose mode is to be computed
#' @details Any NA values are stripped from \code{x} before computation
#' takes place.
#' @return Mode of \code{x}
#' @examples
#' stat_mode(mtcars$mpg)
#' stat_mode(mtcars$cyl)
#' @seealso \code{\link[stats]{mean}} \code{\link[stats]{median}}
#' @export
#'
stat_mode <- function(x) {

    if(!is.numeric(x)) {
      stop('x must be numeric')
    }

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

#' @title Range
#' @description Compute the range of a numeric vector
#' @param x a numeric vector
#' @return Range of \code{x}
#' @examples
#' stat_range(mtcars$mpg)
#' @seealso \code{\link[stats]{range}}
#' @export
#'
stat_range <- function(x) {

    if(!is.numeric(x)) {
      stop('data must be numeric')
    }

    out <- x %>%
      na.omit() %>%
      range() %>%
      diff()

    return(out)
}

kurtosis <- function(x) {

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

skewness <- function(x) {

    if(!is.numeric(x)) {
      stop('x must be numeric')
    }
    n <- length(x)
    summation <- sums(x, 3)
    result <- (n / ((n -1) * (n -2))) * summation
    return(result)
}

#' @title Mean Absolute Deviation
#' @description Compute the mean absolute deviation about the mean
#' @param a numeric vector
#' @details The \code{stat_mdev} function computes the mean absolute deviation
#' about the mean. It is different from \code{mad} in \code{stats} package as
#' the statistic used to compute the deviations is not \code{median} but
#' \code{mean}. Any NA values are stripped from \code{x} before computation
#' takes place
#' @return Mean absolute deviation of \code{x}
#' @examples
#' stat_mdev(mtcars$mpg)
#' @seealso \code{\link[stats]{mad}}
#' @export
#'
stat_mdev <- function(x) {

    if(!is.numeric(x)) {
      stop('x must be numeric')
    }
    x <- na.omit(x)
    m <- mean(x)
    result <- sum(sapply(x, md_helper, m)) / length(x)
    return(result)
}

#' @title Coefficient of Variation
#' @description Compute the coefficient of variation
#' @param a numeric vector containing the values whose mode is to be computed
#' @details Any NA values are stripped from \code{x} before computation
#' takes place.
#' @examples
#' stat_cvar(mtcars$mpg)
#' @seealso \code{\link[stats]{var}} \code{\link[stats]{sd}}
#' @references \href{http://www.ats.ucla.edu/stat/mult_pkg/faq/general/coefficient_of_variation.htm}{FAQ: What is the coefficient of variation?  UCLA: Statistical Consulting Group.}
#' @export
#'
stat_cvar <- function(x) {

    if(!is.numeric(x)) {
      stop('x must be numeric')
    }
    (sd(x) / mean(x)) * 100

}


#' @title Corrected Sum of Squares
#' @description Compute the corrected sum of squares
#' @param a numeric vector containing the values whose mode is to be computed
#' @details Any NA values are stripped from \code{x} before computation
#' takes place.
#' @return Corrected sum of squares of \code{x}
#' @examples
#' stat_css(mtcars$mpg)
#' @references \href{http://www.itl.nist.gov/div898/handbook/prc/section4/prc421.htm}{NIST/SEMATECH e-Handbook of Statistical Methods}
#' @export
#'
stat_css <- function(x) {

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


rindex <- function(data, values) {

    if(!is.numeric(data)) {
      stop('data must be numeric')
    }

    if(!is.numeric(values)) {
      stop('values must be numeric')
    }

    out <- c()
    for (i in seq_along(values)) {
        k <- return_pos(data, values[i])
        out <- c(out, k)
    }
    return(unique(out))
}
