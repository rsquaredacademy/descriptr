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

gmean <- function(x) {

    if(!is.numeric(x)) {
      stop('x must be numeric')
    }
    result <- (prod(x)) ^ (1 / length(x))
    return(result)
}

hmean <- function(x) {

    if(!is.numeric(x)) {
      stop('x must be numeric')
    }
    result <- length(x) / sum(sapply(x, divide_by))
    return(result)
}

stat_mode <- function(x) {

    if(!is.numeric(x)) {
      stop('x must be numeric')
    }
    y <- as.data.frame(table(x))
    y <- y[order(-y$Freq), ]
    mode <- y$x[which(y$Freq == max(y$Freq))]
    mode <- as.numeric(as.vector(mode))
    mode <- min(mode)
    return(mode)
}

stat_range <- function(data) {

    if(!is.numeric(data)) {
      stop('data must be numeric')
    }
    result <- diff(range(data))
    return(result)
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

stat_mdev <- function(x) {

    if(!is.numeric(x)) {
      stop('x must be numeric')
    }
    m <- mean(x)
    result <- sum(sapply(x, md_helper, m)) / length(x)
    return(result)
}

stat_cvar <- function(x) {

    if(!is.numeric(x)) {
      stop('x must be numeric')
    }
    result <- (sd(x) / mean(x)) * 100
    return(result)
}

stat_css <- function(x) {

    if(!is.numeric(x)) {
      stop('x must be numeric')
    }
    result <- sum(sapply(x, uss, mean(x)))
    return(round(result))
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
