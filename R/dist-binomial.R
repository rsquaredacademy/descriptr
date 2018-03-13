#' Visualize binomial distribution
#'
#' Visualize how changes in number of trials and the probability of
#' success affect the shape of the binomial distribution. Compute & visualize
#' probability from a given quantile and quantiles out of given probability.
#'
#' @param n Number of trials.
#' @param p Aggregate probability.
#' @param s Number of success.
#' @param tp Probability of success in a trial.
#' @param type Lower/upper/exact/interval.
#'
#' @return A list containing the following components:
#'
#' \item{avg}{Mean of the binomial distribution,}
#' \item{stdev}{Standard deviation of the binomial distribution.}
#' \item{prob}{Probability of success.}
#'
#' @section Deprecated functions:
#' \code{binom_plot()}, \code{binom_prob()}, \code{binom_perc()} have been
#' deprecated. Instead use \code{dist_binom_plot()},
#' \code{dist_binom_prob()} and \code{dist_binom_perc()}.
#'
#' @importFrom stats dbinom pbinom qbinom
#' @importFrom graphics axis mtext
#'
#' @examples
#' # visualize binomial distribution
#' dist_binom_plot(10, 0.3)
#'
#' # visualize probability from a given quantile
#' dist_binom_prob(10, 0.3, 4, type = 'exact')
#' dist_binom_prob(10, 0.3, 4, type = 'lower')
#' dist_binom_prob(10, 0.3, 4, type = 'upper')
#' dist_binom_prob(10, 0.3, c(4, 6), type = 'interval')
#'
#' # visualize quantiles out of given probability
#' dist_binom_perc(10, 0.5, 0.05)
#' dist_binom_perc(10, 0.5, 0.05, "upper")
#'
#' @seealso \code{\link[stats]{Binomial}}
#'
#' @export
#'
dist_binom_plot <- function(n, p) {
  if (!is.numeric(n)) {
    stop("n must be numeric/integer")
  }

  if (!is.numeric(p)) {
    stop("p must be numeric")
  }

  if ((p < 0) | (p > 1)) {
    stop("p must be between 0 and 1")
  }

  n <- as.integer(n)
  bm <- round(n * p, 2)
  bsd <- round(sqrt(bm * (1 - p)), 2)

  x <- seq(0, n, 1)
  k <- barplot(
    dbinom(x, n, p),
    col = "blue",
    xlab = "No. of success",
    ylab = "Probability",
    ylim = c(0, max(dbinom(x, n, p)) + 0.2),
    main = paste("Binomial Distribution: n =", n, ", p =", p)
  )

  axis(1, at = (k), labels = (0:n))
  mtext(text = paste("Mean =", bm, ", Std. Dev. =", bsd), side = 3)

  result <- list(avg = bm, stdev = bsd)
  invisible(result)
}

#' @export
#' @rdname dist_binom_plot
#' @usage NULL
#'
binom_plot <- function(n, p) {
  .Deprecated("dist_binom_plot()")
  dist_binom_plot(n, p)
}


#' @rdname dist_binom_plot
#' @export
#'
dist_binom_prob <- function(n, p, s,
                            type = c("lower", "upper", "exact", "interval")) {
  method <- match.arg(type)

  if ((p < 0) | (p > 1)) {
    stop("p must be between 0 and 1")
  }

  if (!is.numeric(n)) {
    stop("n must be numeric/integer")
  }

  if (!is.numeric(p)) {
    stop("p must be numeric")
  }

  if (!is.numeric(s)) {
    stop("s must be numeric/integer")
  }

  if (method == "interval") {
    if (length(s) != 2) {
      stop("Please specify an interval for s")
    }
  }

  if (any(s > n)) {
    stop("s must be less than or equal to n")
  }

  n <- as.integer(n)
  s <- as.integer(s)

  bm <- round(n * p, 2)
  bsd <- round(sqrt(bm * (1 - p)), 2)

  x <- seq(0, n, 1)

  if (method == "lower") {
    k <- round(pbinom(s, n, p), 3)
    cols <- ifelse(cumsum(round(dbinom(x, n, p), 3)) <= k, "#0000CD", "#6495ED")
  } else if (method == "upper") {
    k <- round(1 - pbinom((s - 1), n, p), 3)
    cols <- ifelse(cumsum(round(dbinom(x, n, p), 3)) >= k, "#0000CD", "#6495ED")
  } else if (method == "exact") {
    k <- pbinom(s, n, p) - pbinom((s - 1), n, p)
    cols <- ifelse(round(dbinom(x, n, p), 5) == round(k, 5), "#0000CD", "#6495ED")
  } else {
    k1 <- pbinom((s[1] - 1), n, p)
    k2 <- pbinom(s[2], n, p)
    k <- pbinom(s[2], n, p) - pbinom((s[1] - 1), n, p)
    cols <- ifelse((round(cumsum(dbinom(x, n, p)), 6) > round(k1, 6) &
      round(cumsum(dbinom(x, n, p)), 6) <= round(k2, 6)), "#0000CD", "#6495ED")
  }


  bp <- barplot(
    dbinom(x, n, p),
    col = cols,
    xlab = "No. of success",
    ylab = "Probability",
    ylim = c(0, max(dbinom(x, n, p)) + 0.2),
    main = paste("Binomial Distribution: n =", n, ", p =", p),
    sub = paste("Mean =", bm, ", Std. Dev. =", bsd)
  )

  axis(1, at = (bp), labels = (0:n))

  if (method == "lower") {
    mtext(text = paste("P(X) <=", s, "=", round(k, 3)), side = 3)
  } else if (method == "upper") {
    mtext(text = paste("P(X) >=", s, "=", round(k, 3)), side = 3)
  } else if (method == "exact") {
    mtext(text = paste("P(X) =", s, "=", round(k, 3)), side = 3)
  } else {
    mtext(text = paste0("P(", s[1], " <= X <= ", s[2], ")", " = ", round(k, 3)), side = 3)
  }

  result <- list(prob = k, avg = bm, stdev = bsd)
  invisible(result)
}

#' @export
#' @rdname dist_binom_plot
#' @usage NULL
#'
binom_prob <- function(n, p) {
  .Deprecated("dist_binom_prob()")
  dist_binom_prob(n, p, s, type)
}



#' @rdname dist_binom_plot
#' @export
#'
dist_binom_perc <- function(n, p, tp, type = c("lower", "upper")) {
  if (!is.numeric(n)) {
    stop("n must be numeric/integer")
  }

  if (!is.numeric(p)) {
    stop("p must be numeric")
  }

  if (!is.numeric(tp)) {
    stop("tp must be numeric")
  }

  if ((p < 0) | (p > 1)) {
    stop("p must be between 0 and 1")
  }

  if ((tp < 0) | (tp > 0.5)) {
    stop("tp must be between 0 and 0.5")
  }

  n <- as.integer(n)

  method <- match.arg(type)

  x <- seq(0, n, 1)

  if (method == "lower") {
    k <- round(qbinom(tp, n, p), 3)
    cols <- ifelse(cumsum(dbinom(x, n, p)) <= pbinom(k, n, p), "#0000CD", "#6495ED")
  } else {
    k <- round(qbinom(tp, n, p, lower.tail = F), 3)
    cols <- ifelse(cumsum(dbinom(x, n, p)) > pbinom((k + 1), n, p), "#0000CD", "#6495ED")
  }


  bp <- barplot(
    dbinom(x, n, p),
    col = cols,
    xlab = "No. of success",
    ylab = "Probability",
    ylim = c(0, max(dbinom(x, n, p)) + 0.2),
    main = paste("Binomial Distribution: n =", n, ", p =", p)
  )

  axis(1, at = (bp), labels = (0:n))

  if (method == "lower") {
    mtext(text = paste0(
      "P(X <= ", k, ") <= ", tp, ", but P(X <= ", (k + 1),
      ") > ", tp
    ), side = 3)
  } else {
    mtext(text = paste0(
      "P(X >= ", (k + 1), ") <= ", tp, ", but P(X >= ", k,
      ") > ", tp
    ), side = 3)
  }
}


#' @export
#' @rdname dist_binom_plot
#' @usage NULL
#'
binom_perc <- function(n, p) {
  .Deprecated("dist_binom_perc()")
  dist_binom_perc(n, p, tp, type)
}
