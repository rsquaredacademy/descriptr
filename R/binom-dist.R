#' @importFrom stats dbinom pbinom qbinom
#' @importFrom graphics axis mtext
#' @title Visualize Binomial Distribution
#' @description Visualize how changes in number of trials and the probability of
#' success affect the shape of the binomial distribution. Compute/visualize
#' probability from a given quantile and quantiles out of given probability.
#' @param n number of trials
#' @param p aggregate probability
#' @param s number of success
#' @param tp the probability of success in a trial
#' @param type lower/upper/exact/interval
#' @return a list containing the following components:
#'
#' \item{avg}{mean of the binomial distribution}
#' \item{stdev}{standard deviation of the binomial distribution}
#' \item{prob}{probability of s success}
#' @examples
#' # visualize binomial distribution
#' binom_plot(10, 0.3)
#'
#' # Compute/visualize probability from a given quantile
#' binom_prob(10, 0.3, 4, type = 'exact')
#' binom_prob(10, 0.3, 4, type = 'lower')
#' binom_prob(10, 0.3, 4, type = 'upper')
#' binom_prob(10, 0.3, c(4, 6), type = 'interval')
#'
#' # Compute/visualize quantiles out of given probability
#' binom_perc(10, 0.5, 0.05)
#' binom_perc(10, 0.5, 0.05, "upper")
#' @seealso \code{\link[stats]{Binomial}}
#' @export
#'
binom_plot <- function(n, p) {

  if(!is.numeric(n)) {
    stop('n must be numeric/integer')
  }

  if(!is.numeric(p)) {
    stop('p must be numeric')
  }

  if((p < 0) | (p > 1)) {
    stop('p must be between 0 and 1')
  }

  n <- as.integer(n)
	bm  <- round(n * p, 2)
	bsd <- round(sqrt(bm * (1 - p)), 2)

	x <- seq(0, n, 1)
	k <- barplot(dbinom(x, n, p),
					      col  = 'blue',
					      xlab = 'No. of success',
					      ylab = 'Probability',
					      ylim = c(0, max(dbinom(x, n, p)) + 0.2),
					      main = paste('Binomial Distribution: n =', n, ', p =', p))

	axis(1, at = (k), labels = (0:n))
	mtext(text = paste('Mean =', bm, ', Std. Dev. =', bsd), side = 3)

  result <- list(avg = bm, stdev = bsd)
  invisible(result)

}


#' @rdname binom_plot
#' @export
#'
binom_prob <- function(n, p, s,
                       type = c("lower", "upper", "exact", "interval")) {

    method <- match.arg(type)

    if((p < 0) | (p > 1)) {
      stop('p must be between 0 and 1')
    }

    if(!is.numeric(n)) {
      stop('n must be numeric/integer')
    }

    if(!is.numeric(p)) {
      stop('p must be numeric')
    }

    if(!is.numeric(s)) {
      stop('s must be numeric/integer')
    }

    if (method == 'interval') {

      if (length(s) != 2) {
        stop('Please specify an interval for s')
      }

    }

    if (any(s > n)) {
      stop('s must be less than or equal to n')
    }

    n <- as.integer(n)
    s <- as.integer(s)

    bm  <- round(n * p, 2)
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


    bp <- barplot(dbinom(x, n, p),
                 col  = cols,
                 xlab = 'No. of success',
                 ylab = 'Probability',
                 ylim = c(0, max(dbinom(x, n, p)) + 0.2),
                 main = paste('Binomial Distribution: n =', n, ', p =', p),
                 sub  = paste('Mean =', bm, ', Std. Dev. =', bsd))

    axis(1, at = (bp), labels = (0:n))

    if (method == "lower") {

    	mtext(text = paste('P(X) <=', s, '=', round(k, 3)), side = 3)

    } else if (method == "upper") {

    	mtext(text = paste('P(X) >=', s, '=', round(k, 3)), side = 3)

    } else if (method == "exact") {

			mtext(text = paste('P(X) =', s, '=', round(k, 3)), side = 3)

    } else {

    	mtext(text = paste0('P(', s[1], ' <= X <= ', s[2], ')', ' = ', round(k, 3)), side = 3)

    }

    result <- list(prob = k, avg = bm, stdev = bsd)
    invisible(result)

}


#' @rdname binom_plot
#' @export
#'
binom_perc <- function(n, p, tp, type = c("lower", "upper")) {

    if(!is.numeric(n)) {
      stop('n must be numeric/integer')
    }

    if(!is.numeric(p)) {
      stop('p must be numeric')
    }

    if(!is.numeric(tp)) {
      stop('tp must be numeric')
    }

    if((p < 0) | (p > 1)) {
      stop('p must be between 0 and 1')
    }

    if((tp < 0) | (tp > 0.5)) {
      stop('tp must be between 0 and 0.5')
    }

    n <- as.integer(n)

    method <- match.arg(type)

    x <- seq(0, n, 1)

    if (method == "lower") {

        k <- round(qbinom(tp, n, p), 3)
        cols <- ifelse(cumsum(dbinom(x, n, p)) <= pbinom(k, n, p), "#0000CD", "#6495ED")

    }  else {

        k <- round(qbinom(tp, n, p, lower.tail = F), 3)
        cols <- ifelse(cumsum(dbinom(x, n, p)) > pbinom((k + 1), n, p), "#0000CD", "#6495ED")

    }


    bp <- barplot(dbinom(x, n, p),
                  col  = cols,
                  xlab = 'No. of success',
                  ylab = 'Probability',
                  ylim = c(0, max(dbinom(x, n, p)) + 0.2),
                  main = paste('Binomial Distribution: n =', n, ', p =', p))

    axis(1, at = (bp), labels = (0:n))

    if (method == "lower") {

        mtext(text = paste0('P(X <= ', k, ') <= ', tp, ', but P(X <= ', (k + 1),
                            ') > ', tp), side = 3)

    } else {

        mtext(text = paste0('P(X >= ', (k + 1), ') <= ', tp, ', but P(X >= ', k,
                            ') > ', tp), side = 3)

    }

}
