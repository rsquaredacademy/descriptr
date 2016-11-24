#' @importFrom graphics curve
#' @title Visualize Normal Distribution
#' @description Visualize how changes in mean and standard deviation affect the
#' shape of the normal distribution.
#' @param mean mean of the normal distribution
#' @param sd standard deviation of the normal distribution
#' @examples
#' norm_plot()
#' norm_plot(mean = 2, sd = 0.6)
#' @seealso \code{\link{norm_per}} \code{\link{norm_prob}}
#' \code{\link[stats]{Normal}}
#' @export
#'
norm_plot <- function(mean = 0, sd = 1) {

    if (!is.numeric(mean)) {
      stop('mean must be numeric/integer')
    }

    if (!is.numeric(sd)) {
      stop('sd must be numeric/integer')
    }

    if(sd < 0) {
      stop('sd must be positive')
    }

    x <- xax(mean)


    l    <- seql(mean, sd)
    col  <- c("#0000CD", "#4682B4", "#6495ED", "#4682B4", "#6495ED")
    l1   <- c(3, 2, 1, 5, 6)
    l2   <- c(5, 3, 2, 6, 7)


    xm <- xmm(mean, sd)
    curve(dnorm(x, mean, sd),
          xlab = '',
          ylab = '',
          yaxt = 'n',
          xaxt = 'n',
          xlim = c(xm[1], xm[2]),
          main = 'Normal Distribution')

    mtext(text = paste('Mean:', mean, '     Standard Deviation:', sd), side = 3)
    axis(1, at = l, labels = l)

    texts <- c(expression(paste(mu, '-3', sigma)),
             expression(paste(mu, '-2', sigma)),
             expression(paste(mu, '-', sigma)),
             expression(paste(mu)),
             expression(paste(mu, '+', sigma)),
             expression(paste(mu, '+2', sigma)),
             expression(paste(mu, '+3', sigma)))

    for (i in seq_len(length(texts))) {
        mtext(side = 1, text = texts[i], outer = FALSE, at = l[i],
              line = 2.5, col = "#4B0082")
    }


    for (i in seq_len(length(l1))) {
        pol_cord(l[l1[i]], l[l2[i]], mean, sd, col = col[i])
    }

}


#' @importFrom stats qnorm
#' @title Nomral Distribution Percentile
#' @description Visualize the percentile from the value of the lower/upper
#' cumulative distribution function of the student's t-distribution
#' @param probs a probability value
#' @param mean mean of the normal distribution
#' @param sd standard deviation of the normal distribution
#' @param type lower tail, upper tail or both
#' @return percentile for the \code{probs} based on \code{mean}, \code{sd} and
#' \code{type}
#' @examples
#' norm_per(0.95, mean = 2, sd = 1.36)
#' norm_per(0.3, mean = 2, sd = 1.36, type = 'upper')
#' norm_per(0.95, mean = 2, sd = 1.36, type = 'both')
#' @seealso \code{\link{norm_plot}} \code{\link{norm_prob}} \code{\link[stats]{Normal}}
#' @export
#'
norm_per <- function(probs = 0.95, mean = 0, sd = 1, type = c("lower", "upper", "both")) {

  if (!is.numeric(mean)) {
    stop('mean must be numeric/integer')
  }

  if (!is.numeric(sd)) {
    stop('sd must be numeric/integer')
  }

  if(sd < 0) {
    stop('sd must be positive')
  }

  if(!is.numeric(probs)) {
    stop('probs must be numeric')
  }

  if((probs < 0) | (probs > 1)) {
    stop('probs must be between 0 and 1')
  }

  x <- xax(mean)

  method <- match.arg(type)

  l    <- seql(mean, sd)
  ln   <- length(l)

  if (method == 'lower') {

    pp   <- round(qnorm(probs, mean, sd), 3)
    lc   <- c(l[1], pp, l[ln])
    col  <- c("#0000CD", "#6495ED")
    l1   <- c(1, 2)
    l2   <- c(2, 3)

  } else if (method == "upper") {

    pp   <- round(qnorm(probs, mean, sd, lower.tail = F), 3)
    lc   <- c(l[1], pp, l[ln])
    col  <- c("#6495ED", "#0000CD")
    l1   <- c(1, 2)
    l2   <- c(2, 3)

  } else {

    alpha <- (1 - probs) / 2
    pp1   <- round(qnorm(alpha, mean, sd), 3)
    pp2   <- round(qnorm(alpha, mean, sd, lower.tail = F), 3)
    pp    <- c(pp1, pp2)
    lc    <- c(l[1], pp1, pp2, l[ln])
    col   <- c("#6495ED", "#0000CD", "#6495ED")
    l1    <- c(1, 2, 3)
    l2    <- c(2, 3, 4)

  }

  xm <- xmm(mean, sd)
  curve(dnorm(x, mean, sd),
        xlab = '',
        ylab = '',
        yaxt = 'n',
        xaxt = 'n',
        xlim = c(xm[1], xm[2]),
        ylim = c(0, max(dnorm(x, mean, sd)) + 0.03),
        main = 'Normal Distribution',
        sub  = paste('Mean:', mean, ' Standard Deviation:', sd),
        bty  = 'n')

  if (method == "lower") {

    mtext(text = paste0('P(X < ', pp, ') = ', probs * 100, '%'), side = 3)
    text(x = pp - 0.15, y = max(dnorm(x, mean, sd)), labels = paste0(probs * 100, '%'), col = "#0000CD")
    text(x = pp + 0.15, y = max(dnorm(x, mean, sd)), labels = paste0((1 - probs) * 100, '%'), col = "#6495ED")

  } else if (method == "upper") {

    mtext(text = paste0('P(X > ', pp, ') = ', probs * 100, '%'), side = 3)
    text(x = pp - 0.15, y = max(dnorm(x, mean, sd)), labels = paste0((1 - probs) * 100, '%'), col = "#6495ED")
    text(x = pp + 0.15, y = max(dnorm(x, mean, sd)), labels = paste0(probs * 100, '%'), col = "#0000CD")

  } else {

    mtext(text = paste0('P(', pp[1], ' < X < ', pp[2], ') = ', probs * 100, '%'), side = 3)
    text(x = mean, y = max(dnorm(x, mean, sd)) + 0.025, labels = paste0(probs * 100, '%'), col = "#0000CD")
    text(x = pp[1] - 0.15, y = max(dnorm(x, mean, sd)) + 0.025, labels = paste0(alpha * 100, '%'), col = "#6495ED")
    text(x = pp[2] + 0.15, y = max(dnorm(x, mean, sd)) + 0.025, labels = paste0(alpha * 100, '%'), col = "#6495ED")

  }


  axis(1, at = l, labels = l)

  mtext(side = 1, text = expression(paste(mu)), outer = FALSE, at = mean,
        line = 2.5, col = "#4B0082")

  for (i in seq_len(length(l1))) {
      pol_cord(lc[l1[i]], lc[l2[i]], mean, sd, col = col[i])
  }

  pln <- length(pp)

  for (i in seq_len(pln)) {

    abline(v = pp[i], lty = 3, lwd = 2)
    points(x = pp[i], y = min(dnorm(x, mean, sd)),
           type = 'p', pch = 4, cex = 2)
    mtext(side = 1, text = pp[i], outer = FALSE, at = pp[i],
          line = 0.3, col = "#4B0082", cex = 0.8)

  }

  result <- list(x = pp)
  return(result)

}


#' @importFrom stats pnorm
#' @title Area Under Normal Distribution
#' @description Visualize area under normal distribution
#' @param perc percentile
#' @param mean mean of the normal distribution
#' @param sd standard deviation of the normal distribution
#' @param type lower tail, upper tail, or both
#' @return probability value for \code{perc} based on \code{mean}, \code{sd} and
#' \code{type}
#' @examples
#' norm_prob(3.78, mean = 2, sd = 1.36)
#' norm_prob(3.43, mean = 2, sd = 1.36, type = 'upper')
#' norm_prob(c(-1.74, 1.83), type = 'both')
#' @seealso \code{\link{norm_plot}} \code{\link{norm_per}} \code{\link[stats]{Normal}}
#' @export
#'
norm_prob <- function(perc, mean = 0, sd = 1, type = c("lower", "upper", "both")) {

  method <- match.arg(type)

  if (length(perc) == 2) {
    method <- 'both'
  }

  if (!is.numeric(mean)) {
    stop('mean must be numeric/integer')
  }

  if (!is.numeric(sd)) {
    stop('sd must be numeric/integer')
  }

  if (!is.numeric(perc)) {
    stop('perc must be numeric/integer')
  }

  if(sd < 0) {
    stop('sd must be positive')
  }

  if (length(perc) > 2) {
    stop('Please do not specify more than 2 percentile values')
  }

  if ((method == 'both') & (length(perc) != 2)) {
    stop('Specify two percentile values')
  }

  x    <- xax(mean)
  l    <- seql(mean, sd)
  ln   <- length(l)

  if (method == 'lower') {

    pp   <- round(pnorm(perc, mean, sd), 3)
    lc   <- c(l[1], perc, l[ln])
    col  <- c("#0000CD", "#6495ED")
    l1   <- c(1, 2)
    l2   <- c(2, 3)

  } else if (method == "upper") {

    pp   <- round(pnorm(perc, mean, sd, lower.tail = F), 3)
    lc   <- c(l[1], perc, l[ln])
    col  <- c("#6495ED", "#0000CD")
    l1   <- c(1, 2)
    l2   <- c(2, 3)

  } else {

    pp1   <- round(pnorm(perc[1], mean, sd), 3)
    pp2   <- round(pnorm(perc[2], mean, sd, lower.tail = F), 3)
    pp    <- c(pp1, pp2)
    lc    <- c(l[1], perc[1], perc[2], l[ln])
    col   <- c("#6495ED", "#0000CD", "#6495ED")
    l1    <- c(1, 2, 3)
    l2    <- c(2, 3, 4)

  }

  xm <- xmm(mean, sd)
  curve(dnorm(x, mean, sd),
        xlab = '',
        ylab = '',
        yaxt = 'n',
        xaxt = 'n',
        xlim = c(xm[1], xm[2]),
        ylim = c(0, max(dnorm(x, mean, sd)) + 0.08),
        main = 'Normal Distribution',
        sub  = paste('Mean:', mean, ' Standard Deviation:', sd),
        bty  = 'n')

  if (method == "lower") {

    mtext(text = paste0('P(X < ', perc, ') = ', pp * 100, '%'), side = 3)
    text(x = perc - 0.25, y = max(dnorm(x, mean, sd)) + 0.07, labels = paste0(pp * 100, '%'), col = "#0000CD")
    text(x = perc + 0.25, y = max(dnorm(x, mean, sd)) + 0.07, labels = paste0((1 - pp) * 100, '%'), col = "#6495ED")

  } else if (method == "upper") {

    mtext(text = paste0('P(X > ', perc, ') = ', pp * 100, '%'), side = 3)
    text(x = perc - 0.25, y = max(dnorm(x, mean, sd)) + 0.07, labels = paste0((1 - pp) * 100, '%'), col = "#6495ED")
    text(x = perc + 0.25, y = max(dnorm(x, mean, sd)) + 0.07, labels = paste0(pp * 100, '%'), col = "#0000CD")

  } else {

    mtext(text = paste0('P(', perc[1], ' < X < ', perc[2], ') = ', pp * 100, '%'), side = 3)
    text(x = mean, y = max(dnorm(x, mean, sd)) + 0.07, labels = paste0((1 - (pp1 + pp2)) * 100, '%'), col = "#0000CD")
    text(x = perc[1] - 0.25, y = max(dnorm(x, mean, sd)) + 0.07, labels = paste0(pp[1] * 100, '%'), col = "#6495ED")
    text(x = perc[2] + 0.25, y = max(dnorm(x, mean, sd)) + 0.07, labels = paste0(pp[2] * 100, '%'), col = "#6495ED")

  }


  axis(1, at = l, labels = l)

  mtext(side = 1, text = expression(paste(mu)), outer = FALSE, at = mean,
        line = 2.5, col = "#4B0082")

  for (i in seq_len(length(l1))) {
      pol_cord(lc[l1[i]], lc[l2[i]], mean, sd, col = col[i])
  }

  pln <- length(pp)

  for (i in seq_len(pln)) {

    abline(v = perc[i], lty = 3, lwd = 2)
    points(x = perc[i], y = min(dnorm(x, mean, sd)),
           type = 'p', pch = 4, cex = 2)
    mtext(side = 1, text = perc[i], outer = FALSE, at = perc[i],
          line = 0.3, col = "#4B0082", cex = 0.8)

  }

  result <- list(prob = pp)
  return(result)

}
