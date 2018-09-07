#' Visualize normal distribution
#'
#' Visualize how changes in mean and standard deviation affect the
#' shape of the normal distribution. Compute & visualize quantiles out of given
#' probability  and probability from a given quantile.
#'
#' @param perc Quantile value.
#' @param probs Probability value.
#' @param mean Mean of the normal distribution.
#' @param sd Standard deviation of the normal distribution.
#' @param type Lower tail, upper tail or both.
#'
#' @return Percentile for the \code{probs} based on \code{mean}, \code{sd} and
#' \code{type} or probability value for \code{perc} based on \code{mean},
#' \code{sd} and \code{type}.
#'
#' @section Deprecated functions:
#' \code{norm_plot()}, \code{norm_prob()} and \code{norm_per()} have been
#' deprecated. Instead use \code{dist_norm_plot()}, \code{dist_norm_prob()} and
#' \code{dist_norm_per()}.
#'
#' @importFrom graphics curve
#' @importFrom stats qnorm pnorm
#'
#' @examples
#' # visualize normal distribution
#' dist_norm_plot()
#' dist_norm_plot(mean = 2, sd = 0.6)
#'
#' # visualize probability from a given quantile
#' dist_norm_prob(3.78, mean = 2, sd = 1.36)
#' dist_norm_prob(3.43, mean = 2, sd = 1.36, type = 'upper')
#' dist_norm_prob(c(-1.74, 1.83), type = 'both')
#'
#' # visualize quantiles out of given probability
#' dist_norm_perc(0.95, mean = 2, sd = 1.36)
#' dist_norm_perc(0.3, mean = 2, sd = 1.36, type = 'upper')
#' dist_norm_perc(0.95, mean = 2, sd = 1.36, type = 'both')
#'
#' @seealso \code{\link[stats]{Normal}}
#'
#' @export
#'
dist_norm_plot <- function(mean = 0, sd = 1) {
  if (!is.numeric(mean)) {
    stop("mean must be numeric/integer")
  }

  if (!is.numeric(sd)) {
    stop("sd must be numeric/integer")
  }

  if (sd < 0) {
    stop("sd must be positive")
  }

  x   <- xax(mean)
  l   <- seql(mean, sd)
  col <- c("#0000CD", "#4682B4", "#6495ED", "#4682B4", "#6495ED")
  l1  <- c(3, 2, 1, 5, 6)
  l2  <- c(5, 3, 2, 6, 7)


  xm <- xmm(mean, sd)
  curve(
    dnorm(x, mean, sd),
    xlab = "",
    ylab = "",
    yaxt = "n",
    xaxt = "n",
    xlim = c(xm[1], xm[2]),
    ylim = c(0, max(dnorm(x, mean, sd)) + 0.03),
    main = "Normal Distribution"
  )

  mtext(text = paste("Mean:", mean, "     Standard Deviation:", sd), side = 3)
  axis(1, at = l, labels = l)

  texts <- c(
    expression(paste(mu, "-3", sigma)),
    expression(paste(mu, "-2", sigma)),
    expression(paste(mu, "-", sigma)),
    expression(paste(mu)),
    expression(paste(mu, "+", sigma)),
    expression(paste(mu, "+2", sigma)),
    expression(paste(mu, "+3", sigma))
  )

  ll <- l[3:9]

  for (i in seq_len(length(texts))) {
    mtext(
      side = 1, text = texts[i], outer = FALSE, at = ll[i],
      line = 2.5, col = "#4B0082"
    )
  }


  for (i in seq_len(length(l1))) {
    pol_cord(ll[l1[i]], ll[l2[i]], mean, sd, col = col[i])
  }
}

#' @export
#' @rdname dist_norm_plot
#' @usage NULL
#'
norm_plot <- function(mean = 0, sd = 1) {
  .Deprecated("dist_norm_plot()")
  dist_norm_plot(mean, sd)
}


#' @rdname dist_norm_plot
#' @export
#'
dist_norm_perc <- function(probs = 0.95, mean = 0, sd = 1, type = c("lower", "upper", "both")) {

  if (!is.numeric(mean)) {
    stop("mean must be numeric/integer")
  }

  if (!is.numeric(sd)) {
    stop("sd must be numeric/integer")
  }

  if (sd < 0) {
    stop("sd must be positive")
  }

  if (!is.numeric(probs)) {
    stop("probs must be numeric")
  }

  if ((probs < 0) | (probs > 1)) {
    stop("probs must be between 0 and 1")
  }

  x      <- xax(mean)
  method <- match.arg(type)
  l      <- seql(mean, sd)
  ln     <- length(l)

  if (method == "lower") {
    pp  <- round(qnorm(probs, mean, sd), 3)
    lc  <- c(l[1], pp, l[ln])
    col <- c("#0000CD", "#6495ED")
    l1  <- c(1, 2)
    l2  <- c(2, 3)
  } else if (method == "upper") {
    pp  <- round(qnorm(probs, mean, sd, lower.tail = F), 3)
    lc  <- c(l[1], pp, l[ln])
    col <- c("#6495ED", "#0000CD")
    l1  <- c(1, 2)
    l2  <- c(2, 3)
  } else {
    alpha <- (1 - probs) / 2
    pp1 <- round(qnorm(alpha, mean, sd), 3)
    pp2 <- round(qnorm(alpha, mean, sd, lower.tail = F), 3)
    pp  <- c(pp1, pp2)
    lc  <- c(l[1], pp1, pp2, l[ln])
    col <- c("#6495ED", "#0000CD", "#6495ED")
    l1  <- c(1, 2, 3)
    l2  <- c(2, 3, 4)
  }

  xm <- xmm(mean, sd)
  curve(
    dnorm(x, mean, sd),
    xlab = "",
    ylab = "",
    yaxt = "n",
    xaxt = "n",
    xlim = c(xm[1], xm[2]),
    ylim = c(0, max(dnorm(x, mean, sd)) + 0.03),
    main = "Normal Distribution",
    sub  = paste("Mean:", mean, " Standard Deviation:", sd),
    bty  = "n"
  )

  if (method == "lower") {
    mtext(text = paste0("P(X < ", pp, ") = ", probs * 100, "%"), side = 3)
    text(x = pp - sd, y = max(dnorm(x, mean, sd)) + 0.025, labels = paste0(probs * 100, "%"), col = "#0000CD", cex = 0.6)
    text(x = pp + sd, y = max(dnorm(x, mean, sd)) + 0.025, labels = paste0((1 - probs) * 100, "%"), col = "#6495ED", cex = 0.6)
  } else if (method == "upper") {
    mtext(text = paste0("P(X > ", pp, ") = ", probs * 100, "%"), side = 3)
    text(x = pp - sd, y = max(dnorm(x, mean, sd)) + 0.025, labels = paste0((1 - probs) * 100, "%"), col = "#6495ED", cex = 0.6)
    text(x = pp + sd, y = max(dnorm(x, mean, sd)) + 0.025, labels = paste0(probs * 100, "%"), col = "#0000CD", cex = 0.6)
  } else {
    mtext(text = paste0("P(", pp[1], " < X < ", pp[2], ") = ", probs * 100, "%"), side = 3)
    text(x = mean, y = max(dnorm(x, mean, sd)) + 0.025, labels = paste0(probs * 100, "%"), col = "#0000CD", cex = 0.6)
    text(x = pp[1] - sd, y = max(dnorm(x, mean, sd)) + 0.025, labels = paste0(alpha * 100, "%"), col = "#6495ED", cex = 0.6)
    text(x = pp[2] + sd, y = max(dnorm(x, mean, sd)) + 0.025, labels = paste0(alpha * 100, "%"), col = "#6495ED", cex = 0.6)
  }


  axis(1, at = l, labels = l)

  mtext(
    side = 1, text = expression(paste(mu)), outer = FALSE, at = mean,
    line = 2.5, col = "#4B0082"
  )

  for (i in seq_len(length(l1))) {
    pol_cord(lc[l1[i]], lc[l2[i]], mean, sd, col = col[i])
  }

  pln <- length(pp)

  for (i in seq_len(pln)) {
    abline(v = pp[i], lty = 3, lwd = 2)
    points(
      x = pp[i], y = 0,
      type = "p", pch = 4, cex = 2
    )
    mtext(
      side = 1, text = pp[i], outer = FALSE, at = pp[i],
      line = 0.3, col = "#4B0082", cex = 0.8
    )
  }

  result <- list(x = pp)
  invisible(result)
}

#' @export
#' @rdname dist_norm_plot
#' @usage NULL
#'
norm_per <- function(probs = 0.95, mean = 0, sd = 1, type = c("lower", "upper", "both")) {
  .Deprecated("dist_norm_perc()")
  dist_norm_perc(probs, mean, sd, type)
}



#' @rdname dist_norm_plot
#' @export
#'
dist_norm_prob <- function(perc, mean = 0, sd = 1, type = c("lower", "upper", "both")) {

  method <- match.arg(type)

  if (length(perc) == 2) {
    method <- "both"
  }

  if (!is.numeric(mean)) {
    stop("mean must be numeric/integer")
  }

  if (!is.numeric(sd)) {
    stop("sd must be numeric/integer")
  }

  if (!is.numeric(perc)) {
    stop("perc must be numeric/integer")
  }

  if (sd < 0) {
    stop("sd must be positive")
  }

  if (length(perc) > 2) {
    stop("Please do not specify more than 2 percentile values")
  }

  if ((method == "both") & (length(perc) != 2)) {
    stop("Specify two percentile values")
  }

  el <- max(abs(perc - mean)) / sd + 1
  x  <- xaxp(mean, el)
  l  <- seqlp(mean, sd, el)
  ln <- length(l)

  if (method == "lower") {
    pp  <- round(pnorm(perc, mean, sd), 3)
    lc  <- c(l[1], perc, l[ln])
    col <- c("#0000CD", "#6495ED")
    l1  <- c(1, 2)
    l2  <- c(2, 3)
  } else if (method == "upper") {
    pp  <- round(pnorm(perc, mean, sd, lower.tail = F), 3)
    lc  <- c(l[1], perc, l[ln])
    col <- c("#6495ED", "#0000CD")
    l1  <- c(1, 2)
    l2  <- c(2, 3)
  } else {
    pp1 <- round(pnorm(perc[1], mean, sd), 3)
    pp2 <- round(pnorm(perc[2], mean, sd, lower.tail = F), 3)
    pp  <- c(pp1, pp2)
    lc  <- c(l[1], perc[1], perc[2], l[ln])
    col <- c("#6495ED", "#0000CD", "#6495ED")
    l1  <- c(1, 2, 3)
    l2  <- c(2, 3, 4)
  }

  xm <- xmmp(mean, sd, el)
  curve(
    dnorm(x, mean, sd),
    xlab = "",
    ylab = "",
    yaxt = "n",
    xaxt = "n",
    xlim = c(xm[1], xm[2]),
    ylim = c(0, max(dnorm(x, mean, sd)) + 0.08),
    main = "Normal Distribution",
    sub  = paste("Mean:", mean, " Standard Deviation:", sd),
    bty  = "n"
  )

  if (method == "lower") {
    mtext(text = paste0("P(X < ", perc, ") = ", pp * 100, "%"), side = 3)
    text(x = perc - sd, y = max(dnorm(x, mean, sd)) + 0.07, labels = paste0(pp * 100, "%"), col = "#0000CD", cex = 0.6)
    text(x = perc + sd, y = max(dnorm(x, mean, sd)) + 0.07, labels = paste0((1 - pp) * 100, "%"), col = "#6495ED", cex = 0.6)
  } else if (method == "upper") {
    mtext(text = paste0("P(X > ", perc, ") = ", pp * 100, "%"), side = 3)
    text(x = perc - sd, y = max(dnorm(x, mean, sd)) + 0.07, labels = paste0((1 - pp) * 100, "%"), col = "#6495ED", cex = 0.6)
    text(x = perc + sd, y = max(dnorm(x, mean, sd)) + 0.07, labels = paste0(pp * 100, "%"), col = "#0000CD", cex = 0.6)
  } else {
    mtext(text = paste0("P(", perc[1], " < X < ", perc[2], ") = ", (1 - (pp1 + pp2)) * 100, "%"), side = 3)
    text(x = mean(perc), y = max(dnorm(x, mean, sd)) + 0.07, labels = paste0((1 - (pp1 + pp2)) * 100, "%"), col = "#0000CD", cex = 0.6)
    text(x = perc[1] - sd, y = max(dnorm(x, mean, sd)) + 0.07, labels = paste0(pp[1] * 100, "%"), col = "#6495ED", cex = 0.6)
    text(x = perc[2] + sd, y = max(dnorm(x, mean, sd)) + 0.07, labels = paste0(pp[2] * 100, "%"), col = "#6495ED", cex = 0.6)
  }


  axis(1, at = l, labels = l)

  mtext(
    side = 1, text = expression(paste(mu)), outer = FALSE, at = mean,
    line = 2.5, col = "#4B0082"
  )

  for (i in seq_len(length(l1))) {
    pol_cord(lc[l1[i]], lc[l2[i]], mean, sd, col = col[i])
  }

  pln <- length(pp)

  for (i in seq_len(pln)) {
    abline(v = perc[i], lty = 3, lwd = 2)
    points(
      x = perc[i], y = 0,
      type = "p", pch = 4, cex = 2
    )
    mtext(
      side = 1, text = perc[i], outer = FALSE, at = perc[i],
      line = 0.3, col = "#4B0082", cex = 0.8
    )
  }

  result <- list(prob = pp)
  invisible(result)
}

#' @export
#' @rdname dist_norm_plot
#' @usage NULL
#'
norm_prob <- function(perc, mean = 0, sd = 1, type = c("lower", "upper", "both")) {
  .Deprecated("dist_norm_prob()")
  dist_norm_prob(perc, mean, sd, type)
}
