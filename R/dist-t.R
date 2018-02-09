#' @importFrom stats dt qt pt
#' @title Visualize t Distribution
#' @description Visualize how degrees of freedom affect the shape of t
#' distribution, visualize quantiles out of given probability and
#' probability from a given quantile.
#' @param probs a probability value
#' @param perc a quantile value
#' @param df degrees of freedom
#' @param type lower tail, upper tail, interval or both
#' @return percentile for the \code{probs} based on \code{df} and
#' \code{type} or probability value for the \code{perc} based on \code{df} and
#' \code{type}
#' @section Deprecated Functions:
#' \code{t_plot()}, \code{t_prob()} and \code{t_per()} have been deprecated.
#' Instead use \code{dist_t_plot()}, \code{dist_t_prob()} and \code{dist_t_perc()}.
#' @family dist_t
#' @examples
#' # visualize t distribution
#' dist_t_plot()
#' dist_t_plot(6)
#' dist_t_plot(df = 8)
#'
#' # compute\/visualize quantiles out of given probability
#' dist_t_perc(probs = 0.95, df = 4, type = 'lower')
#' dist_t_perc(probs = 0.35, df = 4, type = 'upper')
#' dist_t_perc(probs = 0.69, df = 7, type = 'both')
#'
#' # compute\/visualize probability from a given quantile
#' dist_t_prob(2.045, 7, 'lower')
#' dist_t_prob(0.945, 7, 'upper')
#' dist_t_prob(1.445, 7, 'interval')
#' dist_t_prob(1.6, 7, 'both')
#' @seealso \code{\link[stats]{TDist}}
#' @name dist_t
NULL

#' @export
#' @rdname dist_t
#'
dist_t_plot <- function(df = 3) {
  if (!is.numeric(df)) {
    stop("df must be numeric/integer")
  }

  df <- as.integer(df)

  x <- seq(-4, 4, 0.01)
  plot(
    x, dt(x, df),
    type = "l",
    lwd = 2,
    col = "blue",
    xlab = "",
    ylab = "",
    xaxt = "n",
    yaxt = "n",
    main = "t distribution"
  )

  y <- c(-4, seq(-4, 4, 0.01), 4)
  z <- c(0, dt(seq(-4, 4, 0.01), df), 0)
  polygon(y, z, col = "#4682B4")

  mtext(text = paste("df =", df), side = 3)
  axis(1, at = (-4:4), labels = (-4:4))
}

#' @export
#' @rdname dist_t
#' @usage NULL
#'
t_plot <- function(df = 3) {
  .Deprecated("dist_t_plot()")
  dist_t_plot(df)
}

#' @rdname dist_t
#' @export
#'
dist_t_perc <- function(probs = 0.95, df = 4, type = c("lower", "upper", "both")) {
  if (!is.numeric(probs)) {
    stop("probs must be numeric")
  }

  if (!is.numeric(df)) {
    stop("df must be numeric/integer")
  }

  if ((probs < 0) | (probs > 1)) {
    stop("probs must be between 0 and 1")
  }

  df <- as.integer(df)

  method <- match.arg(type)

  l <- seq(-5, 5, 0.01)
  ln <- length(l)

  if (method == "lower") {
    pp <- round(qt(probs, df), 3)
    lc <- c(l[1], pp, l[ln])
    col <- c("#0000CD", "#6495ED")
    l1 <- c(1, 2)
    l2 <- c(2, 3)
  } else if (method == "upper") {
    pp <- round(qt(probs, df, lower.tail = F), 3)
    lc <- c(l[1], pp, l[ln])
    col <- c("#6495ED", "#0000CD")
    l1 <- c(1, 2)
    l2 <- c(2, 3)
  } else {
    alpha <- (1 - probs) / 2
    pp1 <- round(qt(alpha, df), 3)
    pp2 <- round(qt(alpha, df, lower.tail = F), 3)
    pp <- c(pp1, pp2)
    lc <- c(l[1], pp1, pp2, l[ln])
    col <- c("#6495ED", "#0000CD", "#6495ED")
    l1 <- c(1, 2, 3)
    l2 <- c(2, 3, 4)
  }

  plot(
    l, dt(l, df),
    type = "l",
    lwd = 2,
    col = "blue",
    xlab = "",
    ylab = "",
    xaxt = "n",
    yaxt = "n",
    xlim = c(-5, 5),
    ylim = c(0, max(dt(l, df)) + 0.03),
    bty = "n",
    sub = paste("df =", df),
    main = "t distribution"
  )

  if (method == "lower") {
    mtext(text = paste0("P(X < ", pp, ") = ", probs * 100, "%"), side = 3)
    text(x = pp - 0.3, y = max(dt(l, df)) + 0.025, labels = paste0(probs * 100, "%"), col = "#0000CD", cex = 0.6)
    text(x = pp + 0.3, y = max(dt(l, df)) + 0.025, labels = paste0((1 - probs) * 100, "%"), col = "#6495ED", cex = 0.6)
  } else if (method == "upper") {
    mtext(text = paste0("P(X > ", pp, ") = ", probs * 100, "%"), side = 3)
    text(x = pp - 0.3, y = max(dt(l, df)) + 0.025, labels = paste0((1 - probs) * 100, "%"), col = "#6495ED", cex = 0.6)
    text(x = pp + 0.3, y = max(dt(l, df)) + 0.025, labels = paste0(probs * 100, "%"), col = "#0000CD", cex = 0.6)
  } else {
    mtext(text = paste0("P(", pp[1], " < X < ", pp[2], ") = ", probs * 100, "%"), side = 3)
    text(x = mean(l), y = max(dt(l, df)) + 0.025, labels = paste0(probs * 100, "%"), col = "#0000CD", cex = 0.6)
    text(x = pp[1] - 0.3, y = max(dt(l, df)) + 0.025, labels = paste0(alpha * 100, "%"), col = "#6495ED", cex = 0.6)
    text(x = pp[2] + 0.3, y = max(dt(l, df)) + 0.025, labels = paste0(alpha * 100, "%"), col = "#6495ED", cex = 0.6)
  }


  axis(1, at = (-5:5), labels = (-5:5))

  for (i in seq_len(length(l1))) {
    pol_t(lc[l1[i]], lc[l2[i]], df, col = col[i])
  }

  pln <- length(pp)

  for (i in seq_len(pln)) {
    abline(v = pp[i], lty = 3, lwd = 2)
    points(
      x = pp[i], y = min(dt(l, df)),
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
#' @rdname dist_t
#' @usage NULL
#'
t_per <- function(probs = 0.95, df = 4, type = c("lower", "upper", "both")) {
  .Deprecated("dist_t_perc()")
  dist_t_perc(probs, df, type)
}

#' @rdname dist_t
#' @export
#'
dist_t_prob <- function(perc, df, type = c("lower", "upper", "interval", "both")) {
  if (!is.numeric(perc)) {
    stop("perc must be numeric/integer")
  }

  if (!is.numeric(df)) {
    stop("df must be numeric/integer")
  }

  df <- as.integer(df)

  method <- match.arg(type)

  l <- if (abs(perc) < 5) {
    seq(-5, 5, 0.01)
  } else {
    seq(-(perc + 1), (perc + 1), 0.01)
  }

  ln <- length(l)

  if (method == "lower") {
    pp <- round(pt(perc, df), 3)
    lc <- c(l[1], perc, l[ln])
    col <- c("#0000CD", "#6495ED")
    l1 <- c(1, 2)
    l2 <- c(2, 3)
  } else if (method == "upper") {
    pp <- round(pt(perc, df, lower.tail = F), 3)
    lc <- c(l[1], perc, l[ln])
    col <- c("#6495ED", "#0000CD")
    l1 <- c(1, 2)
    l2 <- c(2, 3)
  } else if (method == "interval") {
    if (perc < 0) {
      perc <- -perc
    }

    pp1 <- round(pt(-perc, df), 3)
    pp2 <- round(pt(perc, df, lower.tail = F), 3)
    pp <- c(pp1, pp2)
    lc <- c(l[1], -perc, perc, l[ln])
    col <- c("#6495ED", "#0000CD", "#6495ED")
    l1 <- c(1, 2, 3)
    l2 <- c(2, 3, 4)
  } else {
    if (perc < 0) {
      perc <- -perc
    }

    pp1 <- round(pt(-perc, df), 3)
    pp2 <- round(pt(perc, df, lower.tail = F), 3)
    pp <- c(pp1, pp2)
    lc <- c(l[1], -perc, perc, l[ln])
    col <- c("#0000CD", "#6495ED", "#0000CD")
    l1 <- c(1, 2, 3)
    l2 <- c(2, 3, 4)
  }

  plot(
    l, dt(l, df),
    type = "l",
    lwd = 2,
    col = "blue",
    xlab = "",
    ylab = "",
    xaxt = "n",
    yaxt = "n",
    xlim = c(min(l), max(l)),
    ylim = c(0, max(dt(l, df)) + 0.07),
    bty = "n",
    sub = paste("df =", df),
    main = "t distribution"
  )

  axis(1, at = (min(l):max(l)), labels = (min(l):max(l)))

  for (i in seq_len(length(l1))) {
    pol_t(lc[l1[i]], lc[l2[i]], df, col = col[i])
  }


  if (method == "lower") {
    mtext(text = paste0("P(X < ", perc, ") = ", pp * 100, "%"), side = 3)
    text(x = perc - 1, y = max(dt(l, df)) + 0.07, labels = paste0(pp * 100, "%"), col = "#0000CD", cex = 0.6)
    text(x = perc + 1, y = max(dt(l, df)) + 0.07, labels = paste0((1 - pp) * 100, "%"), col = "#6495ED", cex = 0.6)
    abline(v = perc, lty = 3, lwd = 2)
    points(
      x = perc, y = min(dt(l, df)),
      type = "p", pch = 4, cex = 2
    )
    mtext(
      side = 1, text = perc, outer = FALSE, at = perc,
      line = 0.3, col = "#4B0082", cex = 0.8
    )
  } else if (method == "upper") {
    mtext(text = paste0("P(X > ", perc, ") = ", pp * 100, "%"), side = 3)
    text(x = perc - 1, y = max(dt(l, df)) + 0.07, labels = paste0((1 - pp) * 100, "%"), col = "#6495ED", cex = 0.6)
    text(x = perc + 1, y = max(dt(l, df)) + 0.07, labels = paste0(pp * 100, "%"), col = "#0000CD", cex = 0.6)
    abline(v = perc, lty = 3, lwd = 2)
    points(
      x = perc, y = min(dt(l, df)),
      type = "p", pch = 4, cex = 2
    )
    mtext(
      side = 1, text = perc, outer = FALSE, at = perc,
      line = 0.3, col = "#4B0082", cex = 0.8
    )
  } else if (method == "interval") {
    mtext(text = paste0("P(", -perc, " < X < ", perc, ") = ", (1 - (pp1 + pp2)) * 100, "%"), side = 3)
    text(x = 0, y = max(dt(l, df)) + 0.07, labels = paste0((1 - (pp1 + pp2)) * 100, "%"), col = "#0000CD", cex = 0.6)
    text(x = perc + 1, y = max(dt(l, df)) + 0.07, labels = paste0(pp[1] * 100, "%"), col = "#6495ED", cex = 0.6)
    text(x = -perc - 1, y = max(dt(l, df)) + 0.07, labels = paste0(pp[2] * 100, "%"), col = "#6495ED", cex = 0.6)
    abline(v = -perc, lty = 3, lwd = 2)
    abline(v = perc, lty = 3, lwd = 2)
    points(
      x = -perc, y = min(dt(l, df)),
      type = "p", pch = 4, cex = 2
    )
    points(
      x = perc, y = min(dt(l, df)),
      type = "p", pch = 4, cex = 2
    )
    mtext(
      side = 1, text = -perc, outer = FALSE, at = -perc,
      line = 0.3, col = "#4B0082", cex = 0.8
    )
    mtext(
      side = 1, text = perc, outer = FALSE, at = perc,
      line = 0.3, col = "#4B0082", cex = 0.8
    )
  } else {
    mtext(text = paste0("P(|X| > ", perc, ") = ", (pp1 + pp2) * 100, "%"), side = 3)
    text(x = 0, y = max(dt(l, df)) + 0.07, labels = paste0((1 - (pp1 + pp2)) * 100, "%"), col = "#0000CD", cex = 0.6)
    text(x = perc + 1, y = max(dt(l, df)) + 0.07, labels = paste0(pp[1] * 100, "%"), col = "#6495ED", cex = 0.6)
    text(x = -perc - 1, y = max(dt(l, df)) + 0.07, labels = paste0(pp[2] * 100, "%"), col = "#6495ED", cex = 0.6)
    abline(v = -perc, lty = 3, lwd = 2)
    abline(v = perc, lty = 3, lwd = 2)
    points(
      x = -perc, y = min(dt(l, df)),
      type = "p", pch = 4, cex = 2
    )
    points(
      x = perc, y = min(dt(l, df)),
      type = "p", pch = 4, cex = 2
    )
    mtext(
      side = 1, text = -perc, outer = FALSE, at = -perc,
      line = 0.3, col = "#4B0082", cex = 0.8
    )
    mtext(
      side = 1, text = perc, outer = FALSE, at = perc,
      line = 0.3, col = "#4B0082", cex = 0.8
    )
  }

  result <- list(probs = pp)
  invisible(result)
}

#' @export
#' @rdname dist_t
#' @usage NULL
#'
t_prob <- function(perc, df, type = c("lower", "upper", "interval", "both")) {
  .Deprecated("dist_t_prob()")
  dist_t_prob(perc, df, type)
}
