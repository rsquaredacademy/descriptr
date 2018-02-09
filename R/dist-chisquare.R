#' @importFrom stats dchisq qchisq pchisq dnorm
#' @importFrom graphics plot lines polygon points text abline
#' @title Visualize Chi Square Distribution
#' @description Visualize how changes in degrees of freedom affect the shape of
#' the chi square distribution. compute\/visualize quantiles out of given
#' probability and probability from a given quantile.
#' @param df degrees of freedom
#' @param probs a probability value
#' @param perc a quantile value
#' @param type lower tail or upper tail
#' @param normal logical; if \code{TRUE}, normal curve with same \code{mean} and
#' \code{sd} as the chi square distribution is drawn
#' @return percentile for the \code{probs} based on \code{df} and \code{type} or
#' probability value for \code{perc} based on \code{df} and \code{type}
#' @section Deprecated Functions:
#' \code{chi_plot()}, \code{chi_prob()} and \code{chi_per()} have been
#' deprecated. Instead use \code{dist_chi_plot()}, \code{dist_chi_prob()} and
#' \code{dist_chi_perc()}.
#' @examples
#' # visualize chi square distribution
#' dist_chi_plot()
#' dist_chi_plot(df = 5)
#' dist_chi_plot(df = 5, normal = TRUE)
#'
#' # compute\/visualize quantiles out of given probability
#' dist_chi_perc(0.165, 8, 'upper')
#' dist_chi_perc(0.22, 13, 'upper')
#'
#' # compute\/visualize probability from a given quantile.
#' dist_chi_prob(13.58, 11, 'lower')
#' dist_chi_prob(15.72, 13, 'upper')
#' @seealso \code{\link[stats]{Chisquare}}
#' @export
#'
dist_chi_plot <- function(df = 3, normal = FALSE) {
  if (!is.numeric(df)) {
    stop("df must be numeric/integer")
  }

  if (!is.logical(normal)) {
    stop("normal must be logical")
  }

  df <- as.integer(df)

  chim <- round(df, 3)
  chisd <- round(sqrt(2 * df), 3)

  x <- seq(0, 25, 0.01)
  plot(
    x, dchisq(x, df),
    type = "l",
    lwd = 2,
    col = "blue",
    xlab = "",
    ylab = "",
    xaxt = "n",
    yaxt = "n",
    main = paste("Chi Square Distribution: df =", df),
    sub = paste("Mean =", chim, " Std Dev. =", chisd),
    bty = "n"
  )

  if (normal == TRUE) {
    lines(x, dnorm(x, chim, chisd), lty = 2, col = "#FF4500")
    y <- c(0, seq(0, 25, 0.01), 25)
    z <- c(0, dnorm(seq(0, 25, 0.01), chim, chisd), 0)
    polygon(y, z, col = "#FF4500")
  }

  y <- c(0, seq(0, 25, 0.01), 25)
  z <- c(0, dchisq(seq(0, 25, 0.01), df), 0)
  polygon(y, z, col = "#4682B4")

  mtext(text = paste("df =", df), side = 3)
  axis(1, at = (0:25), labels = (0:25))

  points(
    x = chim, y = min(dchisq(x, df)),
    type = "p", pch = 4, cex = 2, col = "#FF4500"
  )

  mtext(
    side = 1, text = expression(paste(mu)), outer = FALSE, at = chim,
    line = 0.5, col = "#4B0082"
  )

  result <- list(mean = chim, stdev = chisd)
  invisible(result)
}

#' @export
#' @rdname dist_chi_plot
#' @usage NULL
#'
chi_plot <- function(df = 3, normal = FALSE) {
  .Deprecated("dist_chi_plot()")
  dist_chi_plot(df, normal)
}

#' @rdname dist_chi_plot
#' @export
#'
dist_chi_perc <- function(probs = 0.95, df = 3, type = c("lower", "upper")) {
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

  chim <- round(df, 3)
  chisd <- round(sqrt(2 * df), 3)

  l <- chiseql(chim, chisd)
  ln <- length(l)

  if (method == "lower") {
    pp <- round(qchisq(probs, df), 3)
    lc <- c(l[1], pp, l[ln])
    col <- c("#0000CD", "#6495ED")
    l1 <- c(1, 2)
    l2 <- c(2, 3)
  } else {
    pp <- round(qchisq(probs, df, lower.tail = F), 3)
    lc <- c(l[1], pp, l[ln])
    col <- c("#6495ED", "#0000CD")
    l1 <- c(1, 2)
    l2 <- c(2, 3)
  }
  xm <- xmm(chim, chisd)
  plot(
    l, dchisq(l, df),
    type = "l",
    lwd = 2,
    col = "blue",
    xlab = "",
    ylab = "",
    xaxt = "n",
    yaxt = "n",
    xlim = c(xm[1], xm[2]),
    ylim = c(0, max(dchisq(l, df)) + 0.03),
    main = paste("Chi Square Distribution: df =", df),
    sub = paste("Mean =", chim, " Std Dev. =", chisd),
    bty = "n"
  )


  if (method == "lower") {
    mtext(text = paste0("P(X < ", pp, ") = ", probs * 100, "%"), side = 3)
    text(x = pp - chisd, y = max(dchisq(l, df)) + 0.02, labels = paste0(probs * 100, "%"), col = "#0000CD", cex = 0.6)
    text(x = pp + chisd, y = max(dchisq(l, df)) + 0.02, labels = paste0((1 - probs) * 100, "%"), col = "#6495ED", cex = 0.6)
  } else {
    mtext(text = paste0("P(X > ", pp, ") = ", probs * 100, "%"), side = 3)
    text(x = pp - chisd, y = max(dchisq(l, df)) + 0.02, labels = paste0((1 - probs) * 100, "%"), col = "#6495ED", cex = 0.6)
    text(x = pp + chisd, y = max(dchisq(l, df)) + 0.02, labels = paste0(probs * 100, "%"), col = "#0000CD", cex = 0.6)
  }


  axis(1, at = seq(0, xm[2], 5), labels = seq(0, xm[2], 5))

  for (i in seq_len(length(l1))) {
    pol_chi(lc[l1[i]], lc[l2[i]], df, col = col[i])
  }

  pln <- length(pp)

  for (i in seq_len(pln)) {
    abline(v = pp[i], lty = 3, lwd = 2)
    points(
      x = pp[i], y = min(dchisq(l, df)),
      type = "p", pch = 4, cex = 2
    )
    mtext(
      side = 1, text = pp[i], outer = FALSE, at = pp[i],
      line = 0.3, col = "#4B0082", cex = 0.8
    )
  }

  result <- list(x = pp, mean = chim, stdev = chisd)
  invisible(result)
}

#' @export
#' @rdname dist_chi_plot
#' @usage NULL
#'
chi_per <- function(probs = 0.95, df = 3, type = c("lower", "upper")) {
  .Deprecated("dist_chi_perc()")
  dist_chi_perc(probs, df, type)
}

#' @rdname dist_chi_plot
#' @export
#'
dist_chi_prob <- function(perc, df, type = c("lower", "upper")) {
  if (!is.numeric(df)) {
    stop("df must be numeric/integer")
  }

  if (!is.numeric(perc)) {
    stop("perc must be numeric/integer")
  }

  method <- match.arg(type)

  chim <- round(df, 3)
  chisd <- round(sqrt(2 * df), 3)

  l <- if (perc < 25) {
    seq(0, 25, 0.01)
  } else {
    seq(0, (perc + (3 * chisd)), 0.01)
  }
  ln <- length(l)

  if (method == "lower") {
    pp <- round(pchisq(perc, df), 3)
    lc <- c(l[1], perc, l[ln])
    col <- c("#0000CD", "#6495ED")
    l1 <- c(1, 2)
    l2 <- c(2, 3)
  } else {
    pp <- round(pchisq(perc, df, lower.tail = F), 3)
    lc <- c(l[1], perc, l[ln])
    col <- c("#6495ED", "#0000CD")
    l1 <- c(1, 2)
    l2 <- c(2, 3)
  }

  plot(
    l, dchisq(l, df),
    type = "l",
    lwd = 2,
    col = "blue",
    xlab = "",
    ylab = "",
    xaxt = "n",
    yaxt = "n",
    xlim = c((-chisd - 1), l[ln]),
    ylim = c(0, max(dchisq(l, df)) + 0.03),
    main = paste("Chi Square Distribution: df =", df),
    sub = paste("Mean =", chim, " Std Dev. =", chisd),
    bty = "n"
  )


  if (method == "lower") {
    mtext(text = paste0("P(X < ", perc, ") = ", pp * 100, "%"), side = 3)
    text(x = perc - chisd, y = max(dchisq(l, df)) + 0.02, labels = paste0(pp * 100, "%"), col = "#0000CD", cex = 0.6)
    text(x = perc + chisd, y = max(dchisq(l, df)) + 0.02, labels = paste0(round((1 - pp) * 100, 2), "%"), col = "#6495ED", cex = 0.6)
  } else {
    mtext(text = paste0("P(X > ", perc, ") = ", pp * 100, "%"), side = 3)
    text(x = perc - chisd, y = max(dchisq(l, df)) + 0.02, labels = paste0(round((1 - pp) * 100, 2), "%"), col = "#6495ED", cex = 0.6)
    text(x = perc + chisd, y = max(dchisq(l, df)) + 0.02, labels = paste0(pp * 100, "%"), col = "#0000CD", cex = 0.6)
  }


  axis(1, at = seq(0, l[ln], 5), labels = seq(0, l[ln], 5))

  for (i in seq_len(length(l1))) {
    pol_chi(lc[l1[i]], lc[l2[i]], df, col = col[i])
  }

  pln <- length(pp)

  for (i in seq_len(pln)) {
    abline(v = perc[i], lty = 3, lwd = 2)
    points(
      x = perc[i], y = min(dchisq(l, df)),
      type = "p", pch = 4, cex = 2
    )
    mtext(
      side = 1, text = perc[i], outer = FALSE, at = perc[i],
      line = 0.3, col = "#4B0082", cex = 0.8
    )
  }

  result <- list(prob = pp, mean = chim, stdev = chisd)
  invisible(result)
}

#' @export
#' @rdname dist_chi_plot
#' @usage NULL
#'
chi_prob <- function(perc, df, type = c("lower", "upper")) {
  .Deprecated("dist_chi_prob()")
  dist_chi_prob(perc, df, type)
}
