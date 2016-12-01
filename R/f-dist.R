#' @importFrom stats df
#' @title Visualize F Distribution
#' @description Visualize how changes in degrees of freedom affect the
#' shape of the F distribution.
#' @param num_df degrees of freedom associated with the numerator of f statistic
#' @param den_df degrees of freedom associated with the denominator of f statistic
#' @param normal logical; if \code{TRUE}, normal curve with same \code{mean} and
#' \code{sd} as the F distribution is drawn.
#' @examples
#' f_plot()
#' f_plot(6, 10, normal = TRUE)
#' @seealso \code{\link{f_per}} \code{\link{f_prob}}
#' \code{\link[stats]{FDist}}
#' @export
#'
f_plot <- function(num_df = 4, den_df = 30, normal = FALSE) {

    if (!is.numeric(num_df)) {
      stop('Numerator DF must be numeric/integer')
    }

    if (!is.numeric(den_df)) {
      stop('Denominator DF must be numeric/integer')
    }

    if (!is.logical(normal)) {
      stop('input for normal must be logical')
    }

    num_df <- as.integer(num_df)
    den_df <- as.integer(den_df)

    # mean and sd
    fm  <- round(den_df / (den_df - 2), 3)
    fsd <- round(sqrt((2 * (fm ^ 2) * (num_df + den_df - 2)) / (num_df * (den_df - 4))), 3)

    x <- seq(0, 4, 0.01)
    plot(x, df(x, num_df, den_df),
         type = 'l',
         lwd  = 2,
         col  = 'blue',
         xlab = '',
         ylab = '',
         xaxt = 'n',
         yaxt = 'n',
         main = 'f distribution',
         sub  = paste('Mean =', fm, ' Std Dev. =', fsd),
         bty  = 'n')

    if (normal == TRUE) {

        lines(x, dnorm(x, fm, fsd), lty = 2, col = "#FF4500")
        y <- c(0, seq(0, 4, 0.01), 4)
        z <- c(0, dnorm(seq(0, 4, 0.01), fm, fsd), 0)
        polygon(y, z, col = "#FF4500")

    }

    y <- c(0, seq(0, 4, 0.01), 4)
    z <- c(0, df(seq(0, 4, 0.01), num_df, den_df), 0)
    polygon(y, z, col = "#4682B4")

    mtext(text = paste('Num df =', num_df, '  Den df =', den_df), side = 3)
    axis(1, at = (0:4), labels = (0:4))

    points(x = fm, y = min(df(x, num_df, den_df)),
           type = 'p', pch = 4, cex = 2, col = '#FF4500')

    mtext(side = 1, text = expression(paste(mu)), outer = FALSE, at = fm,
        line = 0.5, col = "#4B0082")

    result <- list(mean = fm, stdev = fsd)
    invisible(result)

}


#' @importFrom stats qf
#' @title Visualize F Distribution Percentile
#' @description Calculate and visualize quantiles out of given probability
#' @param probs a probability value
#' @param num_df degrees of freedom associated with the numerator of f statistic
#' @param den_df degrees of freedom associated with the denominator of f statistic
#' @param type lower tail or upper tail
#' @return percentile for the \code{probs} based on \code{num_df}, \code{den_df}
#' and \code{type}
#' @examples
#' f_per(0.95, 3, 30, 'lower')
#' f_per(0.125, 9, 35, 'upper')
#' @seealso \code{\link{f_plot}} \code{\link{f_prob}} \code{\link[stats]{FDist}}
#' @export
#'
f_per <- function(probs = 0.95, num_df = 3, den_df = 30, type = c("lower", "upper")) {

  if (!is.numeric(num_df)) {
    stop('Numerator DF must be numeric/integer')
  }

  if (!is.numeric(den_df)) {
    stop('Denominator DF must be numeric/integer')
  }

  if(!is.numeric(probs)) {
    stop('probs must be numeric')
  }

  if((probs < 0) | (probs > 1)) {
    stop('probs must be between 0 and 1')
  }

  num_df <- as.integer(num_df)
  den_df <- as.integer(den_df)

  method <- match.arg(type)

  # mean and sd
  fm  <- round(den_df / (den_df - 2), 3)
  fsd <- round(sqrt((2 * (fm ^ 2) * (num_df + den_df - 2)) / (num_df * (den_df - 4))), 3)

  l   <- seq(0, 4, 0.01)
  ln  <- length(l)

  if (method == 'lower') {

    pp   <- round(qf(probs, num_df, den_df), 3)
    lc   <- c(l[1], pp, l[ln])
    col  <- c("#0000CD", "#6495ED")
    l1   <- c(1, 2)
    l2   <- c(2, 3)

  } else {

    pp   <- round(qf(probs, num_df, den_df, lower.tail = F), 3)
    lc   <- c(l[1], pp, l[ln])
    col  <- c("#6495ED", "#0000CD")
    l1   <- c(1, 2)
    l2   <- c(2, 3)

  }

  plot(l, df(l, num_df, den_df),
         type = 'l',
         lwd  = 2,
         col  = 'blue',
         xlab = '',
         ylab = '',
         xaxt = 'n',
         yaxt = 'n',
         xlim = c(0, 5),
         ylim = c(0, max(df(l, num_df, den_df)) + 0.03),
         main = 'f distribution',
         sub  = paste('Mean =', fm, ' Std Dev. =', fsd),
         bty  = 'n')


  if (method == "lower") {

    mtext(text = paste0('P(X < ', pp, ') = ', probs * 100, '%'), side = 3)
    text(x = pp - 0.2, y = max(df(l, num_df, den_df)) + 0.02, labels = paste0(probs * 100, '%'), col = "#0000CD", cex = 0.6)
    text(x = pp + 0.2, y = max(df(l, num_df, den_df)) + 0.02, labels = paste0((1 - probs) * 100, '%'), col = "#6495ED", cex = 0.6)

  } else {

    mtext(text = paste0('P(X > ', pp, ') = ', probs * 100, '%'), side = 3)
    text(x = pp - 0.2, y = max(df(l, num_df, den_df)) + 0.02, labels = paste0((1 - probs) * 100, '%'), col = "#6495ED", cex = 0.6)
    text(x = pp + 0.2, y = max(df(l, num_df, den_df)) + 0.02, labels = paste0(probs * 100, '%'), col = "#0000CD", cex = 0.6)

  }


  axis(1, at = (0:5), labels = (0:5))

  for (i in seq_len(length(l1))) {
      pol_f(lc[l1[i]], lc[l2[i]], num_df, den_df, col = col[i])
  }

  pln <- length(pp)

  for (i in seq_len(pln)) {

    abline(v = pp[i], lty = 3, lwd = 2)
    points(x = pp[i], y = min(df(l, num_df, den_df)),
           type = 'p', pch = 4, cex = 2)
    mtext(side = 1, text = pp[i], outer = FALSE, at = pp[i],
          line = 0.3, col = "#4B0082", cex = 0.8)

  }

  result <- list(x = pp, mean = fm, stdev = fsd)
  invisible(result)

}


#' @importFrom stats pf
#' @title Visualize F Distribution Probabilities
#' @description Calculate and visualize probability from a given quantile
#' @param perc a quantile value
#' @param num_df degrees of freedom associated with the numerator of f statistic
#' @param den_df degrees of freedom associated with the denominator of f statistic
#' @param type lower tail or upper tail
#' @return probability value for \code{perc} based on \code{num_df}, \code{den_df}
#' and \code{type}
#' @examples
#' f_prob(2.35, 5, 32)
#' f_prob(1.5222, 9, 35, type = "upper")
#' @seealso \code{\link{f_plot}} \code{\link{f_per}} \code{\link[stats]{FDist}}
#' @export
#'
f_prob <- function(perc, num_df, den_df, type = c("lower", "upper")) {

  if (!is.numeric(perc)) {
    stop('perc must be numeric/integer')
  }

  if (!is.numeric(num_df)) {
    stop('num_df must be numeric/integer')
  }

  if (!is.numeric(den_df)) {
    stop('den_df must be numeric/integer')
  }

  num_df <- as.integer(num_df)
  den_df <- as.integer(den_df)

  method <- match.arg(type)

  # mean and sd
  fm  <- round(den_df / (den_df - 2), 3)
  fsd <- round(sqrt((2 * (fm ^ 2) * (num_df + den_df - 2)) / (num_df * (den_df - 4))), 3)

  l <- if (perc < 4) {
    seq(0, 4, 0.01)
  } else {
    seq(0, (perc * 1.25), 0.01)
  }
  ln  <- length(l)

  if (method == 'lower') {

    pp   <- round(pf(perc, num_df, den_df), 3)
    lc   <- c(l[1], perc, l[ln])
    col  <- c("#0000CD", "#6495ED")
    l1   <- c(1, 2)
    l2   <- c(2, 3)

  } else {

    pp   <- round(pf(perc, num_df, den_df, lower.tail = F), 3)
    lc   <- c(l[1], perc, l[ln])
    col  <- c("#6495ED", "#0000CD")
    l1   <- c(1, 2)
    l2   <- c(2, 3)

  }

  plot(l, df(l, num_df, den_df),
         type = 'l',
         lwd  = 2,
         col  = 'blue',
         xlab = '',
         ylab = '',
         xaxt = 'n',
         yaxt = 'n',
         xlim = c((0 - (1.5 * fsd)), l[ln]),
         ylim = c(0, max(df(l, num_df, den_df)) + 0.05),
         main = 'f distribution',
         sub  = paste('Mean =', fm, ' Std Dev. =', fsd),
         bty  = 'n')


  if (method == "lower") {

    mtext(text = paste0('P(X < ', perc, ') = ', pp * 100, '%'), side = 3)
    text(x = perc - fsd, y = max(df(l, num_df, den_df)) + 0.04, labels = paste0(pp * 100, '%'), col = "#0000CD", cex = 0.6)
    text(x = perc + fsd, y = max(df(l, num_df, den_df)) + 0.04, labels = paste0(round((1 - pp) * 100, 2), '%'), col = "#6495ED", cex = 0.6)

  } else {

    mtext(text = paste0('P(X > ', perc, ') = ', pp * 100, '%'), side = 3)
    text(x = perc - fsd, y = max(df(l, num_df, den_df)) + 0.04, labels = paste0(round((1 - pp) * 100, 2), '%'), col = "#6495ED", cex = 0.6)
    text(x = perc + fsd, y = max(df(l, num_df, den_df)) + 0.04, labels = paste0(pp * 100, '%'), col = "#0000CD", cex = 0.6)

  }


  axis(1, at = (0:max(l)), labels = (0:max(l)))

  for (i in seq_len(length(l1))) {
      pol_f(lc[l1[i]], lc[l2[i]], num_df, den_df, col = col[i])
  }

  pln <- length(pp)

  for (i in seq_len(pln)) {

    abline(v = perc[i], lty = 3, lwd = 2)
    points(x = perc[i], y = min(df(l, num_df, den_df)),
           type = 'p', pch = 4, cex = 2)
    mtext(side = 1, text = perc[i], outer = FALSE, at = perc[i],
          line = 0.3, col = "#4B0082", cex = 0.8)

  }

  result <- list(probs = pp, mean = fm, stdev = fsd)
  invisible(result)

}
