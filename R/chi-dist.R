#' @importFrom stats dchisq dnorm
#' @importFrom graphics plot lines polygon points
#' @title Visualize Chi Square Distribution
#' @description Visualize how changes in degrees of freedom affect the shape of
#' the chi square distribution.
#' @param df degrees of freedom
#' @param normal logical; if \code{TRUE}, normal curve with same \code{mean} and
#' \code{sd} as the chi square distribution is drawn
#' @examples
#' chi_plot()
#' chi_plot(df = 5)
#' chi_plot(df = 5, normal = TRUE)
#' @seealso \code{\link{chi_per}} \code{\link{chi_prob}}
#' \code{\link[stats]{Chisquare}}
#' @export
#'
chi_plot <- function(df = 3, normal = FALSE) {

    if (!is.numeric(df)) {
      stop('df must be numeric/integer')
    }

    if (!is.logical(normal)) {
      stop('normal must be logical')
    }

    df <- as.integer(df)

    chim  <- round(df, 3)
    chisd <- round(sqrt(2 * df), 3)

    x <- seq(0, 25, 0.01)
    plot(x, dchisq(x, df),
         type = 'l',
         lwd  = 2,
         col  = 'blue',
         xlab = '',
         ylab = '',
         xaxt = 'n',
         yaxt = 'n',
         main = paste('Chi Square Distribution: df =', df),
         sub  = paste('Mean =', chim, ' Std Dev. =', chisd),
         bty  = 'n')

    if (normal == TRUE) {

        lines(x, dnorm(x, chim, chisd), lty = 2, col = "#FF4500")
        y <- c(0, seq(0, 25, 0.01), 25)
        z <- c(0, dnorm(seq(0, 25, 0.01), chim, chisd), 0)
        polygon(y, z, col = "#FF4500")

    }

    y <- c(0, seq(0, 25, 0.01), 25)
    z <- c(0, dchisq(seq(0, 25, 0.01), df), 0)
    polygon(y, z, col = "#4682B4")

    mtext(text = paste('df =', df), side = 3)
    axis(1, at = (0:25), labels = (0:25))

    points(x = chim, y = min(dchisq(x, df)),
           type = 'p', pch = 4, cex = 2, col = '#FF4500')

    mtext(side = 1, text = expression(paste(mu)), outer = FALSE, at = chim,
        line = 0.5, col = "#4B0082")

    result <- list(mean = chim, stdev = chisd)
    return(result)

}


#' @importFrom stats qchisq
#' @importFrom graphics text abline
#' @title Chi Square Distribution Percentile
#' @description Visualize the percentile from the value of the lower/upper
#' cumulative distribution function of the chi square distribution
#' @param probs a probability value
#' @param df degrees of freedom
#' @param type lower tail or upper tail
#' @return percentile for the \code{probs} based on \code{df} and \code{type}
#' @examples
#' chi_per(0.165, 8, 'upper')
#' chi_per(0.22, 13, 'upper')
#' @seealso \code{\link{chi_plot}} \code{\link{chi_prob}}
#' \code{\link[stats]{Chisquare}}
#' @export
#'
chi_per <- function(probs = 0.95, df = 3, type = c("lower", "upper")) {

  if(!is.numeric(probs)) {
    stop('probs must be numeric')
  }

  if (!is.numeric(df)) {
    stop('df must be numeric/integer')
  }

  if((probs < 0) | (probs > 1)) {
    stop('probs must be between 0 and 1')
  }

  df <- as.integer(df)

  method <- match.arg(type)

  chim  <- round(df, 3)
  chisd <- round(sqrt(2 * df), 3)

  l   <- seq(0, 25, 0.01)
  ln  <- length(l)

  if (method == 'lower') {

    pp   <- round(qchisq(probs, df), 3)
    lc   <- c(l[1], pp, l[ln])
    col  <- c("#0000CD", "#6495ED")
    l1   <- c(1, 2)
    l2   <- c(2, 3)

  } else {

    pp   <- round(qchisq(probs, df, lower.tail = F), 3)
    lc   <- c(l[1], pp, l[ln])
    col  <- c("#6495ED", "#0000CD")
    l1   <- c(1, 2)
    l2   <- c(2, 3)

  }

  plot(l, dchisq(l, df),
         type = 'l',
         lwd  = 2,
         col  = 'blue',
         xlab = '',
         ylab = '',
         xaxt = 'n',
         yaxt = 'n',
         xlim = c(0, 30),
         ylim = c(0, max(dchisq(l, df)) + 0.03),
         main = paste('Chi Square Distribution: df =', df),
         sub  = paste('Mean =', chim, ' Std Dev. =', chisd),
         bty  = 'n')


  if (method == "lower") {

    mtext(text = paste0('P(X < ', pp, ') = ', probs * 100, '%'), side = 3)
    text(x = pp - 0.75, y = max(dchisq(l, df)), labels = paste0(probs * 100, '%'), col = "#0000CD")
    text(x = pp + 0.75, y = max(dchisq(l, df)), labels = paste0((1 - probs) * 100, '%'), col = "#6495ED")

  } else {

    mtext(text = paste0('P(X > ', pp, ') = ', probs * 100, '%'), side = 3)
    text(x = pp - 0.75, y = max(dchisq(l, df)), labels = paste0((1 - probs) * 100, '%'), col = "#6495ED")
    text(x = pp + 0.75, y = max(dchisq(l, df)), labels = paste0(probs * 100, '%'), col = "#0000CD")

  }


  axis(1, at = seq(0, 30, 5), labels = seq(0, 30, 5))

  for (i in seq_len(length(l1))) {
      pol_chi(lc[l1[i]], lc[l2[i]], df, col = col[i])
  }

  pln <- length(pp)

  for (i in seq_len(pln)) {

    abline(v = pp[i], lty = 3, lwd = 2)
    points(x = pp[i], y = min(dchisq(l, df)),
           type = 'p', pch = 4, cex = 2)
    mtext(side = 1, text = pp[i], outer = FALSE, at = pp[i],
          line = 0.3, col = "#4B0082", cex = 0.8)

  }

  result <- list(x = pp, mean = chim, stdev = chisd)
  return(result)

}


#' @importFrom stats pchisq
#' @title Area Under Chi Square Distribution
#' @description Visualize area under chi square distribution
#' @param perc percentile
#' @param df degrees of freedom
#' @param type lower tail or upper tail
#' @return probability value for \code{perc} based on \code{df} and \code{type}
#' @examples
#' chi_prob(13.58, 11, 'upper')
#' chi_prob(15.72, 13, 'upper')
#' @seealso \code{\link{chi_plot}} \code{\link{chi_per}}
#' \code{\link[stats]{Chisquare}}
#' @export
#'
chi_prob <- function(perc, df, type = c("lower", "upper")) {

   if (!is.numeric(df)) {
     stop('df must be numeric/integer')
   }

  if (!is.numeric(perc)) {
    stop('perc must be numeric/integer')
  }

  method <- match.arg(type)

  chim  <- round(df, 3)
  chisd <- round(sqrt(2 * df), 3)

  l   <- seq(0, 25, 0.01)
  ln  <- length(l)

  if (method == 'lower') {

    pp   <- round(pchisq(perc, df), 3)
    lc   <- c(l[1], perc, l[ln])
    col  <- c("#0000CD", "#6495ED")
    l1   <- c(1, 2)
    l2   <- c(2, 3)

  } else {

    pp   <- round(pchisq(perc, df, lower.tail = F), 3)
    lc   <- c(l[1], perc, l[ln])
    col  <- c("#6495ED", "#0000CD")
    l1   <- c(1, 2)
    l2   <- c(2, 3)

  }

  plot(l, dchisq(l, df),
         type = 'l',
         lwd  = 2,
         col  = 'blue',
         xlab = '',
         ylab = '',
         xaxt = 'n',
         yaxt = 'n',
         xlim = c(0, 30),
         ylim = c(0, max(dchisq(l, df)) + 0.03),
         main = paste('Chi Square Distribution: df =', df),
         sub  = paste('Mean =', chim, ' Std Dev. =', chisd),
         bty  = 'n')


  if (method == "lower") {

    mtext(text = paste0('P(X < ', perc, ') = ', pp * 100, '%'), side = 3)
    text(x = perc - 0.75, y = max(dchisq(l, df)), labels = paste0(pp * 100, '%'), col = "#0000CD")
    text(x = perc + 0.75, y = max(dchisq(l, df)), labels = paste0(round((1 - pp) * 100, 2), '%'), col = "#6495ED")

  } else {

    mtext(text = paste0('P(X > ', perc, ') = ', pp * 100, '%'), side = 3)
    text(x = perc - 0.75, y = max(dchisq(l, df)), labels = paste0(round((1 - pp) * 100, 2), '%'), col = "#6495ED")
    text(x = perc + 0.75, y = max(dchisq(l, df)), labels = paste0(pp * 100, '%'), col = "#0000CD")

  }


  axis(1, at = seq(0, 30, 5), labels = seq(0, 30, 5))

  for (i in seq_len(length(l1))) {
      pol_chi(lc[l1[i]], lc[l2[i]], df, col = col[i])
  }

  pln <- length(pp)

  for (i in seq_len(pln)) {

    abline(v = perc[i], lty = 3, lwd = 2)
    points(x = perc[i], y = min(dchisq(l, df)),
           type = 'p', pch = 4, cex = 2)
    mtext(side = 1, text = perc[i], outer = FALSE, at = perc[i],
          line = 0.3, col = "#4B0082", cex = 0.8)

  }

  result <- list(prob = pp, mean = chim, stdev = chisd)
  return(result)

}
