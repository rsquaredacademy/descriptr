#' Visualize chi square distribution
#'
#' Visualize how changes in degrees of freedom affect the shape of
#' the chi square distribution. Compute & visualize quantiles out of given
#' probability and probability from a given quantile.
#'
#' @param df Degrees of freedom.
#' @param probs Probability value.
#' @param perc Quantile value.
#' @param type Lower tail or upper tail.
#' @param normal If \code{TRUE}, normal curve with same \code{mean} and
#' \code{sd} as the chi square distribution is drawn.
#'
#' @return Percentile for the \code{probs} based on \code{df} and \code{type} or
#' probability value for \code{perc} based on \code{df} and \code{type}.
#'
#' @section Deprecated functions:
#' \code{chi_plot()}, \code{chi_prob()} and \code{chi_per()} have been
#' deprecated. Instead use \code{dist_chi_plot()}, \code{dist_chi_prob()} and
#' \code{dist_chi_perc()}.
#'
#' @importFrom stats dchisq qchisq pchisq dnorm
#' @importFrom graphics plot lines polygon points text abline
#'
#' @examples
#' # visualize chi square distribution
#' dist_chi_plot()
#' dist_chi_plot(df = 5)
#' dist_chi_plot(df = 5, normal = TRUE)
#'
#' # visualize quantiles out of given probability
#' dist_chi_perc(0.165, 8, 'upper')
#' dist_chi_perc(0.22, 13, 'upper')
#'
#' # visualize probability from a given quantile.
#' dist_chi_prob(13.58, 11, 'lower')
#' dist_chi_prob(15.72, 13, 'upper')
#'
#' @seealso \code{\link[stats]{Chisquare}}
#'
#' @export
#'
dist_chi_plot <- function(df = 3, normal = FALSE) {
  vistributions::vdist_chisquare_plot(df, normal) 
  warning('`dist_chi_plot()` has been soft deprecated and will be removed in the next version of descriptr. Please use the vistributions package for visualizing probability distributions.')
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
  print(vistributions::vdist_chisquare_perc(probs, df, type))
  warning('`dist_chi_perc()` has been soft deprecated and will be removed in the next version of descriptr. Please use the vistributions package for visualizing probability distributions.')
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
  print(vistributions::vdist_chisquare_prob(perc, df, type))
  warning('`dist_chi_prob()` has been soft deprecated and will be removed in the next version of descriptr. Please use the vistributions package for visualizing probability distributions.')
}

#' @export
#' @rdname dist_chi_plot
#' @usage NULL
#'
chi_prob <- function(perc, df, type = c("lower", "upper")) {
  .Deprecated("dist_chi_prob()")
  dist_chi_prob(perc, df, type)
}
