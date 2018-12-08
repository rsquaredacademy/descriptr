#' Visualize t distribution
#'
#' Visualize how degrees of freedom affect the shape of t
#' distribution, visualize quantiles out of given probability and
#' probability from a given quantile.
#'
#' @param probs Probability value.
#' @param perc Quantile value.
#' @param df Degrees of freedom.
#' @param type Lower tail, upper tail, interval or both.
#'
#' @return Percentile for the \code{probs} based on \code{df} and
#' \code{type} or probability value for the \code{perc} based on \code{df} and
#' \code{type}.
#'
#' @section Deprecated functions:
#' \code{t_plot()}, \code{t_prob()} and \code{t_per()} have been deprecated.
#' Instead use \code{dist_t_plot()}, \code{dist_t_prob()} and
#' \code{dist_t_perc()}.
#'
#' @importFrom stats dt qt pt
#'
#' @examples
#' # visualize t distribution
#' dist_t_plot()
#' dist_t_plot(6)
#' dist_t_plot(df = 8)
#'
#' # visualize quantiles out of given probability
#' dist_t_perc(probs = 0.95, df = 4, type = 'lower')
#' dist_t_perc(probs = 0.35, df = 4, type = 'upper')
#' dist_t_perc(probs = 0.69, df = 7, type = 'both')
#'
#' # visualize probability from a given quantile
#' dist_t_prob(2.045, 7, 'lower')
#' dist_t_prob(0.945, 7, 'upper')
#' dist_t_prob(1.445, 7, 'interval')
#' dist_t_prob(1.6, 7, 'both')
#'
#' @seealso \code{\link[stats]{TDist}}
#'
#' @name dist_t
NULL

#' @export
#' @rdname dist_t
#'
dist_t_plot <- function(df = 3) {
  vistributions::vdist_t_plot(df)
  warning('`dist_t_plot()` has been soft deprecated and will be removed in the next version of descriptr. Please use the vistributions package for visualizing probability distributions.')
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
  vistributions::vdist_t_perc(probs, df, type)
  warning('`dist_t_perc()` has been soft deprecated and will be removed in the next version of descriptr. Please use the vistributions package for visualizing probability distributions.')
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
  vistributions::vdist_t_prob(perc, df, type)
  warning('`dist_t_prob()` has been soft deprecated and will be removed in the next version of descriptr. Please use the vistributions package for visualizing probability distributions.')
}

#' @export
#' @rdname dist_t
#' @usage NULL
#'
t_prob <- function(perc, df, type = c("lower", "upper", "interval", "both")) {
  .Deprecated("dist_t_prob()")
  dist_t_prob(perc, df, type)
}
