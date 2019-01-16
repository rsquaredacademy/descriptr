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
  vistributions::vdist_normal_plot(mean, sd)
  warning('`dist_normal_plot()` has been soft deprecated and will be removed in the next version of descriptr. Please use the vistributions package for visualizing probability distributions.')
}

#' @rdname dist_norm_plot
#' @export
#'
dist_norm_perc <- function(probs = 0.95, mean = 0, sd = 1, type = c("lower", "upper", "both")) {
  vistributions::vdist_normal_perc(probs, mean, sd, type)
  warning('`dist_normal_perc()` has been soft deprecated and will be removed in the next version of descriptr. Please use the vistributions package for visualizing probability distributions.')
}

#' @rdname dist_norm_plot
#' @export
#'
dist_norm_prob <- function(perc, mean = 0, sd = 1, type = c("lower", "upper", "both")) {
  vistributions::vdist_normal_prob(perc, mean, sd, type)
  warning('`dist_normal_prob()` has been soft deprecated and will be removed in the next version of descriptr. Please use the vistributions package for visualizing probability distributions.')
}
