#' Visualize f distribution
#'
#' @description Visualize how changes in degrees of freedom affect the
#' shape of the F distribution. Compute & visualize quantiles out of given
#' probability and probability from a given quantile.
#'
#' @param probs Probability value.
#' @param perc Quantile value.
#' @param num_df Degrees of freedom associated with the numerator of f statistic.
#' @param den_df Degrees of freedom associated with the denominator of f statistic.
#' @param normal If \code{TRUE}, normal curve with same \code{mean} and
#' \code{sd} as the F distribution is drawn.
#' @param type Lower tail or upper tail.
#'
#' @return Percentile for the \code{probs} based on \code{num_df}, \code{den_df}
#' and \code{type} or probability value for \code{perc} based on \code{num_df},
#' \code{den_df} and \code{type}.
#'
#' @examples
#' # visualize F distribution
#' dist_f_plot()
#' dist_f_plot(6, 10, normal = TRUE)
#'
#' # visualize probability from a given quantile
#' dist_f_perc(0.95, 3, 30, 'lower')
#' dist_f_perc(0.125, 9, 35, 'upper')
#'
#' # visualize quantiles out of given probability
#' dist_f_prob(2.35, 5, 32)
#' dist_f_prob(1.5222, 9, 35, type = "upper")
#'
#' @seealso \code{\link[stats]{FDist}}
#'
#' @export
#'
dist_f_plot <- function(num_df = 4, den_df = 30, normal = FALSE) {
  vistributions::vdist_f_plot(num_df, den_df, normal)
  warning('`dist_f_plot()` has been soft deprecated and will be removed in the next version of descriptr. Please use the vistributions package for visualizing probability distributions.')
}

#' @rdname dist_f_plot
#' @export
#'
dist_f_perc <- function(probs = 0.95, num_df = 3, den_df = 30, type = c("lower", "upper")) {
  vistributions::vdist_f_perc(probs, num_df, den_df, type)
  warning('`dist_f_perc()` has been soft deprecated and will be removed in the next version of descriptr. Please use the vistributions package for visualizing probability distributions.')
}

#' @rdname dist_f_plot
#' @export
#'
dist_f_prob <- function(perc, num_df, den_df, type = c("lower", "upper")) {
  vistributions::vdist_f_prob(perc, num_df, den_df, type)
  warning('`dist_f_prob()` has been soft deprecated and will be removed in the next version of descriptr. Please use the vistributions package for visualizing probability distributions.')
}

