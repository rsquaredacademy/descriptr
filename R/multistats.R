#' @importFrom rlang quos
#' @importFrom tidyr gather
#' @importFrom dplyr group_by summarise_all funs
#' @title Multi Variable Statistics
#' @description Summary/descriptive statistics for multiple variables
#' @param x a tibble/data.frame
#' @param ... columns in \code{x}
#' @return a tibble
#' @examples
#' multistats(mtcars, mpg, disp, hp)
#' @export
#'
multistats <- function(x, ...) {
  vars <- quos(...)
  x %>%
    select(!!!vars) %>%
    gather(vars, values) %>%
    group_by(vars) %>%
    summarise_all(funs(min = min, max = max, mean = mean, t_mean = trimmed_mean,
      median = median, mode = stat_mode, range = stat_range, variance = var,
      stdev = sd, skew = skewness, kurtosis = kurtosis, coeff_var = stat_cvar,
      q1 = quant1, q3 = quant3,iqrange = IQR)
    )
}
