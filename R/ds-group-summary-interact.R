#' Category wise descriptive statistics
#'
#' Descriptive statistics of a continuous variable for the combination of levels
#' of two or more categorical variables.
#'
#' @param data A \code{data.frame} or a \code{tibble}.
#' @param col Column in \code{data}; continuous variable.
#' @param ... Columns in \code{data}; categorical variables.
#'
#' @details Character grouping columns are accepted and treated as factors.
#' Factor levels may contain any character, including \code{.}.
#'
#' @examples
#' ds_group_summary_interact(mtcarz, mpg, cyl, gear)
#'
#' @seealso \code{\link{ds_group_summary}}
#'
#' @export
#'
ds_group_summary_interact <- function(data, col, ...) {

  check_df(data)

  cvar_name <- deparse(substitute(col))
  c_var <- rlang::enquo(col)
  check_numeric(data, !! c_var, cvar_name)

  g_var <- rlang::quos(...)
  gdata <- dplyr::select(data, !!! g_var)

  # grouping columns must be categorical; character columns are accepted and
  # treated as factors rather than being silently dropped
  is_cat   <- vapply(gdata, function(x) is.factor(x) || is.character(x), logical(1))
  non_type <- names(gdata)[!is_cat]

  error_message <- paste0("Below grouping variables are not categorical: \n",
                    paste("-", non_type, collapse = "\n"))

  if (length(non_type) > 0) {
        stop(error_message, call. = FALSE)
  }

  cnames <- names(gdata)

  # group by the selected columns themselves; the labels are never joined
  # into (and split out of) a string, so distinct tuples whose labels would
  # serialize to the same text cannot be merged
  cname <- names(dplyr::select(data, !! c_var))

  gdata_num <-
    data %>%
    dplyr::select(!!! g_var, !! c_var) %>%
    na.omit()

  cvec <- gdata_num[[cname]]
  gid  <- interaction(gdata_num[cnames], drop = TRUE)

  stat_names <- c(
    "min", "max", "mean", "t_mean", "median", "mode", "range", "variance",
    "stdev", "skew", "kurtosis", "coeff_var", "q1", "q3", "iqrange"
  )

  group_stats <- function(x) {
    stats::setNames(
      c(
        min(x), max(x), mean(x), trimmed_mean(x), median(x), ds_mode(x),
        ds_range(x), var(x), sd(x), ds_skewness(x), ds_kurtosis(x),
        ds_cvar(x), quant1(x), quant3(x), IQR(x)
      ),
      stat_names
    )
  }

  indexes <- split(seq_len(nrow(gdata_num)), gid)

  stat_mat <- t(vapply(indexes, function(i) group_stats(cvec[i]),
                       numeric(length(stat_names))))
  colnames(stat_mat) <- stat_names

  # keep the first row of each group as its label
  keys <- gdata_num[vapply(indexes, `[`, integer(1), 1L), cnames, drop = FALSE]
  keys[] <- lapply(keys, as.character)

  out <- cbind(keys, as.data.frame(stat_mat))
  row.names(out) <- NULL

  dplyr::as_tibble(out, .name_repair = "minimal")
}

