#' Screen data
#'
#' Screen data  and return details such as variable names, class, levels and
#' missing values. \code{plot.ds_screener()} creates bar plots to visualize %
#' of missing observations for each variable in a data set.
#'
#' @param data A \code{tibble} or a \code{data.frame}.
#' @param x An object of class \code{ds_screener}.
#' @param ... Further arguments to be passed to or from methods.
#'
#' @return \code{ds_screener()} returns an object of class \code{"ds_screener"}.
#' An object of class \code{"ds_screener"} is a list containing the
#' following components:
#'
#' \item{Rows}{Number of rows in the data frame.}
#' \item{Columns}{Number of columns in the data frame.}
#' \item{Variables}{Names of the variables in the data frame.}
#' \item{Types}{Class of the variables in the data frame.}
#' \item{Count}{Length of the variables in the data frame.}
#' \item{nlevels}{Number of levels of a factor variable.}
#' \item{levels}{Levels of factor variables in the data frame.}
#' \item{Missing}{Number of missing observations in each variable.}
#' \item{MissingPer}{Percent of missing observations in each variable.}
#' \item{MissingTotal}{Total number of missing observations in the data frame.}
#' \item{MissingTotPer}{Total percent of missing observations in the data frame.}
#' \item{MissingRows}{Total number of rows with missing observations in the
#' data frame.}
#' \item{MissingCols}{Total number of columns with missing observations in the
#' data frame.}
#'
#' @examples
#' # screen data
#' ds_screener(mtcarz)
#' ds_screener(airquality)
#'
#' # plot
#' x <- ds_screener(airquality)
#' plot(x)
#'
#' @export
#'
ds_screener <- function(data) UseMethod("ds_screener")

#' @export
#'
ds_screener.default <- function(data) {

  check_df(data)

  rows     <- nrow(data)
  cols     <- ncol(data)
  varnames <- names(data)
  datatype <- sapply(data, class)
  counts   <- sapply(data, length)
  nlev     <- lapply(data, nlevels)
  lev      <- lapply(data, levels)

  for (i in seq_len(length(lev))) {
    if (is.null(lev[[i]])) {
      lev[[i]] <- NA
    }
  }

  mvalues    <- sapply(data, function(z) sum(is.na(z)))
  mvaluesper <- round((mvalues / counts) * 100, 2)
  mtotal     <- sum(is.na(data))
  mtotalper  <- round((mtotal / sum(counts)) * 100, 2)
  mrows      <- sum(!stats::complete.cases(data))
  mcols      <- sum(mvalues != 0)

  result <- list(Rows          = rows,
                 Columns       = cols,
                 Variables     = varnames,
                 Types         = datatype,
                 Count         = counts,
                 nlevels       = nlev,
                 levels        = lev,
                 Missing       = mvalues,
                 MissingPer    = mvaluesper,
                 MissingTotal  = mtotal,
                 MissingTotPer = mtotalper,
                 MissingRows   = mrows,
                 MissingCols   = mcols)

  class(result) <- "ds_screener"

  return(result)
}

#' @export
print.ds_screener <- function(x, ...) {
  print_screen(x)
}

#' @rdname ds_screener
#' @export
#'
plot.ds_screener <- function(x, ...) {

  `% Missing`  <- NULL
  mydat        <- data.frame(x = names(x$MissingPer), y = x$MissingPer)
  mydat$y      <- mydat$y / 100
  mydat$color  <- ifelse(mydat$y >= 0.1, ">= 10%", "< 10%")
  names(mydat) <- c("x", "y", "% Missing")

  ggplot2::ggplot(mydat) +
    ggplot2::geom_col(ggplot2::aes(x = stats::reorder(x, y), y = y, fill = `% Missing`)) +
    ggplot2::scale_y_continuous(labels = scales::percent_format()) +
    ggplot2::xlab("Column") + ggplot2::ylab("Percentage") +
    ggplot2::ggtitle("Missing Values (%)") +
    ggplot2::scale_fill_manual(values = c("green", "red"))

}
