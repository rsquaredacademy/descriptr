#' Screen data
#'
#' Screen data  and return details such as variable names, class, levels and
#' missing values. \code{plot.ds_screener()} creates bar plots to visualize %
#' of missing observations for each variable in a data set.
#'
#' @param y A \code{tibble} or a \code{data.frame}.
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
#' @section Deprecated function:
#' \code{screener()} has been deprecated. Instead
#' use \code{ds_screener()}.
#'
#' @examples
#' # screen data
#' ds_screener(mtcarz)
#'
#' @export
#'
ds_screener <- function(y) UseMethod("ds_screener")

#' @export
#'
ds_screener.default <- function(y) {
  
  if (!is.data.frame(y)) {
    stop("y must be a data frame")
  }

  rows     <- nrow(y)
  cols     <- ncol(y)
  varnames <- names(y)
  datatype <- purrr::map_chr(y, class)
  counts   <- purrr::map_int(y, length)
  nlev     <- purrr::map(y, nlevels)
  lev      <- purrr::map(y, levels)
  
  for (i in seq_len(length(lev))) {
    if (is.null(lev[[i]])) {
      lev[[i]] <- NA
    }
  }
  
  mvalues    <- purrr::map_int(y, function(z) sum(is.na(z)))
  
  mvaluesper <- 
    mvalues %>%
      magrittr::divide_by(counts) %>%
      magrittr::multiply_by(100) %>%
      round(2)
  
  mtotal <- 
    y %>%
    is.na() %>%
    sum()
  
  mtotalper <- 
    mtotal %>%
    magrittr::divide_by(sum(counts)) %>%
    magrittr::multiply_by(100) %>%
    round(2)

  mrows <- 
    y %>%
    stats::complete.cases() %>%
    `!` %>%
    sum()

  mcols <- 
    mvalues %>%
    `!=`(0) %>%
    sum()

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
#' @rdname ds_screener
#' @usage NULL
#'
screener <- function(y) {
  .Deprecated("ds_screener()")
  ds_screener(y)
}

#' @export
print.ds_screener <- function(x, ...) {
  print_screen(x)
}



#' @rdname ds_screener
#' @export
#'
plot.ds_screener <- function(x, ...) {

  dat  <- x$MissingPer
  ymax <- max(dat) * 1.5
  cols <- c("green", "red")[(dat > 10) + 1]

  h <- graphics::barplot(dat, 
               main = "Missing Values (%)",
               xlab = "Column Names", 
               ylab = "Percentage",
               col  = cols, 
               ylim = c(0, ymax))

  graphics::legend("top", 
          legend     = c("> 10%", "<= 10%"), 
          fill       = c("red", "green"),
          horiz      = TRUE, 
          title      = "% Missing", 
          cex        = 0.5, 
          text.width = 0.7)

  line_data <- cbind(h, as.vector(dat))
  graphics::text(line_data[, 1], line_data[, 2] + 2, as.vector(dat))

}
