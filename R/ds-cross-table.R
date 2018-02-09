#' @importFrom graphics barplot mosaicplot
#' @importFrom grDevices rainbow
#' @title Two Way Tables
#' @description \code{ds_cross_tablecross_table} creates two way tables of categorical
#' variables. The tables created can be visualized as barplots and mosaicplots.
#' @param data a \code{data.frame} or a \code{tibble}
#' @param var1 First categorical variable
#' @param var2 Second categorical variable
#' @param x An object of class cross_table
#' @param stacked a logical value. If FALSE, the columns of height are portrayed
#' as stacked bars, and if TRUE the columns are portrayed as juxtaposed bars.
#' @param proportional a logical value. If TRUE, the height of the bars is
#' proportional
#' @param ... further arguments to be passed to or from methods.
#' @return \code{ds_cross_table} returns an object of class \code{"ds_cross_table"}.
#' An object of class \code{"ds_cross_table"} is a list containing at least the
#' following components:
#'
#' \item{obs}{obs number of observations}
#' \item{var2_levels}{levels of the second categorical variable}
#' \item{var1_levels}{levels of the first categorical variable}
#' \item{varnames}{names of the variables}
#' \item{twowaytable}{table of the variables}
#' \item{percent_table}{table of percentages}
#' \item{row_percent}{table of row percentages}
#' \item{col_percent}{table of column percentages}
#' \item{column_totals}{total of columns}
#' \item{percent_column}{total of columns as a percentage}
#' @section Deprecated Function:
#' \code{ds_cross_table()} has been deprecated. Instead use \code{ds_cross_table()}.
#' @examples
#' k <- ds_cross_table(mtcarz, cyl, am)
#' k
#'
#' # bar plots
#' plot(k)
#' plot(k, stacked = TRUE)
#' plot(k, proportional = TRUE)
#' @export
#'
ds_cross_table <- function(data, var1, var2) UseMethod("ds_cross_table")

#' @export
ds_cross_table.default <- function(data, var1, var2) {
  var_1 <- enquo(var1)
  var_2 <- enquo(var2)

  var_names <-
    data %>%
    select(!! var_1, !! var_2) %>%
    names()

  varone <-
    data %>%
    pull(!! var_1)

  vartwo <-
    data %>%
    pull(!! var_2)

  x <- as.matrix(table(varone, vartwo))
  rownames(x) <- NULL
  n <- length(varone)
  if (is.factor(varone)) {
    row_name <- levels(varone)
  } else {
    row_name <- unique(sort(varone))
  }
  per_mat <- round(x / n, 3)
  row_pct <- apply(per_mat, 1, sum)
  col_pct <- apply(per_mat, 2, sum)
  per_mat <- cbind(per_mat, row_pct)
  per_mat <- suppressWarnings(rbind(per_mat, col_pct))
  d <- dim(per_mat)
  per_mat[d[1], d[2]] <- 1
  rowtotal <- apply(x, 1, sum)
  coltotal <- apply(x, 2, sum)
  rcent <- row_pct(x, rowtotal)
  rcent <- cbind(rcent, row_pct)
  rcent <- apply(rcent, c(1, 2), rounda)
  ccent <- col_pct(x, coltotal)
  ccent <- apply(ccent, c(1, 2), rounda)
  x <- cbind(x, rowtotal)
  x <- cbind(unname(row_name), x)
  if (is.factor(vartwo)) {
    col_name <- levels(vartwo)
  } else {
    col_name <- unique(sort(vartwo))
  }

  result <- list(
    obs = n, var2_levels = col_name, var1_levels = row_name, varnames = var_names,
    twowaytable = x, percent_table = per_mat, row_percent = rcent, column_percent = ccent,
    column_totals = coltotal, percent_column = col_pct, data = data
  )


  class(result) <- "ds_cross_table"
  return(result)
}

#' @export
#' @rdname ds_cross_table
#' @usage NULL
#'
cross_table <- function(var1, var2) {
  .Deprecated("ds_cross_table()")
}

#' @export
print.ds_cross_table <- function(x, ...) {
  print_cross(x)
}


#' @importFrom ggplot2 ggplot aes geom_bar xlab ggtitle labs scale_y_continuous
#' @importFrom tibble as_tibble
#' @importFrom scales percent_format
#' @importFrom magrittr extract
#' @export
#' @rdname ds_cross_table
#'
plot.ds_cross_table <- function(x, stacked = FALSE, proportional = FALSE, ...) {
  x_lab <-
    x %>%
    use_series(varnames) %>%
    extract(1)

  y_lab <-
    x %>%
    use_series(varnames) %>%
    extract(2)

  k <- string_to_name(x)
  j <- string_to_name(x, 2)

  # proportional stacked bar plots
  if (proportional) {
    p <-
      x %>%
      use_series(data) %>%
      select(x = !! k, y = !! j) %>%
      table() %>%
      as_tibble() %>%
      ggplot(aes(x = x, y = n, fill = y)) +
      geom_bar(stat = "identity", position = "fill") +
      scale_y_continuous(labels = percent_format()) +
      xlab(x_lab) + ggtitle(paste(x_lab, "vs", y_lab)) +
      labs(fill = y_lab)
  } else {
    if (stacked) {
      p <-
        x %>%
        use_series(data) %>%
        select(x = !! k, y = !! j) %>%
        ggplot() +
        geom_bar(aes(x, fill = y), position = "stack") +
        xlab(x_lab) + ggtitle(paste(x_lab, "vs", y_lab)) +
        labs(fill = y_lab)
    } else {
      p <-
        x %>%
        use_series(data) %>%
        select(x = !! k, y = !! j) %>%
        ggplot() +
        geom_bar(aes(x, fill = y), position = "dodge") +
        xlab(x_lab) + ggtitle(paste(x_lab, "vs", y_lab)) +
        labs(fill = y_lab)
    }
  }

  print(p)
  result <- list(plot = p)
  invisible(result)
}
