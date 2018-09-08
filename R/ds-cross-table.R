#' Two way table
#'
#' Creates two way tables of categorical variables. The tables created can be
#' visualized as barplots and mosaicplots.
#'
#' @param data A \code{data.frame} or a \code{tibble}.
#' @param var1 First categorical variable.
#' @param var2 Second categorical variable.
#' @param x An object of class \code{cross_table}.
#' @param stacked If \code{FALSE}, the columns of height are portrayed
#' as stacked bars, and if \code{TRUE} the columns are portrayed as juxtaposed bars.
#' @param proportional If \code{TRUE}, the height of the bars is proportional.
#' @param ... Further arguments to be passed to or from methods.
#'
#' @section Deprecated function:
#' \code{ds_cross_table()} has been deprecated. Instead use
#' \code{ds_cross_table()}.
#'
#' @importFrom graphics barplot mosaicplot
#' @importFrom grDevices rainbow
#' @importFrom magrittr set_rownames divide_by
#'
#' @examples
#' k <- ds_cross_table(mtcarz, cyl, gear)
#' k
#'
#' # bar plots
#' plot(k)
#' plot(k, stacked = TRUE)
#' plot(k, proportional = TRUE)
#'
#' # alternate
#' ds_twoway_table(mtcarz, cyl, gear)
#'
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

  varone   <- pull(data, !! var_1)
  vartwo   <- pull(data, !! var_2)
  row_name <- get_names(varone)
  col_name <- get_names(vartwo)

  x <- 
    table(varone, vartwo) %>%
    as.matrix() %>%
    set_rownames(NULL)
  
  n <- sum(x)
  
  per_mat <- 
    x %>%
    divide_by(n) %>%
    round(3)

  row_pct  <- apply(per_mat, 1, sum)
  col_pct  <- apply(per_mat, 2, sum)
  rowtotal <- apply(x, 1, sum)
  coltotal <- apply(x, 2, sum)
  finalmat <- prep_per_mat(per_mat, row_pct, col_pct)  
  rcent    <- prep_rcent(x, rowtotal, row_pct)
  ccent    <- prep_ccent(x, coltotal)
  finaltab <- prep_table(x, rowtotal, row_name)
  

  result <- list(
    obs = n, var2_levels = col_name, var1_levels = row_name, varnames = var_names,
    twowaytable = finaltab, percent_table = finalmat, row_percent = rcent, column_percent = ccent,
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

#' @importFrom dplyr summarise tally ungroup mutate inner_join
#' @importFrom magrittr %<>%
#' @rdname ds_cross_table
#' @export
#'
ds_twoway_table <- function(data, var1, var2) {

  var_1 <- enquo(var1)
  var_2 <- enquo(var2)

  group <-
    data %>%
    select(!! var_1, !! var_2) %>%
    drop_na() %>%
    group_by(!! var_1, !! var_2) %>%
    summarise(count = n())

  total <-
    group %>%
    pull(count) %>%
    sum()

  div_by <-
    data %>%
    group_by(!! var_2) %>%
    drop_na() %>%
    tally() %>%
    pull(n)


  group2 <-
    data %>%
    select(!! var_1, !! var_2) %>%
    drop_na() %>%
    group_by(!! var_2, !! var_1) %>%
    summarise(count = n()) %>%
    mutate(
      col_percent = count / sum(count)
    ) %>%
    ungroup()

  group %<>%
    mutate(
      percent     = count / total,
      row_percent = count / sum(count)
    ) %>%
    ungroup()

  result <- inner_join(group, group2)
  return(result)

}

get_names <- function(x) {
  
  if (is.factor(x)) {
    varname <- levels(x)
  } else {
    varname <- 
      x %>%
      sort() %>%
      unique()
  }
  
  return(varname)
  
}

prep_table <- function(x, rowtotal, row_name) {

  x1 <- cbind(x, rowtotal)
  cbind(unname(row_name), x1)

}

prep_per_mat <- function(per_mat, row_pct, col_pct) {

  per_mat_1             <- cbind(per_mat, row_pct)
  per_mat_2             <- suppressWarnings(rbind(per_mat_1, col_pct))
  d                     <- dim(per_mat_2)
  per_mat_2[d[1], d[2]] <- 1
  return(per_mat_2)

}

prep_rcent <- function(x, rowtotal, row_pct) {

  rcent_1 <- row_pct(x, rowtotal)
  rcent_2 <- cbind(rcent_1, row_pct)
  apply(rcent_2, c(1, 2), rounda)

}

prep_ccent <- function(x, coltotal) {

  ccent_1 <- col_pct(x, coltotal)
  apply(ccent_1, c(1, 2), rounda)

}