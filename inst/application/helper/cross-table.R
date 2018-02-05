source('helper/utils.R')
source('helper/output.R')

ds_cross_table <- function(data, var1, var2) UseMethod("ds_cross_table")


ds_cross_table.default <- function(data, var1, var2) {

  var_names <-
    data %>%
    select(!! sym(var1), !! sym(var2)) %>%
    names

  varone <-
    data %>%
    pull(!! sym(var1))

  vartwo <-
    data %>%
    pull(!! sym(var2))

  x <- as.matrix(table(varone, vartwo))
  rownames(x) <- NULL
  n <- length(varone)
  if (is.factor(varone)) {
    row_name <- levels(varone)
  } else {
    row_name <- unique(sort(varone))
  }
  per_mat <- round(x/n, 3)
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

  result <- list(obs = n, var2_levels = col_name, var1_levels = row_name, varnames = var_names,
                 twowaytable = x, percent_table = per_mat, row_percent = rcent, column_percent = ccent,
                 column_totals = coltotal, percent_column = col_pct, data = data)


  class(result) <- "ds_cross_table"
  return(result)
}

print.ds_cross_table <- function(x, ...) {
  print_cross(x)
}

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
      table %>%
      as_tibble %>%
      ggplot(aes(x = x, y = n, fill = y)) +
      geom_bar(stat = "identity", position = "fill") +
      scale_y_continuous(labels=percent_format()) +
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
