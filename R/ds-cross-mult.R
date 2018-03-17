cross_table2 <- function(var1, var2, name_1, name_2) UseMethod("cross_table2")

cross_table2.default <- function(var1, var2, name_1, name_2) {

  # variable names
  var_1 <- deparse(substitute(var1))
  var_2 <- deparse(substitute(var2))
  var_na <- c(var_1, var_2)
  var_names <- c(name_1, name_2)

  # create twowaytable
  x <- as.matrix(table(var1, var2))

  # length of data
  n <- sum(x)

  # row names
  row_name <- levels(var1)


  # percentage matrix
  per_mat <- round(x / n, 3)

  # row total in percent
  per_mat_row <- apply(per_mat, 1, sum)

  # column total in percent
  per_mat_col <- apply(per_mat, 2, sum)

  # append the row/column total percent to the percentage matrix
  per_mat <- cbind(per_mat, per_mat_row)
  per_mat <- suppressWarnings(rbind(per_mat, per_mat_col))

  # row/column total
  rowtotal <- apply(x, 1, sum)
  coltotal <- apply(x, 2, sum)

  # row percent matrix
  rcent <- row_pct(x, rowtotal)
  rcent <- cbind(rcent, per_mat_row)
  rcent <- apply(rcent, c(1, 2), rounda)

  # column percent matrix
  ccent <- col_pct(x, coltotal)
  ccent <- apply(ccent, c(1, 2), rounda)

  x <- cbind(x, rowtotal)
  x <- cbind(row_name, x)

  # column names
  col_name <- levels(var2)
  col_names <- c(var_names[1], col_name, "Row Total")

  # part 6
  last_line <- c("Column Total", coltotal, n)

  # return the following
  result <- list(
    obs = n,
    variable_levels = col_name,
    row_name = row_name,
    variable_names = var_names,
    column_names = col_names,
    twowaytable = x,
    percent_table = per_mat,
    row_percent = rcent,
    column_percent = ccent,
    column_totals = last_line,
    percent_column = per_mat_col
  )


  class(result) <- "cross_table2"
  return(result)
}


print.cross_table2 <- function(x, ...) {
  print_cross2(x)
}
