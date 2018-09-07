cross_table2 <- function(var1, var2, name_1, name_2) UseMethod("cross_table2")

cross_table2.default <- function(var1, var2, name_1, name_2) {

  var_1       <- deparse(substitute(var1))
  var_2       <- deparse(substitute(var2))
  var_na      <- c(var_1, var_2)
  var_names   <- c(name_1, name_2)
  x           <- as.matrix(table(var1, var2))
  n           <- sum(x)
  row_name    <- levels(var1)
  per_mat     <- round(x / n, 3)
  per_mat_row <- apply(per_mat, 1, sum)
  per_mat_col <- apply(per_mat, 2, sum)
  per_mat     <- cbind(per_mat, per_mat_row)
  per_mat     <- suppressWarnings(rbind(per_mat, per_mat_col))
  rowtotal    <- apply(x, 1, sum)
  coltotal    <- apply(x, 2, sum)
  rcent       <- row_pct(x, rowtotal)
  rcent       <- cbind(rcent, per_mat_row)
  rcent       <- apply(rcent, c(1, 2), rounda)
  ccent       <- col_pct(x, coltotal)
  ccent       <- apply(ccent, c(1, 2), rounda)
  x           <- cbind(x, rowtotal)
  x           <- cbind(row_name, x)
  col_name    <- levels(var2)
  col_names   <- c(var_names[1], col_name, "Row Total")
  last_line   <- c("Column Total", coltotal, n)

  result <- list(obs             = n,
                 variable_levels = col_name,
                 row_name        = row_name,
                 variable_names  = var_names,
                 column_names    = col_names,
                 twowaytable     = x,
                 percent_table   = per_mat,
                 row_percent     = rcent,
                 column_percent  = ccent,
                 column_totals   = last_line,
                 percent_column  = per_mat_col
  )


  class(result) <- "cross_table2"
  return(result)
}


print.cross_table2 <- function(x, ...) {
  print_cross2(x)
}
