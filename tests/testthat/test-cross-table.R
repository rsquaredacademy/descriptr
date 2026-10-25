test_that("output from ds_cross_table matches expected results", {
  k <- ds_cross_table(hsb, ses, female)

  expect_equal(k$utility$obs, 200)

  expect_equal(k$utility$var2_levels, c("0", "1"))

  expect_equal(k$utility$var1_levels, c("1", "2", "3"))

  expect_equal(k$utility$varnames, c("ses", "female"))

  expect_equal(k$utility$twowaytable[, 1], c("1", "2", "3"))

  expect_equal(k$utility$twowaytable[, 2], c("15", "47", "29"))

  expect_equal(k$utility$twowaytable[, 3], c("32", "48", "29"))

  expect_equal(k$utility$twowaytable[, 4], c("47", "95", "58"))

  expect_equal(colnames(k$utility$twowaytable), c("", "0", "1", "rowtotal"))

  expect_equal(k$utility$percent_table[, 1], c(0.075, 0.235, 0.145, 0.455), ignore_attr = TRUE)

  expect_equal(k$utility$percent_table[, 2], c(0.160, 0.240, 0.145, 0.545), ignore_attr = TRUE)

  expect_equal(k$utility$percent_table[, 3], c(0.235, 0.475, 0.290, 1.000), ignore_attr = TRUE)

  expect_equal(colnames(k$utility$percent_table), c("0", "1", "row_pct"))

  expect_equal(rownames(k$utility$percent_table), c("", "", "", "col_pct"))

  expect_equal(k$utility$row_percent[, 1], c(0.32, 0.49, 0.50))

  expect_equal(k$utility$row_percent[, 2], c(0.68, 0.51, 0.50))

  expect_equal(k$utility$row_percent[3, 3], c(0.29), ignore_attr = TRUE)

  expect_equal(k$utility$column_percent[, 1], c(0.16, 0.52, 0.32))

  expect_equal(k$utility$column_percent[, 2], c(0.29, 0.44, 0.27))

  expect_equal(unname(k$utility$column_totals), c(91, 109))

  expect_equal(unname(k$utility$percent_column), c(0.455, 0.545))
})


test_that("ouput from plot.ds_cross_table matches expected output", {
  skip_on_cran()

  # cross table
  k <- ds_cross_table(mtcarz, cyl, gear)

  # bar plot
  p1 <- plot(k)
  vdiffr::expect_doppelganger("cross_bar", p1$plot)

  # stacked bar plot
  p2 <- plot(k, stacked = TRUE)
  vdiffr::expect_doppelganger("cross_bar_stacked", p2$plot)

  # proportional bar plot
  p3 <- plot(k, proportional = TRUE)
  vdiffr::expect_doppelganger("cross_bar_proportional", p3$plot)
})

test_that("ds_cross_table throws appropriate error", {

  x <- 1:10
  expect_error(ds_cross_table(x), 'data must be a `data.frame` or `tibble`.')
  expect_error(ds_cross_table(mtcarz, mpg, cyl), 'mpg is not a categorical variable. The function expects an object of type `factor` but mpg is of type `numeric`.')
  expect_error(ds_cross_table(mtcarz, cyl, disp), 'disp is not a categorical variable. The function expects an object of type `factor` but disp is of type `numeric`.')

})

test_that("output from ds_twoway_table is as expected", {
  actual <- sum(ds_twoway_table(mtcarz, cyl, gear)[[3]])
  expected <- 32
  expect_equal(actual, expected)
})

test_that("ds_twoway_table throws appropriate error", {

  x <- 1:10
  expect_error(ds_twoway_table(x), 'data must be a `data.frame` or `tibble`.')
  expect_error(ds_twoway_table(mtcarz, mpg, cyl), 'mpg is not a categorical variable. The function expects an object of type `factor` but mpg is of type `numeric`.')
  expect_error(ds_twoway_table(mtcarz, cyl, disp), 'disp is not a categorical variable. The function expects an object of type `factor` but disp is of type `numeric`.')

})


test_that("output from ds_cross_table is as expected", {
  expect_snapshot(ds_cross_table(mtcarz, cyl, gear))
})

test_that("get_names works as expected", {

  mt       <- mtcarz["gear"]
  mt$gear  <- as.character(mt$gear)
  actual   <- get_names(mt$gear)
  expected <- c("3", "4", "5")
  expect_equal(actual, expected)

})

test_that("ds_twoway_table joins without a message", {

  expect_silent(k <- ds_twoway_table(mtcarz, cyl, gear))

  expect_equal(
    names(k),
    c("cyl", "gear", "count", "percent", "row_percent", "col_percent")
  )
  expect_equal(sum(k$count), 32)
})

test_that("ds_cross_table prints percentages but stores proportions", {

  k   <- ds_cross_table(mtcarz, cyl, gear)
  out <- capture.output(print(k))

  expect_true(any(grepl("3.12", out, fixed = TRUE)))
  expect_true(any(grepl("25.00", out, fixed = TRUE)))
  expect_true(any(grepl("9.09", out, fixed = TRUE)))
  expect_true(any(grepl("6.67", out, fixed = TRUE)))
  expect_true(any(grepl("46.88", out, fixed = TRUE)))
  expect_true(any(grepl("34.38", out, fixed = TRUE)))
  expect_false(any(grepl("0.031", out, fixed = TRUE)))

  expect_equal(unname(k$utility$percent_table[1, 1]), 0.031, ignore_attr = TRUE)
  expect_equal(unname(k$utility$row_percent[1, 1]), 0.09, ignore_attr = TRUE)
  expect_equal(unname(k$utility$column_percent[1, 1]), 0.07, ignore_attr = TRUE)
  expect_equal(unname(k$utility$percent_column[1]), 0.468, ignore_attr = TRUE)
})

test_that("ds_cross_table and ds_twoway_table treat character columns as categorical", {

  char_data <- data.frame(a = c("b", "a", "b", "c"), b = c("y", "x", "y", "x"))
  fact_data <- data.frame(a = factor(char_data$a), b = factor(char_data$b))

  k_char <- ds_cross_table(char_data, a, b)
  k_fact <- ds_cross_table(fact_data, a, b)

  expect_equal(k_char$ftable, k_fact$ftable)
  expect_equal(k_char$utility$twowaytable, k_fact$utility$twowaytable)
  expect_equal(k_char$utility$var1_levels, c("a", "b", "c"))
  expect_equal(k_char$utility$var2_levels, c("x", "y"))

  expect_equal(
    ds_twoway_table(char_data, a, b),
    ds_twoway_table(fact_data, a, b)
  )
})

test_that("ds_cross_table reports missing observations", {

  g <- mtcarz
  g$cyl[1] <- NA

  k <- ds_cross_table(g, cyl, gear)
  expect_equal(k$utility$missing, 1)
  expect_equal(k$utility$obs, 31)

  out <- capture.output(print(k))
  expect_true(any(grepl("Missing Observations:  1", out, fixed = TRUE)))

  k2 <- ds_cross_table(mtcarz, cyl, gear)
  expect_equal(k2$utility$missing, 0)
  expect_false(any(grepl("Missing Observations", capture.output(print(k2)), fixed = TRUE)))
})

test_that("printing a cross table with no rows does not warn", {

  d <- data.frame(a = factor(c(NA, NA)), b = factor(c("u", "v")))
  k <- ds_cross_table(d, a, b)

  expect_silent(out <- capture.output(print(k)))
})
