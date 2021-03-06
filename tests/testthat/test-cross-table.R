context("ds_cross_table")

test_that("output from ds_cross_table matches expected results", {
  k <- ds_cross_table(hsb, ses, female)

  expect_equal(k$utility$obs, 200)

  expect_equivalent(k$utility$var2_levels, c("0", "1"))

  expect_equivalent(k$utility$var1_levels, c("1", "2", "3"))

  expect_equivalent(k$utility$varnames, c("ses", "female"))

  expect_equivalent(k$utility$twowaytable[, 1], c("1", "2", "3"))

  expect_equivalent(k$utility$twowaytable[, 2], c("15", "47", "29"))

  expect_equivalent(k$utility$twowaytable[, 3], c("32", "48", "29"))

  expect_equivalent(k$utility$twowaytable[, 4], c("47", "95", "58"))

  expect_equivalent(colnames(k$utility$twowaytable), c("", "0", "1", "rowtotal"))

  expect_equivalent(k$utility$percent_table[, 1], c(0.075, 0.235, 0.145, 0.455))

  expect_equivalent(k$utility$percent_table[, 2], c(0.160, 0.240, 0.145, 0.545))

  expect_equivalent(k$utility$percent_table[, 3], c(0.235, 0.475, 0.290, 1.000))

  expect_equivalent(colnames(k$utility$percent_table), c("0", "1", "row_pct"))

  expect_equivalent(rownames(k$utility$percent_table), c("", "", "", "col_pct"))

  expect_equivalent(k$utility$row_percent[, 1], c(0.32, 0.49, 0.50))

  expect_equivalent(k$utility$row_percent[, 2], c(0.68, 0.51, 0.50))

  expect_equivalent(k$utility$row_percent[3, 3], c(0.29))

  expect_equivalent(k$utility$column_percent[, 1], c(0.16, 0.52, 0.32))

  expect_equivalent(k$utility$column_percent[, 2], c(0.29, 0.44, 0.27))

  expect_equivalent(unname(k$utility$column_totals), c(91, 109))

  expect_equivalent(unname(k$utility$percent_column), c(0.455, 0.545))
})


test_that("ouput from plot.ds_cross_table matches expected output", {
  skip_on_cran()

  # cross table
  k <- ds_cross_table(mtcarz, cyl, gear)

  # bar plot
  p1 <- plot(k)
  expect_doppelganger("cross_bar", p1$plot)

  # stacked bar plot
  p2 <- plot(k, stacked = TRUE)
  expect_doppelganger("cross_bar_stacked", p2$plot)

  # proportional bar plot
  p3 <- plot(k, proportional = TRUE)
  expect_doppelganger("cross_bar_proportional", p3$plot)
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

  pim <- "    Cell Contents
 |---------------|
 |     Frequency |
 |       Percent |
 |       Row Pct |
 |       Col Pct |
 |---------------|

 Total Observations:  32 

----------------------------------------------------------------------------
|              |                           gear                            |
----------------------------------------------------------------------------
|          cyl |            3 |            4 |            5 |    Row Total |
----------------------------------------------------------------------------
|            4 |            1 |            8 |            2 |           11 |
|              |        0.031 |         0.25 |        0.062 |              |
|              |         0.09 |         0.73 |         0.18 |         0.34 |
|              |         0.07 |         0.67 |          0.4 |              |
----------------------------------------------------------------------------
|            6 |            2 |            4 |            1 |            7 |
|              |        0.062 |        0.125 |        0.031 |              |
|              |         0.29 |         0.57 |         0.14 |         0.22 |
|              |         0.13 |         0.33 |          0.2 |              |
----------------------------------------------------------------------------
|            8 |           12 |            0 |            2 |           14 |
|              |        0.375 |            0 |        0.062 |              |
|              |         0.86 |            0 |         0.14 |         0.44 |
|              |          0.8 |            0 |          0.4 |              |
----------------------------------------------------------------------------
| Column Total |           15 |           12 |            5 |           32 |
|              |        0.468 |        0.375 |        0.155 |              |
----------------------------------------------------------------------------"

expect_output(print(ds_cross_table(mtcarz, cyl, gear)), pim)

})

test_that("get_names works as expected", {

  mt       <- mtcarz["gear"]
  mt$gear  <- as.character(mt$gear)
  actual   <- get_names(mt$gear)
  expected <- c("3", "4", "5")
  expect_equal(actual, expected)

})