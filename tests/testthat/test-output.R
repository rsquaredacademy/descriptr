mt <- mtcars
mt$cyl <- as.factor(mt$cyl)
mt$vs <- as.factor(mt$vs)

test_that("output from print_cross matches expected output", {
  expect_snapshot(ds_cross_table(mt, cyl, vs))
})

test_that("output from print_screener matches expected output", {
  mt <- mtcars
  mt[, c(2, 8:11)] <- lapply(mt[, c(2, 8:11)], factor)
  expect_snapshot(ds_screener(mt))
})


test_that("output from print_fcont matches the expected result", {
  expect_snapshot(ds_freq_table(mtcars, mpg))
})

test_that("output from print_fcont is as expected when data has missing values", {
  mt <- mtcarz
  mt$mpg[c(3, 10, 14, 19)] <- NA
  expect_snapshot(ds_freq_table(mtcars, mpg))
})


test_that("output from freq_table matches the expected result", {
  expect_snapshot(ds_freq_table(mt, cyl))
})

test_that("output from freq_table is as expected when data has missing values", {
  mt <- mtcarz
  mt$cyl[c(3, 10, 14, 19)] <- NA
  expect_snapshot(ds_freq_table(mt, cyl))
})


test_that("output from group_summary matches the expected result", {
  expect_snapshot(ds_group_summary(mt, cyl, mpg))
})


test_that("output from print_ftable2 matches the expected result", {
  mt <- mtcars
  mt[, c(2, 8:11)] <- lapply(mt[, c(2, 8:11)], factor)
  expect_snapshot(ds_auto_freq_table(mt))
})

test_that("output from ds_auto_freq_table() is as expected when data has missing values", {
  fdata <- dplyr::select(mtcarz, cyl, gear)
  fdata$cyl[c(3, 10, 14, 19)] <- NA
  fdata$gear[c(3, 8, 17, 24)] <- NA
  expect_snapshot(ds_auto_freq_table(fdata))
})


test_that("output from print_cross2 matches the expected result", {
  mt <- mtcars
  mt[, c(2, 8, 9)] <- lapply(mt[, c(2, 8, 9)], factor)
  expect_snapshot(ds_auto_cross_table(mt))
})


