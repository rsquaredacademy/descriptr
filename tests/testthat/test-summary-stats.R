ndata <- dplyr::select(mtcarz, mpg)

test_that("output from ds_summary is as expected when data is a data.frame", {
  
  actual <- round(ds_summary(mtcarz, mpg)$variance, 2)
  expected <- 36.32
  expect_equal(actual, expected)

})

test_that("output from ds_summary is as expected when data is numeric", {
  
  actual <- round(ds_summary(mtcarz$mpg)$variance, 2)
  expected <- 36.32
  expect_equal(actual, expected)

})

test_that("ds_summary_stats throws appropriate errors", {
  fdata <- dplyr::select(mtcarz, cyl, gear, am, vs)
  expect_error(ds_summary_stats(fdata), 'Data has no continuous variables.')
  expect_error(ds_summary_stats(mtcarz, cyl, gear), 'Data has no continuous variables.')
})

test_that("ds_summary works with fewer than five non-missing values", {

  x <- c(4, 1, 3, 2)
  k <- ds_summary(x)

  expect_equal(k$obs, length(x))
  expect_equal(k$missing, 0)
  expect_equal(k$avg, mean(x))
  expect_equal(k$median, median(x))
  expect_equal(k$stdev, sd(x))
  expect_equal(k$variance, var(x))
  expect_equal(k$range, max(x) - min(x))
  expect_equal(k$lowobs, sort(x))
  expect_equal(k$highobs, sort(x, decreasing = TRUE))
  expect_equal(unname(k$per25), unname(quantile(x, 0.25)))

  expect_output(ds_summary_stats(x), "Univariate Analysis")
  expect_output(ds_summary_stats(data.frame(x = x[1:3])), "Variable: x")

  k_na <- ds_summary(c(1, 2, NA, 3))
  expect_equal(k_na$obs, 4)
  expect_equal(k_na$missing, 1)
  expect_equal(k_na$lowobs, c(1, 2, 3))
})
