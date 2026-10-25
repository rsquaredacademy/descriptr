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

test_that("ds_summary reports original extreme observation indices (#133)", {

  x <- c(NA, NA, 100, 2, 85, 4, 90, 6, 1, 95)
  k <- ds_summary(x)

  expect_equal(k$lowobs[1], 1)
  expect_equal(k$lowobsi[1], 9)
  expect_equal(k$highobs[1], 100)
  expect_equal(k$highobsi[1], 3)
  expect_equal(length(k$lowobs), 5)
  expect_equal(length(k$lowobsi), 5)

  small <- ds_summary(c(3, 1, 2))
  expect_equal(small$lowobs, c(1, 2, 3))
  expect_equal(small$lowobsi, c(2, 3, 1))
  expect_equal(small$highobs, c(3, 2, 1))
  expect_equal(small$highobsi, c(1, 3, 2))
})

test_that("ds_summary_stats validates every selected column before printing", {

  out <- capture.output(
    err <- tryCatch(ds_summary_stats(mtcarz, mpg, cyl), error = identity)
  )

  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "cyl is not a continuous variable", fixed = TRUE)
  expect_length(out, 0)
})
