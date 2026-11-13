fdata <- dplyr::select(mtcarz, cyl, gear, am, vs)
ndata <- dplyr::select(mtcarz, mpg, disp, hp, wt, qsec, drat)

test_that("multistats throws appropriate error", {
  expect_error(ds_tidy_stats(fdata), "Data has no continuous variables.")
})

test_that("output from multistats is as expected", {

  actual <- 
    ds_tidy_stats(mtcars, mpg, disp, hp) %>%
    dplyr::filter(vars == "disp") %>%
    dplyr::select(min) %>%
    as.data.frame()
  expected <- data.frame(min = 71.1)
  
  expect_equal(actual, expected)

  k <- ds_tidy_stats(ndata)
  actual <- round(k$min[1], 2)
  expected <- 71.1

  expect_equal(actual, expected)

})

test_that("ds_tidy_stats works with a single numeric column", {

  x <- c(7, 2, 9, 4, 5)
  k <- ds_tidy_stats(data.frame(x = x))

  expect_equal(k$vars, "x")
  expect_equal(k$min, min(x))
  expect_equal(k$max, max(x))
  expect_equal(k$mean, mean(x))
  expect_equal(k$t_mean, mean(x, trim = 0.05))
  expect_equal(k$median, median(x))
  expect_equal(k$range, max(x) - min(x))
  expect_equal(k$variance, var(x))
  expect_equal(k$stdev, sd(x))
  expect_equal(unname(k$q1), unname(quantile(x, 0.25)))
  expect_equal(unname(k$q3), unname(quantile(x, 0.75)))
  expect_equal(unname(k$iqrange), unname(IQR(x)))

})
