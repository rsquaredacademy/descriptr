context("ds_freq_cont")

test_that("output from ds_freq_cont matches expected result", {
  k <- ds_freq_cont(mtcars, mpg, bins = 4)
  expect_equivalent(k$breaks, c(10.400, 16.275, 22.150, 28.025, 33.900))
  expect_equivalent(k$frequency, c(10, 13, 5, 4))
  expect_equivalent(k$cumulative, c(10, 23, 28, 32))
  expect_equivalent(k$percent, c(31.25, 40.62, 15.62, 12.50))
  expect_equivalent(k$cum_percent, c(31.25, 71.88, 87.50, 100.00))
  expect_equal(k$bins, 4)
  expect_equal(k$data, mtcars$mpg)
  expect_equivalent(k$varname, "mpg")
})

test_that("ds_freq_cont returns appropriate errors", {
  mt <- mtcars
  mt$cyl <- as.factor(mt$cyl)

  expect_error(ds_freq_cont(mt, cyl), "cyl is not a continuous variable. The function expects an object of type `numeric` or `integer` but cyl is of type `factor`.")
  expect_error(ds_freq_cont(mtcars, mpg, "5"), "bins must be integer value")
})

test_that("output from ds_freq_cont plot is as expected", {
  skip_on_cran()

  k <- ds_freq_cont(mtcarz, mpg)
  p <- plot(k)
  vdiffr::expect_doppelganger("ds_freq_cont", p$plot)
})
