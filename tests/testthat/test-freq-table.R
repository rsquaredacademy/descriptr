context("freq_table")

test_that("output from ds_freq_table matches expected results", {
  k <- ds_freq_table(mtcarz, cyl)
  expect_equivalent(k$ftable[[1]], c("4", "6", "8"))
  expect_equivalent(k$ftable[[2]], c(11, 7, 14))
  expect_equivalent(k$ftable[[3]], c(11, 18, 32))
  expect_equivalent(k$ftable[[4]][-2], c(34.38, 43.75))
  expect_equivalent(k$ftable[[5]], c(34.38, 56.25, 100.00))
  expect_equivalent(k$utility$varname, "cyl")
})


test_that("output from ds_freq_table plot is as expected", {
  skip_on_cran()

  k <- ds_freq_table(mtcarz, cyl)
  p <- plot(k)
  expect_doppelganger("ds_freq_bar", p$plot)
})

test_that("output from ds_freq_cont matches expected result", {
  k <- ds_freq_table(mtcars, mpg, bins = 4)
  expect_equivalent(k$utility$breaks, c(10.400, 16.275, 22.150, 28.025, 33.900))
  expect_equivalent(k$utility$frequency, c(10, 13, 5, 4))
  expect_equivalent(k$utility$cumulative, c(10, 23, 28, 32))
  expect_equivalent(k$utility$percent[-c(2, 3)], c(31.25, 12.50))
  expect_equivalent(k$utility$cum_percent[-2], c(31.25, 87.50, 100.00))
  expect_equal(k$utility$bins, 4)
  expect_equal(k$utility$data, mtcars$mpg)
  expect_equivalent(k$utility$varname, "mpg")
})

test_that("output from ds_freq_table plot is as expected", {
  skip_on_cran()

  k <- ds_freq_table(mtcarz, mpg)
  p <- plot(k)
  expect_doppelganger("ds_freq_cont", p$plot)
})

test_that("output from ds_freq_table is as expected in the presence of missing data", {

  mt <- mtcarz
  mt$cyl[c(3, 8, 15, 20)] <- NA 
  k <- ds_freq_table(mt, cyl)
  actual <- k$utility$na_count
  expected <- 4

  expect_equal(actual, expected)

  mt$mpg[c(3, 8, 15, 20)] <- NA
  k <- ds_freq_table(mt, mpg)
  actual <- k$utility$na_count
  expected <- 4  

  expect_equal(actual, expected)

})

test_that("ds_freq_table throws appropriate error", {

  mt <- mtcarz
  mt$gear <- as.character(mt$gear)

  expect_error(ds_freq_table(data = mtcarz, col = mpg, bins = "5"), "bins must be integer value")
  expect_error(ds_freq_table(data = mt, col = gear), "gear is neither continuous nor categorical.")
})