test_that("output from ds_freq_table matches expected results", {
  k <- ds_freq_table(mtcarz, cyl)
  expect_equal(k$ftable[[1]], c("4", "6", "8"))
  expect_equal(k$ftable[[2]], c(11, 7, 14))
  expect_equal(k$ftable[[3]], c(11, 18, 32))
  expect_equal(k$ftable[[4]][-2], c(34.38, 43.75))
  expect_equal(k$ftable[[5]], c(34.38, 56.25, 100.00))
  expect_equal(k$utility$varname, "cyl")
})


test_that("output from ds_freq_table plot is as expected", {
  skip_on_cran()

  k <- ds_freq_table(mtcarz, cyl)
  p <- plot(k)
  vdiffr::expect_doppelganger("ds_freq_bar", p$plot)
})

test_that("output from ds_freq_cont matches expected result", {
  k <- ds_freq_table(mtcars, mpg, bins = 4)
  expect_equal(k$utility$breaks, c(10.400, 16.275, 22.150, 28.025, 33.900))
  expect_equal(k$utility$frequency, c(10, 13, 5, 4))
  expect_equal(k$utility$cumulative, c(10, 23, 28, 32))
  expect_equal(k$utility$percent[-c(2, 3)], c(31.25, 12.50))
  expect_equal(k$utility$cum_percent[-2], c(31.25, 87.50, 100.00))
  expect_equal(k$utility$bins, 4)
  expect_equal(k$utility$data, mtcars$mpg)
  expect_equal(k$utility$varname, "mpg")
})

test_that("output from ds_freq_table plot is as expected", {
  skip_on_cran()

  k <- ds_freq_table(mtcarz, mpg)
  p <- plot(k)
  vdiffr::expect_doppelganger("ds_freq_cont", p$plot)
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

test_that("continuous bins are half-open and never double-count (#131)", {

  x <- seq(0, 10, 1)
  k <- ds_freq_table(data.frame(x = x), x, bins = 5)

  expect_equal(k$utility$frequency, c(2L, 2L, 2L, 2L, 3L))
  expect_equal(sum(k$utility$frequency), length(x))
  expect_equal(k$utility$cumulative, c(2L, 4L, 6L, 8L, 11L))
  expect_equal(k$utility$cum_percent[length(k$utility$cum_percent)], 100)
  expect_equal(
    k$utility$frequency,
    as.vector(table(cut(x, k$utility$breaks, include.lowest = TRUE, right = FALSE)))
  )
})

test_that("boundary values belong to a single interval (#131)", {

  x <- c(1, 2, 3, 4, 5)
  k <- ds_freq_table(data.frame(x = x), x, bins = 4)

  # cuts at 1, 2, 3, 4, 5: 2 belongs to [2, 3), not to [1, 2]
  expect_equal(k$utility$frequency, c(1L, 1L, 1L, 2L))
  expect_equal(k$ftable$frequency[k$ftable$lower == 2], 1L)
  expect_equal(sum(k$utility$frequency), length(x))
})

test_that("closed argument controls interval boundaries (#131)", {

  x <- seq(0, 10, 1)

  right <- ds_freq_table(data.frame(x = x), x, bins = 5, closed = "right")
  expect_equal(right$utility$frequency, c(3L, 2L, 2L, 2L, 2L))
  expect_equal(sum(right$utility$frequency), length(x))

  both <- ds_freq_table(data.frame(x = x), x, bins = 5, closed = "both")
  expect_equal(both$utility$frequency, c(3L, 3L, 3L, 3L, 3L))
  expect_equal(sum(both$utility$frequency), 15L)
})

test_that("zero variance data cannot be binned (#131)", {

  expect_error(
    ds_freq_table(data.frame(x = rep(5, 10)), x, bins = 5),
    "Data has zero variance; cannot construct continuous bins."
  )
})
