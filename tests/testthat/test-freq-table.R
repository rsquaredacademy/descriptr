context("freq_table")

test_that("output from ds_freq_table matches expected results", {
  k <- ds_freq_table(mtcarz, cyl)
  expect_equivalent(k$ftable[[1]], c("4", "6", "8"))
  expect_equivalent(k$ftable[[2]], c(11, 7, 14))
  expect_equivalent(k$ftable[[3]], c(11, 18, 32))
  expect_equivalent(k$ftable[[4]], c(34.38, 21.88, 43.75))
  expect_equivalent(k$ftable[[5]], c(34.38, 56.25, 100.00))
  expect_equivalent(k$varname, "cyl")
})

test_that("ds_freq_table returns appropriate errors", {
  expect_error(ds_freq_table(mtcars, mpg), "variable must be categorical/qualitative")
})

test_that("output from ds_freq_table plot is as expected", {
  skip_on_cran()

  k <- ds_freq_table(mtcarz, cyl)
  p <- plot(k)
  vdiffr::expect_doppelganger("ds_freq_bar", p$plot)
})
