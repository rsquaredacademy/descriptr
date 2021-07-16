context("multistats")

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
