# grouped and rowwise tibbles give the same results as the ungrouped data

gd_data <- function() {
  d <- mtcarz
  d$mpg[c(2, 5)] <- NA
  d$cyl[7] <- NA
  d
}

same_output <- function(f) {
  d <- gd_data()
  inputs <- list(
    dplyr::group_by(d, am),
    dplyr::group_by(d, cyl, .drop = FALSE),
    dplyr::rowwise(d)
  )
  for (g in inputs) {
    expect_equal(f(g), f(d))
  }
}

test_that("summary functions ignore grouping", {
  same_output(function(x) ds_group_summary(x, cyl, mpg)$tidy_stats)
  same_output(function(x) ds_group_summary_interact(x, mpg, cyl, gear))
  same_output(function(x) ds_measures_location(x, mpg, disp))
  same_output(function(x) ds_measures_variation(x, mpg, disp))
  same_output(function(x) ds_measures_symmetry(x, mpg))
  same_output(function(x) ds_percentiles(x, mpg))
  same_output(function(x) ds_extreme_obs(x, mpg))
  same_output(function(x) ds_tidy_stats(x, mpg, disp))
  same_output(function(x) ds_gmean(x, disp))
  same_output(function(x) ds_mode(x, mpg))
})

test_that("table functions ignore grouping", {
  same_output(function(x) ds_freq_table(x, cyl)$ftable)
  same_output(function(x) ds_freq_table(x, mpg, 4)$ftable)
  same_output(function(x) ds_cross_table(x, cyl, gear)$ftable)
  same_output(function(x) ds_cross_table(x, cyl, gear)$utility[c("twowaytable", "obs", "missing")])
  same_output(function(x) capture.output(ds_summary_stats(x, mpg)))
  same_output(function(x) capture.output(ds_screener(x)))
})

test_that("grouped cross table keeps counts and missing", {
  g <- dplyr::group_by(gd_data(), am)
  k <- ds_cross_table(g, cyl, gear)
  expect_false(is.null(k$utility$twowaytable))
  expect_equal(k$utility$obs, sum(!is.na(gd_data()$cyl) & !is.na(gd_data()$gear)))
  expect_equal(k$utility$missing, 1)
})

test_that("interaction summary of grouped input has the ungrouped means", {
  d <- data.frame(g = factor(c("a", "a", "b")), x = c(1, 3, 5))
  k <- ds_group_summary_interact(dplyr::group_by(d, g), x, g)
  expect_equal(k$mean, c(2, 5))
})
