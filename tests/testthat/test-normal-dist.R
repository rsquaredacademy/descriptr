context("normal-dist")

test_that("dist_norm_plot throws the appropriate warning", {
  expect_warning(dist_norm_plot(), 'vistributions')
})

test_that("dist_norm_prob throws the appropriate warning", {
  expect_warning(dist_norm_prob(3.78, mean = 2, sd = 1.36), 'vistributions')
})

test_that("dist_norm_perc throws the appropriate warning", {
  expect_warning(dist_norm_perc(0.95, mean = 2, sd = 1.36), 'vistributions')
})
