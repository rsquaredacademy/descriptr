context("chi-dist")

test_that("dist_chi_plot throws the appropriate warning", {
  expect_warning(dist_chi_plot(), 'vistributions')
})

test_that("dist_chi_prob throws the appropriate warning", {
  expect_warning(dist_chi_prob(13.58, 11, 'lower'), 'vistributions')
})

test_that("dist_chi_perc throws the appropriate warning", {
  expect_warning(dist_chi_perc(0.165, 8, 'upper'), 'vistributions')
})
