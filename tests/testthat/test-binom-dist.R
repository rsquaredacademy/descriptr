context("binom_dist")

test_that("dist_binom_plot throws the appropriate warning", {
  expect_warning(dist_binom_plot(10, 0.3), 'vistributions')
})

test_that("dist_binom_prob throws the appropriate warning", {
  expect_warning(dist_binom_prob(10, 0.3, 4, type = 'exact'), 'vistributions')
})

test_that("dist_binom_perc throws the appropriate warning", {
  expect_warning(dist_binom_perc(10, 0.5, 0.05), 'vistributions')
})
