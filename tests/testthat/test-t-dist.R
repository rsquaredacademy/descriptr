context("t-dist")

test_that("dist_t_plot throws the appropriate warning", {
  expect_warning(dist_t_plot(), 'vistributions')
})

test_that("dist_t_prob throws the appropriate warning", {
  expect_warning(dist_t_prob(2.045, 7, 'lower'), 'vistributions')
})

test_that("dist_t_perc throws the appropriate warning", {
  expect_warning(dist_t_perc(probs = 0.95, df = 4, type = 'lower'), 'vistributions')
})
