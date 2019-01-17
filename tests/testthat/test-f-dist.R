context("f-dist")


test_that("dist_f_plot throws the appropriate warning", {
  expect_warning(dist_f_plot(), 'vistributions')
})

test_that("dist_f_prob throws the appropriate warning", {
  expect_warning(dist_f_prob(2.35, 5, 32), 'vistributions')
})

test_that("dist_f_perc throws the appropriate warning", {
  expect_warning(dist_f_perc(0.95, 3, 30, 'lower'), 'vistributions')
})
