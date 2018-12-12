context("binom_dist")

test_that("dist_binom_plot throws the appropriate errors", {
  expect_error(dist_binom_plot(10, -0.5), "p must be between 0 and 1")
  expect_error(dist_binom_plot(10, 1.5), "p must be between 0 and 1")
  expect_error(dist_binom_plot("10", 0.5), "n must be numeric/integer")
  expect_error(dist_binom_plot(as.factor(10), 0.5), "n must be numeric/integer")
  expect_error(dist_binom_plot(10, "0.5"), "p must be numeric")
  expect_error(dist_binom_plot(10, as.factor(0.5)), "p must be numeric")
})

test_that("dist_binom_prob throws the appropriate errors", {
  expect_error(dist_binom_prob(10, -0.5, 4), "p must be between 0 and 1")
  expect_error(dist_binom_prob(10, 1.5, 4), "p must be between 0 and 1")
  expect_error(dist_binom_prob(10, "0.5", 4), "p must be numeric")
  expect_error(dist_binom_prob("10", 0.5, 4), "n must be numeric/integer")
  expect_error(dist_binom_prob(as.factor(10), 0.5, 4), "n must be numeric/integer")
  expect_error(dist_binom_prob(10, 0.5, "4"), "s must be numeric/integer")
  expect_error(dist_binom_prob(10, 0.5, as.factor(4)), "s must be numeric/integer")
})

test_that("dist_dist_binom_perc throws the appropriate errors", {
  expect_error(dist_binom_perc(10, -0.5, 0.05), "p must be between 0 and 1")
  expect_error(dist_binom_perc(10, 1.5, 0.05), "p must be between 0 and 1")
  expect_error(dist_binom_perc(10, 0.5, -0.05), "tp must be between 0 and 0.5")
  expect_error(dist_binom_perc(10, 0.5, 0.51), "tp must be between 0 and 0.5")
  expect_error(dist_binom_perc("10", 0.5, 0.05), "n must be numeric/integer")
  expect_error(dist_binom_perc(as.factor(10), 0.5, 0.05), "n must be numeric/integer")
  expect_error(dist_binom_perc(10, "0.5", 0.05), "p must be numeric")
  expect_error(dist_binom_perc(10, as.factor(0.5), 0.05), "p must be numeric")
  expect_error(dist_binom_perc(10, 0.5, "0.05"), "tp must be numeric")
  expect_error(dist_binom_perc(10, 0.5, as.factor(0.05)), "tp must be numeric")
})
