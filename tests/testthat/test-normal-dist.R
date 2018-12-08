context("normal-dist")

test_that("dist_norm_perc throws the appropriate errors", {
  expect_error(dist_norm_perc(0.95, "2", 1), "mean must be numeric/integer")
  expect_error(dist_norm_perc(0.95, as.factor(2), 1), "mean must be numeric/integer")
  expect_error(dist_norm_perc(0.95, 2, "1"), "sd must be numeric/integer")
  expect_error(dist_norm_perc(0.95, 2, as.factor(1)), "sd must be numeric/integer")
  expect_error(dist_norm_perc(0.95, 2, -1), "sd must be positive")
  expect_error(dist_norm_perc("0.95", 2, 1), "probs must be numeric")
  expect_error(dist_norm_perc(as.factor(0.95), 2, 1), "probs must be numeric")
  expect_error(dist_norm_perc(1.95, 2, 1), "probs must be between 0 and 1")
  expect_error(dist_norm_perc(-1.95, 2, 1), "probs must be between 0 and 1")
})

test_that("dist_norm_prob throws the appropriate errors", {
  expect_error(dist_norm_prob(1.64, "0", 1), "mean must be numeric/integer")
  expect_error(dist_norm_prob(1.64, as.factor(0), 1), "mean must be numeric/integer")
  expect_error(dist_norm_prob(1.64, 0, "1"), "sd must be numeric/integer")
  expect_error(dist_norm_prob(1.64, 0, as.factor(1)), "sd must be numeric/integer")
  expect_error(dist_norm_prob(1.64, 0, -1), "sd must be positive")
  expect_error(dist_norm_prob("1.64", 0, 1), "perc must be numeric/integer")
  expect_error(dist_norm_prob(as.factor(1.64), 0, 1), "perc must be numeric/integer")
  expect_error(
    dist_norm_prob(c(1.64, 1.63, 1.62), 0, 1),
    "Please do not specify more than 2 percentile values"
  )
  expect_error(
    dist_norm_prob(1.64, 0, 1, type = "both"),
    "Specify two percentile values"
  )
})
