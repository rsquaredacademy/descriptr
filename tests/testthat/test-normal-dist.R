context("normal-dist")

test_that("output from dist_norm_perc matches expected output", {
  k <- dist_norm_perc()
  expect_equal(k$x, 1.645)

  k <- dist_norm_perc(0.05, type = "upper")
  expect_equal(k$x, 1.645)

  k <- dist_norm_perc(0.95, type = "both")
  expect_equal(k$x, c(-1.96, 1.96))

  k <- dist_norm_perc(0.95, mean = 2, sd = 1.36)
  expect_equal(k$x, 4.237)

  k <- dist_norm_perc(0.3, mean = 2, sd = 1.36, type = "upper")
  expect_equal(k$x, 2.713)

  k <- dist_norm_perc(0.95, mean = 2, sd = 1.36, type = "both")
  expect_equal(k$x, c(-0.666, 4.666))
})


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


test_that("output from dist_norm_prob matches expected output", {
  k <- dist_norm_prob(1.64)
  expect_equal(k$prob, 0.949)

  k <- dist_norm_prob(1.64, type = "upper")
  expect_equal(k$prob, 0.051)

  k <- dist_norm_prob(c(-1.96, 1.96), type = "both")
  expect_equal(k$prob, c(0.025, 0.025))

  k <- dist_norm_prob(3.78, mean = 2, sd = 1.36)
  expect_equal(k$prob, 0.905)

  k <- dist_norm_prob(3.43, mean = 2, sd = 1.36, type = "upper")
  expect_equal(k$prob, 0.147)

  k <- dist_norm_prob(c(-1.74, 1.83), type = "both")
  expect_equal(k$prob, c(0.041, 0.034))
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
