context("t-dist")

test_that("dist_t_perc returns appropriate error messages", {
  expect_error(dist_t_perc("0.95", 4, "lower"), "probs must be numeric")

  expect_error(dist_t_perc(as.factor(1), 4, "lower"), "probs must be numeric")

  expect_error(dist_t_perc(0.95, "4", "lower"), "df must be numeric/integer")

  expect_error(
    dist_t_perc(0.95, as.factor(4), "lower"),
    "df must be numeric/integer"
  )

  expect_error(dist_t_perc(-0.95, 4, "lower"), "probs must be between 0 and 1")

  expect_error(dist_t_perc(1.95, 4, "lower"), "probs must be between 0 and 1")
})

test_that("dist_t_prob returns appropriate error messages", {
  expect_error(dist_t_prob("1.23", 4, "lower"), "perc must be numeric/integer")

  expect_error(
    dist_t_prob(as.factor(1), 4, "lower"),
    "perc must be numeric/integer"
  )

  expect_error(dist_t_prob(1.23, "4", "lower"), "df must be numeric/integer")

  expect_error(
    dist_t_prob(1.23, as.factor(4), "lower"),
    "df must be numeric/integer"
  )
})
