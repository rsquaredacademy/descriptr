context("f-dist")


test_that("dist_f_plot returns appropriate error messages", {
  expect_error(dist_f_plot("4"), "Numerator DF must be numeric/integer")

  expect_error(dist_f_plot(den_df = "30"), "Denominator DF must be numeric/integer")

  expect_error(dist_f_plot(as.factor(4)), "Numerator DF must be numeric/integer")

  expect_error(
    dist_f_plot(den_df = as.factor(30)),
    "Denominator DF must be numeric/integer"
  )

  expect_error(dist_f_plot(normal = 3), "input for normal must be logical")

  expect_error(dist_f_plot(normal = "3"), "input for normal must be logical")
})


test_that("output from dist_f_perc matches the expected result", {
  k <- dist_f_perc(0.95, 3, 30, "lower")
  expect_equal(k$x, 2.922)

  k <- dist_f_perc(0.125, 9, 35, "upper")
  expect_equal(k$x, 1.705)
})


test_that("dist_f_perc returns appropriate error messages", {
  expect_error(dist_f_perc("0.95"), "probs must be numeric")

  expect_error(dist_f_perc(as.factor(1)), "probs must be numeric")

  expect_error(dist_f_perc(0.95, "3"), "Numerator DF must be numeric/integer")

  expect_error(
    dist_f_perc(0.95, as.factor(3)),
    "Numerator DF must be numeric/integer"
  )

  expect_error(dist_f_perc(den_df = "30"), "Denominator DF must be numeric/integer")

  expect_error(
    dist_f_perc(den_df = as.factor(30)),
    "Denominator DF must be numeric/integer"
  )

  expect_error(dist_f_perc(-0.95), "probs must be between 0 and 1")

  expect_error(dist_f_perc(1.95), "probs must be between 0 and 1")
})


test_that("output from dist_f_prob matches expected result", {

  # lower tail
  k <- dist_f_prob(2.35, 5, 32)
  expect_equal(k$probs, 0.937)

  # upper tail
  k <- dist_f_prob(1.5222, 9, 35, type = "upper")
  expect_equal(k$probs, 0.179)
})
