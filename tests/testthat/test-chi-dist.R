context("chi-dist")

test_that("dist_chi_plot returns appropriate error messages", {
  expect_error(dist_chi_plot("3"), "df must be numeric/integer")

  expect_error(dist_chi_plot(as.factor(3)), "df must be numeric/integer")

  expect_error(dist_chi_plot(normal = 3), "normal must be logical")

  expect_error(dist_chi_plot(normal = "3"), "normal must be logical")
})


test_that("dist_chi_perc returns appropriate error messages", {
  expect_error(dist_chi_perc("0.95"), "probs must be numeric")

  expect_error(dist_chi_perc(as.factor(1)), "probs must be numeric")

  expect_error(dist_chi_perc(df = "3"), "df must be numeric/integer")

  expect_error(dist_chi_perc(df = as.factor(3)), "df must be numeric/integer")
})

test_that("dist_chi_prob returns appropriate error messages", {
  expect_error(dist_chi_prob("18.36", 11), "perc must be numeric/integer")

  expect_error(dist_chi_prob(as.factor(18), 11), "perc must be numeric/integer")

  expect_error(dist_chi_prob(18.36, "11"), "df must be numeric/integer")

  expect_error(dist_chi_prob(18.36, as.factor(11)), "df must be numeric/integer")
})
