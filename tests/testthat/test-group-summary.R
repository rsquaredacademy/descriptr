test_that("output from ds_group_summary matches the expected result", {
  mt <- mtcars
  mt$cyl <- as.factor(mt$cyl)
  k <- ds_group_summary(mt, cyl, mpg)
  metrics <- c(
    "Obs", "Missing", "Minimum", "Maximum", "Mean", "Median", "Mode",
    "Std. Deviation", "Variance", "Skewness", "Kurtosis",
    "Uncorrected SS", "Corrected SS", "Coeff Variation",
    "Std. Error Mean", "Range", "Interquartile Range"
  )

  expect_equal(as.character(k$stats[, 1]), metrics)
  expect_equal(k$stats[, 2], c(
    11.00, 0.00, 21.40, 33.90, 26.66, 26.00, 22.80,
    4.51, 20.34, 0.35, -1.43, 8023.83, 203.39,
    16.91, 1.36, 12.50, 7.60
  ))
  expect_equal(k$stats[, 3], c(
    7.00, 0.00, 17.80, 21.40, 19.74, 19.70, 21.00,
    1.45, 2.11, -0.26, -1.83, 2741.14, 12.68,
    7.36, 0.55, 3.60, 2.35
  ))
  expect_equal(k$stats[, 4], c(
    14.00, 0.00, 10.40, 19.20, 15.10, 15.20, 10.40,
    2.56, 6.55, -0.46, 0.33, 3277.34, 85.20,
    16.95, 0.68, 8.80, 1.85
  ))
})

test_that("ds_group_summary handles missing values in the continuous variable", {
  g <- transform(mtcarz, mpg = c(NA, mpg[-1]))
  k <- ds_group_summary(g, cyl, mpg)

  for (lv in levels(g$cyl)) {
    x   <- g$mpg[g$cyl == lv]
    col <- k$stats[[lv]]
    names(col) <- k$stats[[1]]

    expect_equal(col[["Obs"]], sum(!is.na(x)))
    expect_equal(col[["Missing"]], sum(is.na(x)))
    expect_equal(col[["Mean"]], round(mean(x, na.rm = TRUE), 2))
    expect_equal(col[["Median"]], round(median(x, na.rm = TRUE), 2))
    expect_equal(col[["Std. Deviation"]], round(sd(x, na.rm = TRUE), 2))
    expect_equal(col[["Interquartile Range"]], round(IQR(x, na.rm = TRUE), 2))
    expect_equal(col[["Range"]], round(diff(range(x, na.rm = TRUE)), 2))
  }

  expect_equal(
    k$tidy_stats$missing,
    as.numeric(tapply(g$mpg, g$cyl, function(x) sum(is.na(x))))
  )
})

test_that("ds_group_summary reports groups with only missing values", {
  g <- mtcarz
  g$mpg[g$cyl == "4"] <- NA
  k <- ds_group_summary(g, cyl, mpg)

  col4 <- k$stats[["4"]]
  names(col4) <- k$stats[[1]]
  expect_equal(col4[["Obs"]], 0)
  expect_equal(col4[["Missing"]], 11)
  expect_true(all(is.na(col4[-(1:2)])))

  tidy4 <- dplyr::filter(k$tidy_stats, cyl == "4")
  expect_equal(tidy4$length, 0)
  expect_equal(tidy4$missing, 11)
  expect_true(all(is.na(unlist(tidy4[c(
    "min", "max", "mean", "median", "mode", "sd", "variance",
    "skewness", "kurtosis", "coeff_var", "std_error", "range", "iqr"
  )]))))

  # the other groups are unaffected
  expect_equal(
    k$tidy_stats$mean[k$tidy_stats$cyl == "6"],
    mean(g$mpg[g$cyl == "6"], na.rm = TRUE)
  )
})

test_that("ds_group_summary drops unused levels of the grouping factor", {
  g <- mtcarz
  g$cyl <- factor(as.character(g$cyl), levels = c("4", "6", "8", "12"))

  k <- ds_group_summary(g, cyl, mpg)

  expect_equal(names(k$stats), c("Statistic/Levels", "4", "6", "8"))
  expect_equal(
    as.numeric(k$stats[1, -1]),
    as.numeric(table(droplevels(g$cyl)))
  )
  expect_equal(
    k$stats[["6"]][k$stats[[1]] == "Mean"],
    round(mean(g$mpg[g$cyl == "6"]), 2)
  )
})


test_that("ds_group_summary throws the appropriate error", {
  mt <- mtcars
  mt$cyl <- as.factor(mt$cyl)
  mt$am <- as.factor(mt$am)

  expect_error(
    ds_group_summary(mtcars, gear, mpg),
    "gear is not a categorical variable. The function expects an object of type `factor` but gear is of type `numeric`."
  )
  expect_error(
    ds_group_summary(mt, cyl, am),
    "am is not a continuous variable. The function expects an object of type `numeric` or `integer` but am is of type `factor`."
  )
})


test_that("output from ds_group_summary plot is as expected", {
  skip_on_cran()

  k <- ds_group_summary(mtcarz, cyl, mpg)
  p <- plot(k)
  vdiffr::expect_doppelganger("group_summary", p$plot)
})


