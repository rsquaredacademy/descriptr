fdata <- dplyr::select(mtcarz, cyl, gear, am, vs)

test_that("output from ds_tailobs match expected result", {
  expect_equal(ds_tailobs(mtcars$mpg, 5, "low"), c(10.4, 10.4, 13.3, 14.3, 14.7), ignore_attr = TRUE)
  expect_equal(ds_tailobs(mtcars$mpg, 5, "high"), c(33.9, 32.4, 30.4, 30.4, 27.3), ignore_attr = TRUE)
})


test_that("ds_tailobs returns the appropriate error", {
  expect_error(ds_tailobs(mtcars$mpg, 40, "low"), "n must be less than or equal to the number of non-missing observations")
  expect_error(ds_tailobs(mtcars$mpg, 40, "high"), "n must be less than or equal to the number of non-missing observations")
  expect_error(ds_tailobs("mtcars", 40, "high"), "data must be numeric")
  expect_error(ds_tailobs(as.factor(mtcars$disp), 40, "high"), "data must be numeric")
  expect_error(ds_tailobs(mtcars$mpg, "40", "high"), "n must be numeric")
  expect_error(ds_measures_location(mtcarz$carb), "data must be either numeric or a `data.frame`.")
  expect_error(ds_measures_variation(mtcarz$carb), "data must be either numeric or a `data.frame`.")
  expect_error(ds_measures_symmetry(mtcarz$carb), "data must be either numeric or a `data.frame`.")
  expect_error(ds_percentiles(mtcarz$carb), "data must be either numeric or a `data.frame`.")
  expect_error(ds_extreme_obs(mtcarz$carb), "data must be either a numeric vector or a `data.frame`.")
  expect_error(ds_mode(mtcarz$carb), "Mode can be calculated only for numeric data. The variable you have selected is of type factor.")
})


test_that("output from ds_rindex match expected result", {
  expect_equal(ds_rindex(mtcars$mpg, c(13.3)), 24, ignore_attr = TRUE)
  expect_equal(ds_rindex(mtcars$mpg, c(33.9)), 20, ignore_attr = TRUE)
  expect_equal(ds_rindex(mtcars$mpg, c(13.3, 10.4)), c(24, 15, 16), ignore_attr = TRUE)
  expect_null(ds_rindex(mtcars$mpg, c(34)))
})


test_that("ds_rindex returns the appropriate error", {
  expect_error(ds_rindex("mtcars", c(13.3)), "Data must be numeric.")
  expect_error(ds_rindex(as.factor(mtcars$disp), c(13.3)), "Data must be numeric.")
  expect_error(ds_rindex(mtcars$mpg, "13.3"), "Values must be numeric.")
})


test_that("output from ds_skewness matches expected result", {
  expect_equal(round(ds_skewness(mtcars$mpg), 3), 0.672)
  expect_equal(round(ds_skewness(mtcars, mpg), 3), 0.672)
  expect_equal(round(ds_skewness(mtcars$disp), 3), 0.420)
  expect_equal(round(ds_skewness(mtcars$hp), 3), 0.799)
  expect_equal(round(ds_skewness(mtcars$drat), 3), 0.293)
  expect_equal(round(ds_skewness(mtcars$wt), 3), 0.466)
  expect_equal(round(ds_skewness(mtcars$qsec), 3), 0.406)
})


test_that("ds_skewness returns the appropriate error", {
  expect_error(round(ds_skewness("mtcars$mpg"), 3), "Skewness is calculated only for numeric data. The variable you have selected is of type character.")
  expect_error(round(ds_skewness(as.factor(mtcars$mpg)), 3), "Skewness is calculated only for numeric data. The variable you have selected is of type factor.")
})


test_that("output from ds_kurtosis matches expected result", {
  expect_equal(round(ds_kurtosis(mtcars$mpg), 3), -0.022)
  expect_equal(round(ds_kurtosis(mtcars, mpg), 3), -0.022)
  expect_equal(round(ds_kurtosis(mtcars$disp), 3), -1.068)
  expect_equal(round(ds_kurtosis(mtcars$hp), 3), 0.275)
  expect_equal(round(ds_kurtosis(mtcars$drat), 3), -0.450)
  expect_equal(round(ds_kurtosis(mtcars$wt), 3), 0.417)
  expect_equal(round(ds_kurtosis(mtcars$qsec), 3), 0.865)
})


test_that("ds_kurtosis returns the appropriate error", {
  expect_error(round(ds_kurtosis("mtcars$mpg"), 3), "Kurtosis is calculated only for numeric data. The variable you have selected is of type character.")
  expect_error(round(ds_kurtosis(as.factor(mtcars$mpg)), 3), "Kurtosis is calculated only for numeric data. The variable you have selected is of type factor.")
})


test_that("ds_css matches `Sum Sq` from anova", {
  expect_equal(round(ds_css(mtcars$mpg)), round(anova(lm(mpg ~ 1, data = mtcars))[[2]]))
  expect_equal(round(ds_css(mtcars, mpg)), round(anova(lm(mpg ~ 1, data = mtcars))[[2]]))
  expect_equal(round(ds_css(mtcars$disp)), round(anova(lm(disp ~ 1, data = mtcars))[[2]]))
  expect_equal(round(ds_css(mtcars$hp)), round(anova(lm(hp ~ 1, data = mtcars))[[2]]))
  expect_equal(round(ds_css(mtcars$drat)), round(anova(lm(drat ~ 1, data = mtcars))[[2]]))
  expect_equal(round(ds_css(mtcars$wt)), round(anova(lm(wt ~ 1, data = mtcars))[[2]]))
  expect_equal(round(ds_css(mtcars$qsec)), round(anova(lm(qsec ~ 1, data = mtcars))[[2]]))
})


test_that("ds_css returns the appropriate error", {
  expect_error(round(ds_css("mtcars$mpg")), "Corrected sum of squares can be calculated only for numeric data. The variable you have selected is of type character.")
  expect_error(round(ds_css(as.factor(mtcars$mpg))), "Corrected sum of squares can be calculated only for numeric data. The variable you have selected is of type factor.")
})


test_that("output from ds_cvar matches the expected result", {
  expect_equal(round(ds_cvar(mtcars$mpg), 3), 29.999)
  expect_equal(round(ds_cvar(mtcars, mpg), 3), 29.999)
  expect_equal(round(ds_cvar(mtcars$disp), 3), 53.718)
})


test_that("ds_cvar returns the appropriate error", {
  expect_error(ds_cvar("mtcars$mpg"), "Coefficient of variation is calculated only for numeric data. The variable you have selected is of type character.")
  expect_error(ds_cvar(as.factor(mtcars$mpg)), "Coefficient of variation is calculated only for numeric data. The variable you have selected is of type factor.")
})


test_that("output from ds_mode matches the expected result", {
  expect_equal(ds_mode(mtcars$mpg), 10.4)
  expect_equal(ds_mode(mtcars, mpg), 10.4)
  expect_equal(ds_mode(mtcars$disp), 275.8)
  expect_equal(ds_mode(mtcars$hp), 110)
  expect_equal(ds_mode(mtcars$drat), 3.07)
  expect_equal(ds_mode(mtcars$wt), 3.440)
  expect_equal(ds_mode(mtcars$qsec), 17.02)
})


test_that("output from ds_range matches the expected result", {
  expect_equal(ds_range(mtcars$mpg), 23.5)
  expect_equal(ds_range(mtcars, mpg), 23.5)
  expect_equal(ds_range(mtcars$disp), 400.9)
  expect_equal(ds_range(mtcars$hp), 283)
  expect_equal(ds_range(mtcars$drat), 2.17)
  expect_equal(ds_range(mtcars$wt), 3.911)
  expect_equal(ds_range(mtcars$qsec), 8.40)
})


test_that("ds_range returns the appropriate error", {
  expect_error(ds_range("mtcars$mpg"), "Range can be calculated only for numeric data. The variable you have selected is character.")
  expect_error(ds_range(as.factor(mtcars$mpg)), "Range can be calculated only for numeric data. The variable you have selected is factor.")
})


test_that("output from ds_mdev matches the expected result", {
  expect_equal(round(ds_mdev(mtcars$mpg), 3), 4.714)
  expect_equal(round(ds_mdev(mtcars, mpg), 3), 4.714)
  expect_equal(round(ds_mdev(mtcars$disp), 3), 108.786)
  expect_equal(round(ds_mdev(mtcars$hp), 3), 56.48)
  expect_equal(round(ds_mdev(mtcars$drat), 3), 0.453)
  expect_equal(round(ds_mdev(mtcars$wt), 3), 0.73)
  expect_equal(round(ds_mdev(mtcars$qsec), 3), 1.376)
})


test_that("output from ds_mdev matches the expected result", {
  expect_equal(ds_mdev(c(92, 83, 88, 94, 91, 85, 89, 90)), 2.75)
  expect_equal(ds_mdev(c(3, 6, 6, 7, 8, 11, 15, 16)), 3.75)
})

test_that("ds_mdev returns the appropriate error", {
  expect_error(ds_mdev("mtcars$mpg"), "Mean absolute deviation is calculated only for numeric data. The variable you have selected is of type character.")
  expect_error(ds_mdev(as.factor(mtcars$mpg)), "Mean absolute deviation is calculated only for numeric data. The variable you have selected is of type factor.")
})


test_that("output from ds_hmean matches the expected output", {
  expect_equal(round(ds_hmean(mtcars$mpg), 2), 18.44)
  expect_equal(round(ds_hmean(mtcars, mpg), 2), 18.44)
  expect_equal(round(ds_hmean(mtcars$disp), 2), 166.8)
  expect_equal(round(ds_hmean(mtcars$hp), 2), 118.23)
})

test_that("ds_hmean throws the appropriate error", {
  expect_error(ds_hmean("mtcars$mpg"), "Harmonic mean can be calculated only for numeric data. The variable you have selected is character.")
  expect_error(ds_hmean(as.factor(mtcars$mpg)), "Harmonic mean can be calculated only for numeric data. The variable you have selected is factor.")
})


test_that("output from ds_gmean matches the expected output", {
  expect_equal(round(ds_gmean(mtcars$mpg), 2), 19.25)
  expect_equal(round(ds_gmean(mtcars, mpg), 2), 19.25)
  expect_equal(round(ds_gmean(mtcars$disp), 2), 197.32)
  expect_equal(round(ds_gmean(mtcars$hp), 2), 131.88)
})


test_that("ds_gmean throws the appropriate error", {
  expect_error(ds_gmean("mtcars$mpg"), "Geometric mean can be calculated only for numeric data. The variable you have selected is of type character.")
  expect_error(ds_gmean(as.factor(mtcars$mpg)), "Geometric mean can be calculated only for numeric data. The variable you have selected is of type factor.")
})

test_that("output from ds_measures_location is as expected", {

  actual <- round(sum(ds_measures_location(mtcarz)[[4]]), 2)
  expected <- 422.17
  expect_equal(actual, expected)

})

test_that("output from ds_measures_location is as expected", {

  actual <- round(sum(ds_measures_location(mtcarz, mpg)[[4]]), 2)
  expected <- 20.09
  expect_equal(actual, expected)

})

test_that("output from ds_measures_symmetry is as expected", {

  actual <- round(ds_measures_symmetry(mtcarz$mpg)[[3]], 2)
  expected <- 0.67
  expect_equal(actual, expected)

})

test_that("ds_measures_location throws appropriate errors", {

  x <- "mpg"
  expect_error(ds_measures_location(x), 'data must be either numeric or a `data.frame`.')

})

test_that("ds_measures_location throws appropriate errors", {

  mt <- dplyr::select(mtcarz, cyl, gear, am)
  expect_error(ds_measures_location(mt), 'Data has no continuous variables.')

})

test_that("ds_measures_location keeps the selected column order", {

  expect_equal(ds_measures_location(mtcarz, mpg, disp)$variable, c("mpg", "disp"))
  expect_equal(ds_measures_location(mtcarz, disp, mpg)$variable, c("disp", "mpg"))

  df <- data.frame(z = 1:5, a = 6:10)
  expect_equal(ds_measures_location(df)$variable, c("z", "a"))

  d <- mtcarz$disp[1:10]
  k <- ds_measures_location(mtcarz$mpg, d)
  expect_equal(k$variable, c("mtcarz$mpg", "d"))
  expect_equal(k$variable, ds_measures_variation(mtcarz$mpg, d)$variable)
})

test_that("output from ds_measures_variation is as expected", {

  actual <- round(sum(ds_measures_variation(mtcarz)[[3]]), 2)
  expected <- 721.88
  expect_equal(actual, expected)

})

test_that("output from ds_measures_variation is as expected", {

  actual <- round(ds_measures_variation(mtcarz, mpg)[[3]], 2)
  expected <- 23.5
  expect_equal(actual, expected)

})

test_that("output from ds_measures_variation is as expected", {

  actual <- round(ds_measures_variation(mtcarz$mpg)[[3]], 2)
  expected <- 23.5
  expect_equal(actual, expected)

})

test_that("ds_measures_variation throws appropriate errors", {

  x <- "mpg"
  expect_error(ds_measures_variation(x), 'data must be either numeric or a `data.frame`.')

})

test_that("ds_measures_variation throws appropriate errors", {

  mt <- dplyr::select(mtcarz, cyl, gear, am)
  expect_error(ds_measures_variation(mt), 'Data has no continuous variables.')

})

test_that("output from ds_measures_symmetry is as expected", {

  actual <- round(sum(ds_measures_symmetry(mtcarz)[[3]]), 2)
  expected <- 3.06
  expect_equal(actual, expected)

})

test_that("output from ds_measures_symmetry is as expected", {

  actual <- round(sum(ds_measures_symmetry(mtcarz, mpg)[[3]]), 2)
  expected <- 0.67
  expect_equal(actual, expected)

})

test_that("ds_measures_symmetry throws appropriate errors", {

  mt <- dplyr::select(mtcarz, cyl, gear, am)
  expect_error(ds_measures_symmetry(mt), 'Data has no continuous variables.')

})

test_that("output from ds_percentiles is as expected", {

  actual <- round(sum(ds_percentiles(mtcarz)[[3]]), 2)
  expected <- 152.27
  expect_equal(actual, expected)

  actual <- round(sum(ds_percentiles(mtcarz$mpg)[[6]]), 2)
  expected <- 14.34
  expect_equal(actual, expected)

})

test_that("output from ds_percentiles is as expected", {

  actual <- round(sum(ds_percentiles(mtcarz, mpg)[[3]]), 2)
  expected <- 10.4
  expect_equal(actual, expected)

})

test_that("ds_percentiles throws appropriate errors", {

  mt <- dplyr::select(mtcarz, cyl, gear, am)
  expect_error(ds_percentiles(mt), 'Data has no continuous variables.')

})

test_that("output from ds_extreme_obs is as expected", {

  actual <- round(sum(ds_extreme_obs(mtcarz, mpg)[[2]]), 2)
  expected <- 217.5
  expect_equal(actual, expected)

  actual <- round(sum(ds_extreme_obs(mtcarz$mpg)[[2]]), 2)
  expected <- 217.5
  expect_equal(actual, expected)

})


test_that("ds_extreme_obs throws appropriate errors", {

  expect_error(ds_extreme_obs(mtcarz, cyl), 'cyl is not a continuous variable. The function expects an object of type `numeric` or `integer` but cyl is of type `factor`.')

})

test_that("single numeric column data frames work", {

  d <- data.frame(x = 1:10)
  x <- d$x

  v <- ds_measures_variation(d)
  expect_equal(v$var, "x")
  expect_equal(v$n, length(x))
  expect_equal(v$range, max(x) - min(x))
  expect_equal(v$iqr, IQR(x))
  expect_equal(v$variance, round(var(x), 2))
  expect_equal(v$sd, round(sd(x), 2))
  expect_equal(v$coeff_var, round(sd(x) / mean(x) * 100, 2))
  expect_equal(v$std_error, round(sd(x) / sqrt(length(x)), 2))

  v1 <- ds_measures_variation(dplyr::select(mtcarz, mpg))
  expect_equal(v1$var, "mpg")
  expect_equal(v1$n, nrow(mtcarz))
  expect_equal(v1$sd, round(sd(mtcarz$mpg), 2))

  s <- ds_measures_symmetry(d)
  expect_equal(s$var, "x")
  expect_equal(s$n, length(x))
  n <- length(x)
  expect_equal(s$skewness, round(ds_skewness(x), 2))
  expect_equal(s$kurtosis, round(ds_kurtosis(x), 2))
  expect_equal(
    s$skewness,
    round((n / ((n - 1) * (n - 2))) * sum(((x - mean(x)) / sd(x)) ^ 3), 2)
  )

  p <- ds_percentiles(d)
  expect_equal(p$var, "x")
  expect_equal(p$n, length(x))
  expect_equal(
    unname(unlist(p[c("min", "per_1", "per_5", "per_10", "q1", "median",
                       "q3", "per_90", "per_95", "per_99", "max")])),
    unname(quantile(x, c(0, 0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99, 1)))
  )
})

test_that("ds_extreme_obs works with fewer than five non-missing values", {

  x <- c(4, 1, 3, 2)
  k <- ds_extreme_obs(x)

  expect_equal(k$value[k$type == "high"], sort(x, decreasing = TRUE))
  expect_equal(k$value[k$type == "low"], sort(x))

  k_df <- ds_extreme_obs(data.frame(x = x), x)
  expect_equal(k_df$value, k$value)

  k_one <- ds_extreme_obs(5)
  expect_equal(k_one$value, c(5, 5))

  # missing values are removed before the count is taken
  k_na <- ds_extreme_obs(c(1, 2, NA, 3))
  expect_equal(k_na$value[k_na$type == "high"], c(3, 2, 1))
})

test_that("measures handle missing values per column (#132)", {

  d <- mtcarz
  d$disp[3] <- NA

  v <- ds_measures_variation(d)
  expect_equal(v$n[v$var == "mpg"], nrow(mtcarz))
  expect_equal(v$variance[v$var == "mpg"], round(var(mtcarz$mpg), 2))
  expect_equal(v$sd[v$var == "mpg"], round(sd(mtcarz$mpg), 2))
  expect_equal(v$iqr[v$var == "mpg"], round(IQR(mtcarz$mpg), 2))
  expect_equal(v$n[v$var == "disp"], nrow(mtcarz) - 1)
  expect_equal(v$variance[v$var == "disp"], round(var(d$disp, na.rm = TRUE), 2))
  expect_equal(v$sd[v$var == "disp"], round(sd(d$disp, na.rm = TRUE), 2))

  s <- ds_measures_symmetry(d)
  expect_equal(s$n[s$var == "mpg"], nrow(mtcarz))
  expect_equal(s$skewness[s$var == "mpg"], round(ds_skewness(mtcarz$mpg), 2))
  expect_equal(s$kurtosis[s$var == "mpg"], round(ds_kurtosis(mtcarz$mpg), 2))
  expect_equal(s$n[s$var == "disp"], nrow(mtcarz) - 1)
  expect_equal(s$skewness[s$var == "disp"], round(ds_skewness(d$disp), 2))

  p <- ds_percentiles(d)
  expect_equal(p$n[p$var == "mpg"], nrow(mtcarz))
  expect_equal(p$min[p$var == "mpg"], round(min(mtcarz$mpg), 2))
  expect_equal(p$median[p$var == "mpg"], round(median(mtcarz$mpg), 2))
  expect_equal(p$max[p$var == "mpg"], round(max(mtcarz$mpg), 2))
  expect_equal(p$n[p$var == "disp"], nrow(mtcarz) - 1)

  t <- ds_tidy_stats(d)
  expect_equal(t$mean[t$vars == "mpg"], mean(mtcarz$mpg))
  expect_equal(t$median[t$vars == "mpg"], median(mtcarz$mpg))
  expect_equal(t$variance[t$vars == "mpg"], var(mtcarz$mpg))
  expect_equal(t$stdev[t$vars == "mpg"], sd(mtcarz$mpg))
  expect_equal(t$iqrange[t$vars == "mpg"], IQR(mtcarz$mpg))
  expect_equal(t$mean[t$vars == "disp"], mean(d$disp, na.rm = TRUE))
})

test_that("measures isolate missing values in orthogonal columns (#132)", {

  df_na <- data.frame(
    a = c(10, 20, NA, 40, 50),
    b = c(NA, 2, 3, 4, 5),
    c = c(100, 200, 300, 400, NA)
  )

  v <- ds_measures_variation(df_na)
  expect_equal(v$n, c(4L, 4L, 4L))
  expect_equal(v$variance, c(round(var(df_na$a, na.rm = TRUE), 2),
                              round(var(df_na$b, na.rm = TRUE), 2),
                              round(var(df_na$c, na.rm = TRUE), 2)))

  s <- ds_measures_symmetry(df_na)
  expect_equal(s$n, c(4L, 4L, 4L))

  p <- ds_percentiles(df_na)
  expect_equal(p$n, c(4L, 4L, 4L))

  t <- ds_tidy_stats(df_na)
  expect_equal(t$mean, c(mean(df_na$a, na.rm = TRUE),
                         mean(df_na$b, na.rm = TRUE),
                         mean(df_na$c, na.rm = TRUE)))
  expect_equal(t$variance, c(var(df_na$a, na.rm = TRUE),
                              var(df_na$b, na.rm = TRUE),
                              var(df_na$c, na.rm = TRUE)))
})

test_that("measures return NA for an all-missing numeric column (#132)", {

  d <- data.frame(
    a = c(1, 2, NA, 4),
    b = c(NA_real_, NA_real_, NA_real_, NA_real_)
  )

  v <- ds_measures_variation(d)
  expect_equal(nrow(v), 2)
  expect_equal(v$n[v$var == "b"], 0)
  expect_true(all(is.na(v[v$var == "b", -(1:2)])))

  s <- ds_measures_symmetry(d)
  expect_equal(nrow(s), 2)
  expect_equal(s$n[s$var == "b"], 0)
  expect_true(all(is.na(s[s$var == "b", -(1:2)])))

  p <- ds_percentiles(d)
  expect_equal(nrow(p), 2)
  expect_equal(p$n[p$var == "b"], 0)
  expect_true(all(is.na(p[p$var == "b", -(1:2)])))

  t <- ds_tidy_stats(d)
  expect_equal(nrow(t), 2)
  expect_true(all(is.na(t[t$vars == "b", -1])))
})

test_that("extreme observations report original row indices (#133)", {

  d <- mtcarz
  d$mpg[1:2] <- NA

  e <- ds_extreme_obs(d, mpg)
  expect_equal(e$value[e$type == "high"][1], 33.9)
  expect_equal(e$index[e$type == "high"][1], 20)
  expect_equal(e$index[e$type == "high"], c(20, 18, 28, 19, 26))

  # ties are reported on distinct rows
  tied <- ds_extreme_obs(c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5))
  expect_equal(tied$index[tied$type == "high"], c(10, 9, 8, 7, 6))
  expect_equal(tied$index[tied$type == "low"], c(1, 2, 3, 4, 5))
  expect_equal(length(unique(tied$index)), 10)

  # small samples use min(5, n) rows without recycling indices
  small <- ds_extreme_obs(c(3, 1, 2))
  expect_equal(small$value[small$type == "high"], c(3, 2, 1))
  expect_equal(small$index[small$type == "high"], c(1, 3, 2))
  expect_equal(small$value[small$type == "low"], c(1, 2, 3))
  expect_equal(small$index[small$type == "low"], c(2, 3, 1))

  # decimals applies to data.frame input as well
  d2 <- data.frame(x = c(1.23456, 2.34567, 3.45678, 4.56789, 5.67891))
  rounded <- ds_extreme_obs(d2, x, decimals = 3)
  expect_equal(rounded$value,
               round(c(sort(d2$x, decreasing = TRUE), sort(d2$x)), 3))
})

test_that("ds_rindex reports original positions and handles ties (#133)", {

  x <- c(NA, NA, 3, 1, 2)
  expect_equal(ds_rindex(x, 1), 4)
  expect_equal(ds_rindex(x, c(1, 2)), c(4, 5))
  expect_equal(ds_rindex(c(1, 1, 2), 1), c(1, 2))
})

test_that("geometric and harmonic means are numerically stable (#134)", {

  expect_equal(ds_gmean(rep(1000, 500)), 1000)

  large <- c(1e50, 2e50, 3e50, 4e50, 5e50)
  expect_false(is.infinite(ds_gmean(large)))
  expect_equal(ds_gmean(large), exp(mean(log(large))), tolerance = 1e-10)

  expect_warning(g <- ds_gmean(c(-5, 10, 20)),
                 "Geometric mean requires strictly positive")
  expect_true(is.na(g))

  expect_warning(g0 <- ds_gmean(c(0, 10, 20)),
                 "Geometric mean requires strictly positive")
  expect_true(is.na(g0))

  expect_warning(h <- ds_hmean(c(-5, 10, 20)),
                 "Harmonic mean requires strictly positive non-zero values")
  expect_true(is.na(h))

  expect_warning(h0 <- ds_hmean(c(0, 10, 20)),
                 "Harmonic mean requires strictly positive non-zero values")
  expect_true(is.na(h0))
})


test_that("decimals is applied to data-frame input", {

  variation <- ds_measures_variation(mtcarz, mpg, decimals = 1)
  expect_equal(variation$range, 23.5)
  expect_equal(variation$iqr, 7.4)
  expect_equal(variation$sd, 6)

  symmetry <- ds_measures_symmetry(mtcarz, mpg, decimals = 4)
  expect_equal(symmetry$skewness, round(ds_skewness(mtcarz$mpg), 4))

  percentiles <- ds_percentiles(mtcarz, mpg, decimals = 1)
  expect_equal(percentiles$median, 19.2)
  expect_equal(percentiles$q1, 15.4)

  # the defaults are unchanged
  expect_equal(ds_measures_variation(mtcarz, mpg)$sd, 6.03)
  expect_equal(ds_measures_symmetry(mtcarz, mpg)$skewness, 0.67)
  expect_equal(ds_percentiles(mtcarz, mpg)$median, 19.2)
})

test_that("ds_tailobs counts non-missing observations", {

  x <- c(1, 2, NA, NA, NA, 6)

  expect_error(
    ds_tailobs(x, 4),
    "n must be less than or equal to the number of non-missing observations"
  )
  expect_equal(ds_tailobs(x, 3), c(1, 2, 6))
  expect_equal(ds_tailobs(x, 3, type = "high"), c(6, 2, 1))
})
