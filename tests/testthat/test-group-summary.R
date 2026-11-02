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


test_that("ds_group_summary drops rows with a missing grouping label", {
  g <- mtcarz
  g$cyl[2] <- NA

  k <- ds_group_summary(g, cyl, mpg)

  expect_equal(nrow(k$tidy_stats), ncol(k$stats) - 1)
  expect_false(anyNA(k$tidy_stats$cyl))
  expect_equal(names(k$stats), c("Statistic/Levels", "4", "6", "8"))
})

test_that("group summary boxplots exclude missing grouping labels (RH2)", {

  d <- data.frame(g = factor(c("a", "a", NA)), x = c(1, 3, 100))
  k <- ds_group_summary(d, g, x)

  expect_equal(names(k$stats), c("Statistic/Levels", "a"))
  expect_equal(k$stats[["a"]][k$stats[[1]] == "Mean"], 2)

  p <- plot(k, print_plot = FALSE)
  boxes <- ggplot2::ggplot_build(p)$data[[1]]

  expect_equal(nrow(boxes), 1)
  expect_equal(boxes$middle, 2)
  expect_equal(boxes$ymin, 1)
  expect_equal(boxes$ymax, 3)
  expect_false(anyNA(boxes$x))
})

test_that("ds_group_summary treats a character grouping column as categorical", {

  char_data <- data.frame(g = c("b", "a", "b", "c"), x = c(1, 2, 3, 4))
  fact_data <- data.frame(g = factor(char_data$g), x = char_data$x)

  k_char <- ds_group_summary(char_data, g, x)
  k_fact <- ds_group_summary(fact_data, g, x)

  expect_equal(k_char$stats, k_fact$stats)
  expect_equal(k_char$tidy_stats, k_fact$tidy_stats)
  expect_equal(names(k_char$stats), c("Statistic/Levels", "a", "b", "c"))
})

test_that("ds_group_summary stats have default row names", {
  k <- ds_group_summary(mtcarz, cyl, mpg)

  # 17 rows now: Obs, Missing + 15 statistics
  expect_equal(rownames(k$stats), as.character(seq_len(nrow(k$stats))))
})

test_that("ds_group_summary does not overwrite source columns named like statistics (H1)", {

  stat <- function(x, level, name) {
    col <- x$stats[[level]]
    names(col) <- x$stats[[1]]
    unname(col[[name]])
  }

  # continuous variable named `missing`
  d <- data.frame(g = factor(c("a", "a", "b")), missing = c(1, 3, 5))
  k <- ds_group_summary(d, g, missing)

  expect_equal(stat(k, "a", "Minimum"), 1)
  expect_equal(stat(k, "a", "Maximum"), 3)
  expect_equal(stat(k, "a", "Mean"), 2)
  expect_equal(stat(k, "a", "Variance"), 2)
  expect_equal(stat(k, "b", "Minimum"), 5)
  expect_equal(stat(k, "b", "Maximum"), 5)
  expect_equal(stat(k, "b", "Mean"), 5)
  expect_true(is.na(stat(k, "b", "Variance")))

  tidy <- as.data.frame(k$tidy_stats)
  expect_equal(tidy$min, c(1, 5))
  expect_equal(tidy$max, c(3, 5))
  expect_equal(tidy$mean, c(2, 5))
  expect_equal(tidy$variance, c(2, NA))

  # continuous variable named `min`
  d2 <- data.frame(g = factor(c("a", "a", "b")), min = c(4, 8, 2))
  k2 <- ds_group_summary(d2, g, min)

  expect_equal(stat(k2, "a", "Minimum"), 4)
  expect_equal(stat(k2, "a", "Maximum"), 8)
  expect_equal(stat(k2, "a", "Mean"), 6)
  expect_equal(stat(k2, "b", "Mean"), 2)
  expect_equal(as.data.frame(k2$tidy_stats)$max, c(8, 2))
  expect_equal(as.data.frame(k2$tidy_stats)$mean, c(6, 2))

  # continuous variable named `mean`
  d3 <- data.frame(g = factor(c("a", "a", "b")), mean = c(1, 2, 9))
  k3 <- ds_group_summary(d3, g, mean)

  expect_equal(stat(k3, "a", "Mean"), 1.5)
  expect_equal(stat(k3, "a", "Median"), 1.5)
  expect_equal(as.data.frame(k3$tidy_stats)$median, c(1.5, 9))

  # grouping column named `length`
  d4 <- data.frame(length = factor(c("a", "b", "a")), x = c(1, 2, 3))
  k4 <- ds_group_summary(d4, length, x)

  expect_equal(names(k4$stats), c("Statistic/Levels", "a", "b"))
  expect_equal(as.character(k4$tidy_stats[[1]]), c("a", "b"))
  expect_equal(k4$tidy_stats[[2]], c(2L, 1L))
})

test_that("ds_group_summary returns unique column names (RM1)", {

  d <- data.frame(length = factor(c("a", "b", "a")), x = c(1, 2, 3))
  k <- ds_group_summary(d, length, x)

  expect_false(anyDuplicated(names(k$tidy_stats)) > 0L)
  expect_equal(names(k$tidy_stats)[1], "length_group")
  expect_equal(as.character(k$tidy_stats$length_group), c("a", "b"))
  # the statistic keeps its name and both values survive
  expect_equal(k$tidy_stats$length, c(2L, 1L))

  # ordinary dplyr transformations work on the result
  expect_silent(mutated <- dplyr::mutate(k$tidy_stats, z = length * 2))
  expect_equal(mutated$z, c(4L, 2L))
  expect_silent(filtered <- dplyr::filter(k$tidy_stats, .data[["mean"]] > 0))
  expect_equal(nrow(filtered), 2)

  # the printed table is unaffected by the rename
  expect_equal(names(k$stats), c("Statistic/Levels", "a", "b"))
  expect_equal(k$stats[["a"]][k$stats[[1]] == "Obs"], 2)
})

test_that("ds_group_summary keeps ordered grouping keys ordered (RM2)", {

  d <- data.frame(
    g = ordered(c("a", "a", "b", "b", "c", "c"),
                levels = c("c", "b", "a", "unused")),
    x = c(1, 3, 5, 7, 9, 11)
  )

  k <- ds_group_summary(d, g, x)

  expect_true(is.ordered(k$tidy_stats$g))
  expect_equal(class(k$tidy_stats$g), c("ordered", "factor"))
  # observed levels keep their original order; the unused level is dropped
  expect_equal(as.character(k$tidy_stats$g), c("c", "b", "a"))
  expect_equal(levels(k$tidy_stats$g), c("c", "b", "a"))
  expect_equal(k$tidy_stats$mean, c(10, 6, 2))
  expect_true(k$tidy_stats$g[1] < k$tidy_stats$g[2])
  expect_true(k$tidy_stats$g[2] < k$tidy_stats$g[3])

  # unordered factors are still returned as factors
  d2 <- data.frame(g = factor(c("a", "a", "b")), x = c(1, 2, 3))
  expect_equal(class(ds_group_summary(d2, g, x)$tidy_stats$g), "factor")
})

test_that("ds_group_summary reports when there are no usable groups (M2)", {

  d <- data.frame(g = factor(c(NA, NA), levels = "a"), x = 1:2)
  expect_error(ds_group_summary(d, g, x), "No data to summarise")

  d0 <- data.frame(g = factor(character()), x = numeric())
  expect_error(ds_group_summary(d0, g, x), "No data to summarise")
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


