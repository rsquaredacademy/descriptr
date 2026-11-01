test_that("output from ds_group_summary_interact is as expected", {
  actual <-
    ds_group_summary_interact(mtcarz, mpg, cyl, gear) %>%
    dplyr::select(min) %>%
    sum() %>%
    round(2)
  expect_equal(actual, 149.9)
})

test_that("ds_group_summary_interact keeps colliding group labels separate (H2)", {

  d <- data.frame(
    g = factor(c("a\rb", "a")),
    h = factor(c("c", "b\rc")),
    x = c(1, 100)
  )

  k <- ds_group_summary_interact(d, x, g, h)

  expect_equal(nrow(k), 2)
  expect_setequal(paste(k$g, k$h, sep = "|"), c("a\rb|c", "a|b\rc"))
  # the values of the two distinct tuples must not be pooled
  expect_equal(sort(k$mean), c(1, 100))
  expect_equal(sort(k$min), c(1, 100))
  expect_equal(sort(k$max), c(1, 100))
  expect_equal(sort(k$median), c(1, 100))

  # same collision, with a tuple that has more than one observation
  d2 <- data.frame(
    g = factor(c("a\rb", "a", "a")),
    h = factor(c("c", "b\rc", "b\rc")),
    x = c(1, 100, 20)
  )

  k2 <- ds_group_summary_interact(d2, x, g, h)

  expect_equal(nrow(k2), 2)
  expect_equal(sort(k2$mean), c(1, 60))
  expect_equal(sort(k2$min), c(1, 20))
  expect_equal(sort(k2$max), c(1, 100))
})

test_that("ds_group_summary_interact does not overwrite source columns (H1)", {

  # continuous variable named `min`
  d <- data.frame(g = factor(c("a", "a", "b")), min = c(4, 8, 2))
  k <- ds_group_summary_interact(d, min, g)

  expect_equal(k$min, c(4, 2))
  expect_equal(k$max, c(8, 2))
  expect_equal(k$mean, c(6, 2))

  # continuous variable named `Levels`
  d2 <- data.frame(g = factor(c("a", "a", "b")), Levels = c(4, 8, 2))
  k2 <- ds_group_summary_interact(d2, Levels, g)

  expect_equal(k2$min, c(4, 2))
  expect_equal(k2$max, c(8, 2))
  expect_equal(k2$mean, c(6, 2))

  # grouping column named like a statistic keeps its labels
  d3 <- data.frame(min = factor(c("a", "a", "b")), x = c(1, 2, 3))
  k3 <- ds_group_summary_interact(d3, x, min)

  expect_equal(as.character(k3[[1]]), c("a", "b"))
  expect_equal(k3[[2]], c(1, 3))
})

test_that("ds_group_summary_interact throws the appropriate error", {

  expect_error(
    ds_group_summary_interact(mtcarz, am, cyl, gear),
    "am is not a continuous variable. The function expects an object of type `numeric` or `integer` but am is of type `factor`."
  )

  expect_error(
    ds_group_summary_interact(mtcarz, mpg, cyl, disp, hp),
    "Below grouping variables are not categorical: 
- disp
- hp"
  )

  expect_error(
    ds_group_summary_interact(mtcarz, mpg, cyl, hp),
    "Below grouping variables are not categorical: 
- hp"
  )
})

test_that("ds_group_summary_interact keeps factor levels containing dots", {

  g <- mtcarz
  levels(g$cyl) <- c("a.b", "c", "d")

  k <- ds_group_summary_interact(g, mpg, cyl)

  expect_true("cyl" %in% names(k))
  expect_setequal(k$cyl, levels(g$cyl))
  expect_false(any(is.na(k$cyl)))

  expect_equal(k$min, as.numeric(tapply(g$mpg, g$cyl, min))[match(k$cyl, levels(g$cyl))])
})

test_that("ds_group_summary_interact uses character grouping columns", {

  g <- mtcarz
  g$cyl <- as.character(g$cyl)

  k <- ds_group_summary_interact(g, mpg, cyl)

  expect_true("cyl" %in% names(k))
  expect_setequal(k$cyl, unique(g$cyl))

  group_min <- tapply(g$mpg, g$cyl, min)
  expect_equal(k$min, as.numeric(group_min)[match(k$cyl, names(group_min))])
})

test_that("ds_group_summary_interact gives the same result for character and factor groups (regression guard)", {

  char_data <- data.frame(
    g = c("b", "a", "b", "c"),
    h = c("y", "x", "y", "x"),
    x = c(1, 2, 3, 4)
  )
  fact_data <- data.frame(
    g = factor(char_data$g),
    h = factor(char_data$h),
    x = char_data$x
  )

  expect_equal(
    ds_group_summary_interact(char_data, x, g, h),
    ds_group_summary_interact(fact_data, x, g, h)
  )
})
