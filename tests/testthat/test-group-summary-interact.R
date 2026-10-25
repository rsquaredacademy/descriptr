test_that("output from ds_group_summary_interact is as expected", {
  actual <-
    ds_group_summary_interact(mtcarz, mpg, cyl, gear) %>%
    dplyr::select(min) %>%
    sum() %>%
    round(2)
  expect_equal(actual, 149.9)
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
