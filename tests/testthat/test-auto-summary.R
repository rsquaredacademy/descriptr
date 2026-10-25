ndata <- dplyr::select(mtcarz, mpg, disp, hp, wt, qsec, drat)
fdata <- dplyr::select(mtcarz, cyl, gear, am, vs)
sdata <- dplyr::select(mtcarz, mpg)
gdata <- dplyr::select(mtcarz, cyl, mpg)

test_that("ds_auto_summary throws errors as expected", {
  expect_error(ds_auto_summary_stats(fdata), 'Data has no continuous variables.')
  expect_error(ds_auto_summary_stats(mtcarz, cyl, gear), 'Data has no continuous variables.')
  expect_error(ds_auto_summary_stats(mtcarz, cyl), 'Data has no continuous variables.')
})

test_that("ds_auto_group_summary throws errors as expected", {
  expect_error(ds_auto_group_summary(fdata), 'Data has no continuous variables.')
  expect_error(ds_auto_group_summary(ndata), 'Data has no categorical variables.')
  expect_error(ds_auto_group_summary(mtcarz, cyl, gear), 'Data has no continuous variables.')
  expect_error(ds_auto_group_summary(mtcarz, mpg, disp), 'Data has no categorical variables.')
})

test_that("ds_auto_freq_table throws errors as expected", {
  expect_error(ds_auto_freq_table(ndata), 'Data has no categorical variables.')
  expect_error(ds_auto_freq_table(mtcarz, mpg, disp), 'Data has no categorical variables.')
})

test_that("ds_auto_cross_table throws errors as expected", {
  expect_error(ds_auto_cross_table(ndata), 'ds_auto_cross_table() needs at least 2 categorical variables; found 0.', fixed = TRUE)
  expect_error(ds_auto_cross_table(mtcarz, mpg, disp), 'ds_auto_cross_table() needs at least 2 categorical variables; found 0.', fixed = TRUE)
  expect_error(ds_auto_cross_table(mtcarz, cyl), 'ds_auto_cross_table() needs at least 2 categorical variables; found 1.', fixed = TRUE)
  expect_error(ds_auto_cross_table(data.frame(a = factor("x"))),
    'ds_auto_cross_table() needs at least 2 categorical variables; found 1.', fixed = TRUE)
})

test_that("output from ds_auto_summary is as expected", {
  expect_snapshot(ds_auto_summary_stats(mtcarz, mpg))
  expect_snapshot(ds_auto_summary_stats(sdata))
})

test_that("output from ds_auto_group_summary is as expected", {
  expect_snapshot(ds_auto_group_summary(mtcarz, cyl, mpg))
  expect_snapshot(ds_auto_group_summary(gdata))
})


test_that("output from ds_auto_cross_table is as expected", {
  expect_snapshot(ds_auto_cross_table(mtcarz, cyl, gear))
})

test_that("output from ds_auto_freq_table is as expected", {
  expect_snapshot(ds_auto_freq_table(mtcarz, cyl, gear))
})

test_that("ds_auto_summary_stats does not print a stray NULL", {

  out <- capture.output(suppressWarnings(ds_auto_summary_stats(mtcarz, mpg)))
  expect_false(any(trimws(out) == "NULL"))
})

test_that("auto tables treat character columns as categorical", {

  char_data <- data.frame(
    a = c("b", "a", "b", "c"),
    b = c("y", "x", "y", "x"),
    z = 1:4
  )
  fact_data <- data.frame(
    a = factor(char_data$a),
    b = factor(char_data$b),
    z = char_data$z
  )

  expect_equal(
    suppressWarnings(capture.output(ds_auto_freq_table(char_data))),
    suppressWarnings(capture.output(ds_auto_freq_table(fact_data)))
  )
  expect_equal(
    capture.output(ds_auto_cross_table(char_data)),
    capture.output(ds_auto_cross_table(fact_data))
  )
  expect_equal(
    capture.output(ds_auto_group_summary(char_data)),
    capture.output(ds_auto_group_summary(fact_data))
  )
})

test_that("ds_auto_cross_table reports each table's own observation count", {

  g <- mtcarz
  g$cyl[1]  <- NA
  g$gear[2] <- NA

  out <- capture.output(ds_auto_cross_table(g, cyl, gear, am))

  # cyl vs gear drops rows 1 and 2, the other two pairs drop one row each
  expect_true(any(grepl("Total Observations:  30", out, fixed = TRUE)))
  expect_equal(sum(grepl("Total Observations:  31", out, fixed = TRUE)), 2)
  expect_false(any(grepl("Total Observations:  32", out, fixed = TRUE)))

  expect_equal(sum(grepl("Missing Observations:  1", out, fixed = TRUE)), 2)
  expect_equal(sum(grepl("Missing Observations:  2", out, fixed = TRUE)), 1)
})
