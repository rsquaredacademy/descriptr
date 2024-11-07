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
  expect_error(ds_auto_cross_table(ndata), 'Data has no categorical variables.')
  expect_error(ds_auto_cross_table(mtcarz, mpg, disp), 'Data has no categorical variables.')
  expect_error(ds_auto_cross_table(mtcarz, cyl), 'Two way table requires at least 2 categorical variables.')
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
