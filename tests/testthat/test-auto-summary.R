context("test-auto-summary")

ndata <- dplyr::select(mtcarz, mpg, disp, hp, wt, qsec, drat)
fdata <- dplyr::select(mtcarz, cyl, gear, am, vs)

test_that("ds_auto_summary throws errors as expected", {
  expect_error(ds_auto_summary(fdata), 'Data has no continuous variables.')
  expect_error(ds_auto_summary(mtcarz, cyl, gear), 'Data has no continuous variables.')
})

test_that("ds_auto_group_summary throws errors as expected", {
  expect_error(ds_auto_group_summary(fdata), 'Data has no continuous variables.')
  expect_error(ds_auto_group_summary(ndata), 'Data has no categorical variables.')
  expect_error(ds_auto_group_summary(mtcarz, cyl, gear), 'Data has no continuous variables.')
  expect_error(ds_auto_group_summary(mtcarz, mpg, disp), 'Data has no categorical variables.')
})

test_that("ds_auto_tabulation throws errors as expected", {
  expect_error(ds_auto_tabulation(ndata), 'Data has no categorical variables.')
  expect_error(ds_auto_tabulation(mtcarz, mpg, disp), 'Data has no categorical variables.')
})

test_that("ds_auto_cross_table throws errors as expected", {
  expect_error(ds_auto_cross_table(ndata), 'Data has no categorical variables.')
  expect_error(ds_auto_cross_table(mtcarz, mpg, disp), 'Data has no categorical variables.')
  expect_error(ds_auto_cross_table(mtcarz, cyl), 'Two way table requires at least 2 categorical variables.')
})