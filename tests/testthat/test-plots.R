context("test-plots")

ndata <- dplyr::select(mtcarz, mpg, disp, hp, wt, qsec, drat)
fdata <- dplyr::select(mtcarz, cyl, gear, am, vs)

test_that("ds_plot_scatter throws errors as expected", {
  expect_error(ds_plot_scatter(fdata), 'Data has no continuous variables.')
  expect_error(ds_plot_scatter(fdata, cyl, gear), 'Data has no continuous variables.')
  expect_error(ds_plot_scatter(ndata, mpg), 'Scatter plot requires 2 continuous variables.')
})

test_that("ds_plot_histogram throws errors as expected", {
  expect_error(ds_plot_histogram(fdata), 'Data has no continuous variables.')
  expect_error(ds_plot_histogram(fdata, cyl, gear), 'Data has no continuous variables.')
})

test_that("ds_plot_density throws errors as expected", {
  expect_error(ds_plot_density(fdata), 'Data has no continuous variables.')
  expect_error(ds_plot_density(fdata, cyl, gear), 'Data has no continuous variables.')
})

test_that("ds_plot_bar throws errors as expected", {
  expect_error(ds_plot_bar(ndata), 'Data has no categorical variables.')
  expect_error(ds_plot_bar(mtcarz, mpg, disp), 'Data has no categorical variables.')
})

test_that("ds_plot_box_single throws errors as expected", {
  expect_error(ds_plot_box_single(fdata), 'Data has no continuous variables.')
  expect_error(ds_plot_box_single(fdata, cyl, gear), 'Data has no continuous variables.')
})

test_that("ds_plot_bar_stacked throws errors as expected", {
  expect_error(ds_plot_bar_stacked(ndata), 'Data has no categorical variables.')
  expect_error(ds_plot_bar_stacked(mtcarz, mpg, disp), 'Data has no categorical variables.')
  expect_error(ds_plot_bar_stacked(mtcarz, cyl), 'Stacked bar plot requires 2 categorical variables.')
})

test_that("ds_plot_bar_grouped throws errors as expected", {
  expect_error(ds_plot_bar_grouped(ndata), 'Data has no categorical variables.')
  expect_error(ds_plot_bar_grouped(mtcarz, mpg, disp), 'Data has no categorical variables.')
  expect_error(ds_plot_bar_grouped(mtcarz, cyl), 'Grouped bar plot requires 2 categorical variables.')
})

test_that("ds_plot_box_group throws errors as expected", {
  expect_error(ds_plot_box_group(fdata), 'Data has no continuous variables.')
  expect_error(ds_plot_box_group(ndata), 'Data has no categorical variables.')
  expect_error(ds_plot_box_group(fdata, cyl, gear), 'Data has no continuous variables.')
  expect_error(ds_plot_box_group(mtcarz, mpg, disp), 'Data has no categorical variables.')
})

