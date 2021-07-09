context("test-plots")

ndata <- dplyr::select(mtcarz, mpg, disp)
fdata <- dplyr::select(mtcarz, cyl, gear)
mdata <- dplyr::select(mtcarz, cyl, mpg)

test_that("output from ds_screener is as expected", {
  x <- ds_screener(airquality)
  p <- plot(x)
  expect_doppelganger("ds_screener_1", p)
})

test_that("output from ds_plot_scatter is as expected", {
  p <- ds_plot_scatter(mtcarz, mpg, disp, print_plot = FALSE)
  expect_doppelganger("ds_plot_scatter_1", p[[1]])
  expect_doppelganger("ds_plot_scatter_2", p[[2]])

  q <- ds_plot_scatter(ndata, print_plot = FALSE)
  expect_doppelganger("ds_plot_scatter_3", q[[1]])
  expect_doppelganger("ds_plot_scatter_4", q[[2]])
})

test_that("output from ds_plot_histogram is as expected", {
  p <- ds_plot_histogram(mtcarz, mpg, disp, hp, print_plot = FALSE)
  expect_doppelganger("ds_plot_hist_1", p[[1]])
  expect_doppelganger("ds_plot_hist_2", p[[2]])
  expect_doppelganger("ds_plot_hist_3", p[[3]])

  q <- ds_plot_histogram(ndata, print_plot = FALSE)
  expect_doppelganger("ds_plot_hist_4", q[[1]])
  expect_doppelganger("ds_plot_hist_5", q[[2]])
})

test_that("output from ds_plot_density is as expected", {
  # skip_if(getRversion() > '4.0.3')
  p <- ds_plot_density(mtcarz, mpg, disp, hp, print_plot = FALSE)
  expect_doppelganger("ds_plot_density_1", p[[1]])
  expect_doppelganger("ds_plot_density_2", p[[2]])
  expect_doppelganger("ds_plot_density_3", p[[3]])

  q <- ds_plot_density(ndata, print_plot = FALSE)
  expect_doppelganger("ds_plot_density_4", q[[1]])
  expect_doppelganger("ds_plot_density_5", q[[2]])
})

test_that("output from ds_plot_bar is as expected", {
  p <- ds_plot_bar(mtcarz, cyl, gear, print_plot = FALSE)
  expect_doppelganger("ds_plot_bar_1", p[[1]])
  expect_doppelganger("ds_plot_bar_2", p[[2]])

  q <- ds_plot_bar(fdata, print_plot = FALSE)
  expect_doppelganger("ds_plot_bar_3", q[[1]])
  expect_doppelganger("ds_plot_bar_4", q[[2]])

})

test_that("output from ds_plot_box_single is as expected", {
  p <- ds_plot_box_single(mtcarz, mpg, disp, hp, print_plot = FALSE)
  expect_doppelganger("ds_plot_box_single_1", p[[1]])
  expect_doppelganger("ds_plot_box_single_2", p[[2]])

  q <- ds_plot_box_single(ndata, print_plot = FALSE)
  expect_doppelganger("ds_plot_box_single_3", q[[1]])
  expect_doppelganger("ds_plot_box_single_4", q[[2]])
  
})

test_that("output from ds_plot_bar_stacked is as expected", {

  p <- ds_plot_bar_stacked(mtcarz, cyl, gear, print_plot = FALSE)
  expect_doppelganger("ds_plot_bar_stacked_1", p[[1]])
  expect_doppelganger("ds_plot_bar_stacked_2", p[[2]])

  q <- ds_plot_bar_stacked(fdata, print_plot = FALSE)
  expect_doppelganger("ds_plot_bar_stacked_3", q[[1]])
  expect_doppelganger("ds_plot_bar_stacked_4", q[[2]])
})

test_that("output from ds_plot_bar_grouped is as expected", {

  p <- ds_plot_bar_grouped(mtcarz, cyl, gear, print_plot = FALSE)
  expect_doppelganger("ds_plot_bar_grouped_1", p[[1]])
  expect_doppelganger("ds_plot_bar_grouped_2", p[[2]])

  q <- ds_plot_bar_grouped(fdata, print_plot = FALSE)
  expect_doppelganger("ds_plot_bar_grouped_3", q[[1]])
  expect_doppelganger("ds_plot_bar_grouped_4", q[[2]])

})

test_that("output from ds_plot_box_group is as expected", {

  p <- ds_plot_box_group(mtcarz, cyl, gear, mpg, print_plot = FALSE)
  expect_doppelganger("ds_plot_box_group_1", p[[1]])
  expect_doppelganger("ds_plot_box_group_2", p[[2]])

  q <- ds_plot_box_group(mdata, print_plot = FALSE)
  expect_doppelganger("ds_plot_box_group_3", q[[1]])
  
})

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
