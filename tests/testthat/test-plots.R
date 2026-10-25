ndata <- dplyr::select(mtcarz, mpg, disp)
fdata <- dplyr::select(mtcarz, cyl, gear)
mdata <- dplyr::select(mtcarz, cyl, mpg)

test_that("output from ds_screener is as expected", {
  x <- ds_screener(airquality)
  p <- plot(x)
  vdiffr::expect_doppelganger("ds_screener_1", p)
})

test_that("output from ds_plot_scatter is as expected", {
  p <- ds_plot_scatter(mtcarz, mpg, disp, print_plot = FALSE)
  vdiffr::expect_doppelganger("ds_plot_scatter_1", p[[1]])
  vdiffr::expect_doppelganger("ds_plot_scatter_2", p[[2]])

  q <- ds_plot_scatter(ndata, print_plot = FALSE)
  vdiffr::expect_doppelganger("ds_plot_scatter_3", q[[1]])
  vdiffr::expect_doppelganger("ds_plot_scatter_4", q[[2]])
})

test_that("output from ds_plot_histogram is as expected", {
  p <- ds_plot_histogram(mtcarz, mpg, disp, hp, print_plot = FALSE)
  vdiffr::expect_doppelganger("ds_plot_hist_1", p[[1]])
  vdiffr::expect_doppelganger("ds_plot_hist_2", p[[2]])
  vdiffr::expect_doppelganger("ds_plot_hist_3", p[[3]])

  q <- ds_plot_histogram(ndata, print_plot = FALSE)
  vdiffr::expect_doppelganger("ds_plot_hist_4", q[[1]])
  vdiffr::expect_doppelganger("ds_plot_hist_5", q[[2]])
})

test_that("output from ds_plot_density is as expected", {
  # skip_if(getRversion() > '4.0.3')
  p <- ds_plot_density(mtcarz, mpg, disp, hp, print_plot = FALSE)
  vdiffr::expect_doppelganger("ds_plot_density_1", p[[1]])
  vdiffr::expect_doppelganger("ds_plot_density_2", p[[2]])
  vdiffr::expect_doppelganger("ds_plot_density_3", p[[3]])

  q <- ds_plot_density(ndata, print_plot = FALSE)
  vdiffr::expect_doppelganger("ds_plot_density_4", q[[1]])
  vdiffr::expect_doppelganger("ds_plot_density_5", q[[2]])
})

test_that("output from ds_plot_bar is as expected", {
  p <- ds_plot_bar(mtcarz, cyl, gear, print_plot = FALSE)
  vdiffr::expect_doppelganger("ds_plot_bar_1", p[[1]])
  vdiffr::expect_doppelganger("ds_plot_bar_2", p[[2]])

  q <- ds_plot_bar(fdata, print_plot = FALSE)
  vdiffr::expect_doppelganger("ds_plot_bar_3", q[[1]])
  vdiffr::expect_doppelganger("ds_plot_bar_4", q[[2]])

})

test_that("output from ds_plot_box_single is as expected", {
  p <- ds_plot_box_single(mtcarz, mpg, disp, hp, print_plot = FALSE)
  vdiffr::expect_doppelganger("ds_plot_box_single_1", p[[1]])
  vdiffr::expect_doppelganger("ds_plot_box_single_2", p[[2]])

  q <- ds_plot_box_single(ndata, print_plot = FALSE)
  vdiffr::expect_doppelganger("ds_plot_box_single_3", q[[1]])
  vdiffr::expect_doppelganger("ds_plot_box_single_4", q[[2]])
  
})

test_that("output from ds_plot_bar_stacked is as expected", {

  p <- ds_plot_bar_stacked(mtcarz, cyl, gear, print_plot = FALSE)
  vdiffr::expect_doppelganger("ds_plot_bar_stacked_1", p[[1]])
  vdiffr::expect_doppelganger("ds_plot_bar_stacked_2", p[[2]])

  q <- ds_plot_bar_stacked(fdata, print_plot = FALSE)
  vdiffr::expect_doppelganger("ds_plot_bar_stacked_3", q[[1]])
  vdiffr::expect_doppelganger("ds_plot_bar_stacked_4", q[[2]])
})

test_that("output from ds_plot_bar_grouped is as expected", {

  p <- ds_plot_bar_grouped(mtcarz, cyl, gear, print_plot = FALSE)
  vdiffr::expect_doppelganger("ds_plot_bar_grouped_1", p[[1]])
  vdiffr::expect_doppelganger("ds_plot_bar_grouped_2", p[[2]])

  q <- ds_plot_bar_grouped(fdata, print_plot = FALSE)
  vdiffr::expect_doppelganger("ds_plot_bar_grouped_3", q[[1]])
  vdiffr::expect_doppelganger("ds_plot_bar_grouped_4", q[[2]])

})

test_that("output from ds_plot_box_group is as expected", {

  p <- ds_plot_box_group(mtcarz, cyl, gear, mpg, print_plot = FALSE)
  vdiffr::expect_doppelganger("ds_plot_box_group_1", p[[1]])
  vdiffr::expect_doppelganger("ds_plot_box_group_2", p[[2]])

  q <- ds_plot_box_group(mdata, print_plot = FALSE)
  vdiffr::expect_doppelganger("ds_plot_box_group_3", q[[1]])
  
})

ndata <- dplyr::select(mtcarz, mpg, disp, hp, wt, qsec, drat)
fdata <- dplyr::select(mtcarz, cyl, gear, am, vs)

test_that("ds_plot_scatter throws errors as expected", {
  expect_error(ds_plot_scatter(fdata), 'ds_plot_scatter() needs at least 2 continuous variables; found 0.', fixed = TRUE)
  expect_error(ds_plot_scatter(fdata, cyl, gear), 'ds_plot_scatter() needs at least 2 continuous variables; found 0.', fixed = TRUE)
  expect_error(ds_plot_scatter(ndata, mpg), 'ds_plot_scatter() needs at least 2 continuous variables; found 1.', fixed = TRUE)
  expect_error(ds_plot_scatter(data.frame(x = 1:10), print_plot = FALSE),
    'ds_plot_scatter() needs at least 2 continuous variables; found 1.', fixed = TRUE)
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
  expect_error(ds_plot_bar_stacked(ndata), 'ds_plot_bar_stacked() needs at least 2 categorical variables; found 0.', fixed = TRUE)
  expect_error(ds_plot_bar_stacked(mtcarz, mpg, disp), 'ds_plot_bar_stacked() needs at least 2 categorical variables; found 0.', fixed = TRUE)
  expect_error(ds_plot_bar_stacked(mtcarz, cyl), 'ds_plot_bar_stacked() needs at least 2 categorical variables; found 1.', fixed = TRUE)
  expect_error(ds_plot_bar_stacked(data.frame(a = factor("x")), print_plot = FALSE),
    'ds_plot_bar_stacked() needs at least 2 categorical variables; found 1.', fixed = TRUE)
})

test_that("ds_plot_bar_grouped throws errors as expected", {
  expect_error(ds_plot_bar_grouped(ndata), 'ds_plot_bar_grouped() needs at least 2 categorical variables; found 0.', fixed = TRUE)
  expect_error(ds_plot_bar_grouped(mtcarz, mpg, disp), 'ds_plot_bar_grouped() needs at least 2 categorical variables; found 0.', fixed = TRUE)
  expect_error(ds_plot_bar_grouped(mtcarz, cyl), 'ds_plot_bar_grouped() needs at least 2 categorical variables; found 1.', fixed = TRUE)
  expect_error(ds_plot_bar_grouped(data.frame(a = factor("x")), print_plot = FALSE),
    'ds_plot_bar_grouped() needs at least 2 categorical variables; found 1.', fixed = TRUE)
})

test_that("ds_plot_box_group throws errors as expected", {
  expect_error(ds_plot_box_group(fdata), 'Data has no continuous variables.')
  expect_error(ds_plot_box_group(ndata), 'Data has no categorical variables.')
  expect_error(ds_plot_box_group(fdata, cyl, gear), 'Data has no continuous variables.')
  expect_error(ds_plot_box_group(mtcarz, mpg, disp), 'Data has no categorical variables.')
})

test_that("categorical plots treat character columns as categorical", {

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

  p1 <- ds_plot_bar(char_data, print_plot = FALSE)
  p2 <- ds_plot_bar(fact_data, print_plot = FALSE)
  expect_equal(names(p1), names(p2))
  expect_equal(p1[[1]]$data, p2[[1]]$data)

  p1 <- ds_plot_bar_stacked(char_data, print_plot = FALSE)
  p2 <- ds_plot_bar_stacked(fact_data, print_plot = FALSE)
  expect_equal(names(p1), names(p2))
  expect_equal(p1[[1]]$data, p2[[1]]$data)

  p1 <- ds_plot_bar_grouped(char_data, print_plot = FALSE)
  p2 <- ds_plot_bar_grouped(fact_data, print_plot = FALSE)
  expect_equal(names(p1), names(p2))
  expect_equal(p1[[1]]$data, p2[[1]]$data)

  p1 <- ds_plot_box_group(char_data, print_plot = FALSE)
  p2 <- ds_plot_box_group(fact_data, print_plot = FALSE)
  expect_equal(names(p1), names(p2))
  expect_equal(p1[[1]]$data, p2[[1]]$data)
})
