test_that("deprecated wrappers forward to their replacements (L5)", {

  # ds_oway_tables() -> ds_auto_freq_table()
  out <- character()
  expect_warning(out <- capture.output(ds_oway_tables(mtcarz)), "deprecated")
  expect_equal(out, capture.output(ds_auto_freq_table(mtcarz)))
  expect_true(any(grepl("Variable: cyl", out, fixed = TRUE)))

  # ds_tway_tables() -> ds_auto_cross_table()
  out <- character()
  expect_warning(out <- capture.output(ds_tway_tables(mtcarz)), "deprecated")
  expect_equal(out, capture.output(ds_auto_cross_table(mtcarz)))

  # ds_multi_stats() -> ds_tidy_stats()
  expect_warning(k <- ds_multi_stats(mtcarz, mpg, disp), "deprecated")
  expect_equal(k, ds_tidy_stats(mtcarz, mpg, disp))
  expect_equal(k$min[k$vars == "disp"], 71.1)
})

test_that("ds_launch_shiny_app checks its suggested packages (L5)", {

  checked <- character()

  local_mocked_bindings(
    check_suggests = function(pkg) {
      checked <<- c(checked, pkg)
      if (length(checked) == 10L) {
        stop("all dependencies checked", call. = FALSE)
      }
    }
  )

  expect_error(ds_launch_shiny_app(), "all dependencies checked")
  expect_equal(
    checked,
    c("xplorerr", "haven", "jsonlite", "readr", "readxl", "shinyBS",
      "shinycssloaders", "shinythemes", "stringr", "lubridate")
  )
})

test_that("ds_launch_shiny_app launches the xplorerr app (L5)", {
  skip_if_not_installed("xplorerr")

  launched <- 0L
  local_mocked_bindings(check_suggests = function(pkg) invisible(NULL))
  local_mocked_bindings(
    app_descriptive = function(...) launched <<- launched + 1L,
    .package = "xplorerr"
  )

  ds_launch_shiny_app()
  expect_equal(launched, 1L)
})
