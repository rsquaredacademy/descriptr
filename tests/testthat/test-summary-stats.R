context('summary_stats')

test_that('ds_summary_stats returns the appropriate error', {

  expect_error(ds_summary_stats('mtcars$mpg'), 'data must be numeric')
  expect_error(ds_summary_stats(as.factor(mtcars$mpg)), 'data must be numeric')

})
