context('summary_stats')

test_that('summary_stats returns the appropriate error', {

  expect_error(summary_stats('mtcars$mpg'), 'data must be numeric')
  expect_error(summary_stats(as.factor(mtcars$mpg)), 'data must be numeric')

})
