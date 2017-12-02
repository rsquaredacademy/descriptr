context('multistats')

test_that('output from multistats is as expected', {

  actual <- ds_multi_stats(mtcars, mpg, disp, hp) %>%
    filter(vars == 'disp') %>%
    select(min)
  expected <- tibble(min = 71.1)
  expect_equal(actual, expected)
})
