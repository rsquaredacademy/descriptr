context('freq_table')

test_that('output from freq_table matches expected results', {

  k <- freq_table(as.factor(mtcars$cyl))
  expect_equivalent(k$ftable[[1]], c("4", "6", "8"))
  expect_equivalent(k$ftable[[2]], c(11, 7, 14))
  expect_equivalent(k$ftable[[3]], c(11, 18, 32))
  expect_equivalent(k$ftable[[4]], c(34.38, 21.88, 43.75))
  expect_equivalent(k$ftable[[5]], c(34.38, 56.25, 100.00))
  expect_equivalent(k$varname, 'cyl)')
})

test_that('freq_table returns appropriate errors', {

  expect_error(freq_table('mtcars$cyl'), 'data must be categorical/qualitative')

})
