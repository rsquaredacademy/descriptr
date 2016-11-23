context('binom_dist')

test_that('output from binom_plot matches expected result', {

  k <- binom_plot(6, 0.5)
  expect_equal(round(k$avg), 3)
  expect_equal(k$stdev, 1.22)

  k <- binom_plot(25, 0)
  expect_equal(round(k$avg), 0)
  expect_equal(k$stdev, 0)

  k <- binom_plot(44, 1)
  expect_equal(round(k$avg), 44)
  expect_equal(k$stdev, 0)

  k <- binom_plot(115, 0.6)
  expect_equal(round(k$avg), 69)
  expect_equal(k$stdev, 5.25)

})


test_that('binom_plot throws the appropriate errors', {
  
  expect_error(binom_plot(10, -0.5), 'p must be between 0 and 1')
  expect_error(binom_plot(10, 1.5), 'p must be between 0 and 1')
  expect_error(binom_plot('10', 0.5), 'n must be numeric/integer')
  expect_error(binom_plot(as.factor(10), 0.5), 'n must be numeric/integer')
  expect_error(binom_plot(10, '0.5'), 'p must be numeric')
  expect_error(binom_plot(10, as.factor(0.5)), 'p must be numeric')
  
})


test_that('output from binom_prob matches expected result', {
    
    k <- binom_prob(6, 0.5, 3)
    expect_equal(round(k$avg), 3)
    expect_equal(k$stdev, 1.22)
    
    k <- binom_prob(6, 0.5, 3, type = 'exact')
    expect_equal(round(k$prob, 2), 0.31)
    
    k <- binom_prob(6, 0.5, 3, type = 'lower')
    expect_equal(round(k$prob, 2), 0.66)
    
    k <- binom_prob(6, 0.5, 3, type = 'upper')
    expect_equal(round(k$prob, 2), 0.66)
    
    k <- binom_prob(6, 0.5, c(2, 5), type = 'interval')
    expect_equal(round(k$prob, 2), 0.88)
    
    k <- binom_prob(10, 0.3, 4)
    expect_equal(round(k$avg), 3)
    expect_equal(k$stdev, 1.45)
    
    k <- binom_prob(10, 0.3, 4, type = 'exact')
    expect_equal(round(k$prob, 2), 0.20)
    
    k <- binom_prob(10, 0.3, 4, type = 'lower')
    expect_equal(round(k$prob, 2), 0.85)
    
    k <- binom_prob(10, 0.3, 4, type = 'upper')
    expect_equal(round(k$prob, 2), 0.35)
    
    k <- binom_prob(10, 0.3, c(4, 6), type = 'interval')
    expect_equal(round(k$prob, 2), 0.34)
    
    k <- binom_prob(20, 0.7, 6)
    expect_equal(round(k$avg), 14)
    expect_equal(k$stdev, 2.05)
    
    k <- binom_prob(20, 0.7, 6, type = 'exact')
    expect_equal(round(k$prob, 2), 0)
    
    k <- binom_prob(20, 0.7, 6, type = 'lower')
    expect_equal(round(k$prob, 2), 0)
    
    k <- binom_prob(20, 0.7, 6, type = 'upper')
    expect_equal(round(k$prob, 2), 1)
    
    k <- binom_prob(20, 0.7, c(2, 7), type = 'interval')
    expect_equal(round(k$prob, 2), 0)
})


test_that('binom_prob throws the appropriate errors', {
    
    expect_error(binom_prob(10, -0.5, 4), 'p must be between 0 and 1')
    expect_error(binom_prob(10, 1.5, 4), 'p must be between 0 and 1')
    expect_error(binom_prob(10, '0.5', 4), 'p must be numeric')
    expect_error(binom_prob('10', 0.5, 4), 'n must be numeric/integer')
    expect_error(binom_prob(as.factor(10), 0.5, 4), 'n must be numeric/integer')
    expect_error(binom_prob(10, 0.5, '4'), 's must be numeric/integer')
    expect_error(binom_prob(10, 0.5, as.factor(4)), 's must be numeric/integer')
    
})

test_that('binom_perc throws the appropriate errors', {
    
    expect_error(binom_perc(10, -0.5, 0.05), 'p must be between 0 and 1')
    expect_error(binom_perc(10, 1.5, 0.05), 'p must be between 0 and 1')
    expect_error(binom_perc(10, 0.5, -0.05), 'tp must be between 0 and 0.5')
    expect_error(binom_perc(10, 0.5, 0.51), 'tp must be between 0 and 0.5')
    expect_error(binom_perc('10', 0.5, 0.05), 'n must be numeric/integer')
    expect_error(binom_perc(as.factor(10), 0.5, 0.05), 'n must be numeric/integer')
    expect_error(binom_perc(10, '0.5', 0.05), 'p must be numeric')
    expect_error(binom_perc(10, as.factor(0.5), 0.05), 'p must be numeric')
    expect_error(binom_perc(10, 0.5, '0.05'), 'tp must be numeric')
    expect_error(binom_perc(10, 0.5, as.factor(0.05)), 'tp must be numeric')
    
})