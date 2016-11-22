context('t-dist')

test_that('output from t_per matches expected results', {
    
    k <- t_per()
    expect_equal(k$x, 2.132)
    
    k <- t_per(0.35, 4, 'upper')
    expect_equal(k$x, 0.414)
    
    k <- t_per(0.73, 4, 'both')
    expect_equal(k$x, c(-1.279, 1.279))
    
    k <- t_per(0.87, 7, 'lower')
    expect_equal(k$x, 1.226)
    
    k <- t_per(0.385, 7, 'upper')
    expect_equal(k$x, 0.304)
    
    k <- t_per(0.69, 7, 'both')
    expect_equal(k$x, c(-1.094, 1.094))
    
})


test_that('t_per returns appropriate error messages', {
    
    expect_error(t_per('0.95', 4, 'lower'), 'probs must be numeric')
    
    expect_error(t_per(as.factor(1), 4, 'lower'), 'probs must be numeric')
    
    expect_error(t_per(0.95, '4', 'lower'), 'df must be numeric/integer')
    
    expect_error(t_per(0.95, as.factor(4), 'lower'), 
                 'df must be numeric/integer')
    
    expect_error(t_per(-0.95, 4, 'lower'), 'probs must be between 0 and 1')
    
    expect_error(t_per(1.95, 4, 'lower'), 'probs must be between 0 and 1')
    
})


test_that('output from t_prob matches expected result', {
    
    # lower tail
    k <- t_prob(2.045, 7, 'lower')
    expect_equal(k$probs, 0.96)
    
    k <- t_prob(1.23, 4, 'lower')
    expect_equal(k$probs, 0.857)
    
    # upper tail
    k <- t_prob(0.945, 7, 'upper')
    expect_equal(k$probs, 0.188)
    
    k <- t_prob(1.82, 4, 'upper')
    expect_equal(k$probs, 0.071)
    
    # interval
    k <- t_prob(1.445, 7, 'interval')
    expect_equal(k$probs, c(0.096, 0.096))
    
    k <- t_prob(1.48, 4, 'interval')
    expect_equal(k$probs, c(0.106, 0.106))
    
    # two tail
    k <- t_prob(1.6, 7, 'both')
    expect_equal(k$probs, c(0.077, 0.077))
    
    k <- t_prob(1.73, 4, 'both')
    expect_equal(k$probs, c(0.079, 0.079))
})


test_that('t_prob returns appropriate error messages', {
    
    expect_error(t_prob('1.23', 4, 'lower'), 'perc must be numeric/integer')
    
    expect_error(t_prob(as.factor(1), 4, 'lower'), 
                 'perc must be numeric/integer')
    
    expect_error(t_prob(1.23, '4', 'lower'), 'df must be numeric/integer')
    
    expect_error(t_prob(1.23, as.factor(4), 'lower'), 
                 'df must be numeric/integer')
    
})