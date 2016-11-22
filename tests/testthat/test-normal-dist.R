context('normal-dist')

test_that('output from norm_per matches expected output', {
    
    k <- norm_per()
    expect_equal(k$x, 1.645)
    
    k <- norm_per(0.05, type = 'upper')
    expect_equal(k$x, 1.645)
    
    k <- norm_per(0.95, type = 'both')
    expect_equal(k$x, c(-1.96, 1.96))
    
    k <- norm_per(0.95, mean = 2, sd = 1.36)
    expect_equal(k$x, 4.237)
    
    k <- norm_per(0.3, mean = 2, sd = 1.36, type = 'upper')
    expect_equal(k$x, 2.713)
    
    k <- norm_per(0.95, mean = 2, sd = 1.36, type = 'both')
    expect_equal(k$x, c(-0.666, 4.666))
    
})


test_that('norm_per throws the appropriate errors', {
    
    expect_error(norm_per(0.95, '2', 1), 'mean must be numeric/integer')
    expect_error(norm_per(0.95, as.factor(2), 1), 'mean must be numeric/integer')
    expect_error(norm_per(0.95, 2, '1'), 'sd must be numeric/integer')
    expect_error(norm_per(0.95, 2, as.factor(1)), 'sd must be numeric/integer')
    expect_error(norm_per(0.95, 2, -1), 'sd must be positive')
    expect_error(norm_per('0.95', 2, 1), 'probs must be numeric')
    expect_error(norm_per(as.factor(0.95), 2, 1), 'probs must be numeric')
    expect_error(norm_per(1.95, 2, 1), 'probs must be between 0 and 1')
    expect_error(norm_per(-1.95, 2, 1), 'probs must be between 0 and 1')

})


test_that('output from norm_prob matches expected output', {
    
    k <- norm_prob(1.64)
    expect_equal(k$prob, 0.949)
    
    k <- norm_prob(1.64, type = 'upper')
    expect_equal(k$prob, 0.051)
    
    k <- norm_prob(c(-1.96, 1.96), type = 'both')
    expect_equal(k$prob, c(0.025, 0.025))
    
    k <- norm_prob(3.78, mean = 2, sd = 1.36)
    expect_equal(k$prob, 0.905)
    
    k <- norm_prob(3.43, mean = 2, sd = 1.36, type = 'upper')
    expect_equal(k$prob, 0.147)
    
    k <- norm_prob(c(-1.74, 1.83), type = 'both')
    expect_equal(k$prob, c(0.041, 0.034))
    
})


test_that('norm_prob throws the appropriate errors', {
    
    expect_error(norm_prob(1.64, '0', 1), 'mean must be numeric/integer')
    expect_error(norm_prob(1.64, as.factor(0), 1), 'mean must be numeric/integer')
    expect_error(norm_prob(1.64, 0, '1'), 'sd must be numeric/integer')
    expect_error(norm_prob(1.64, 0, as.factor(1)), 'sd must be numeric/integer')    
    expect_error(norm_prob(1.64, 0, -1), 'sd must be positive')
    expect_error(norm_prob('1.64', 0, 1), 'perc must be numeric/integer')
    expect_error(norm_prob(as.factor(1.64), 0, 1), 'perc must be numeric/integer')
    expect_error(norm_prob(c(1.64, 1.63, 1.62), 0, 1), 
                 'Please do not specify more than 2 percentile values')
    expect_error(norm_prob(1.64, 0, 1, type = 'both'), 
                 'Specify two percentile values')
    
})


