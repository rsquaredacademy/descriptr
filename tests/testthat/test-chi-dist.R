context('chi-dist')

test_that('chi_plot returns appropriate error messages', {
    
    expect_error(chi_plot('3'), 'df must be numeric/integer')
    
    expect_error(chi_plot(as.factor(3)), 'df must be numeric/integer')
    
    expect_error(chi_plot(normal = 3), 'normal must be logical')
    
    expect_error(chi_plot(normal = '3'), 'normal must be logical')
    
})


test_that('output from chi_per matches expected results', {
    
    # lower tail
    k <- chi_per(0.93, 8, 'lower')
    expect_equal(k$x, 14.484)
    
    k <- chi_per(0.885, 13, 'lower')
    expect_equal(k$x, 19.269)
    
    # upper tail
    k <- chi_per(0.165, 8, 'upper')
    expect_equal(k$x, 11.702)
    
    k <- chi_per(0.22, 13, 'upper')
    expect_equal(k$x, 16.564)
    
})


test_that('chi_per returns appropriate error messages', {
    
    expect_error(chi_per('0.95'), 'probs must be numeric')
    
    expect_error(chi_per(as.factor(1)), 'probs must be numeric')
    
    expect_error(chi_per(df = '3'), 'df must be numeric/integer')
    
    expect_error(chi_per(df = as.factor(3)), 'df must be numeric/integer')
    
})


test_that('output from chi_prob matches expected result', {
    
    # lower tail
    k <- chi_prob(18.36, 11, 'lower')
    expect_equal(k$prob, 0.926)
    
    k <- chi_prob(16.988, 13, 'lower')
    expect_equal(k$prob, 0.8)
    
    # upper tail
    k <- chi_prob(13.58, 11, 'upper')
    expect_equal(k$prob, 0.257)
    
    k <- chi_prob(15.72, 13, 'upper')
    expect_equal(k$prob, 0.265)
    
})

test_that('chi_prob returns appropriate error messages', {
    
    expect_error(chi_prob('18.36', 11), 'perc must be numeric/integer')
    
    expect_error(chi_prob(as.factor(18), 11), 'perc must be numeric/integer')
    
    expect_error(chi_prob(18.36, '11'), 'df must be numeric/integer')
    
    expect_error(chi_prob(18.36, as.factor(11)), 'df must be numeric/integer')
    
})