context('describe')

test_that('output from tailobs match expected result', {

  expect_equivalent(tailobs(mtcars$mpg, 5, 'low'), c(10.4, 10.4, 13.3, 14.3, 14.7))
  expect_equivalent(tailobs(mtcars$mpg, 5, 'high'), c(33.9, 32.4, 30.4, 30.4, 27.3))

})


test_that('tailobs returns the appropriate error', {

  expect_error(tailobs(mtcars$mpg, 40, 'low'), 'n must be less than the length of data')
  expect_error(tailobs(mtcars$mpg, 40, 'high'), 'n must be less than the length of data')
  expect_error(tailobs('mtcars', 40, 'high'), 'data must be numeric')
  expect_error(tailobs(as.factor(mtcars$disp), 40, 'high'), 'data must be numeric')
  expect_error(tailobs(mtcars$mpg, '40', 'high'), 'n must be numeric')

})


test_that('output from rindex match expected result', {

  expect_equivalent(rindex(mtcars$mpg, c(13.3)), 24)
  expect_equivalent(rindex(mtcars$mpg, c(33.9)), 20)
  expect_equivalent(rindex(mtcars$mpg, c(13.3, 10.4)), c(24, 15, 16))
  expect_null(rindex(mtcars$mpg, c(34)))

})


test_that('rindex returns the appropriate error', {

  expect_error(rindex('mtcars', c(13.3)), 'data must be numeric')
  expect_error(rindex(as.factor(mtcars$disp), c(13.3)), 'data must be numeric')
  expect_error(rindex(mtcars$mpg, '13.3'), 'values must be numeric')

})


test_that('output from skewness matches expected result', {

  expect_equal(round(skewness(mtcars$mpg), 3), 0.672)
  expect_equal(round(skewness(mtcars$disp), 3), 0.420)
  expect_equal(round(skewness(mtcars$hp), 3), 0.799)
  expect_equal(round(skewness(mtcars$drat), 3), 0.293)
  expect_equal(round(skewness(mtcars$wt), 3), 0.466)
  expect_equal(round(skewness(mtcars$qsec), 3), 0.406)

})


test_that('skewness returns the appropriate error', {

  expect_error(round(skewness('mtcars$mpg'), 3), 'x must be numeric')
  expect_error(round(skewness(as.factor(mtcars$mpg)), 3), 'x must be numeric')

})


test_that('output from kurtosis matches expected result', {

  expect_equal(round(kurtosis(mtcars$mpg), 3), -0.022)
  expect_equal(round(kurtosis(mtcars$disp), 3), -1.068)
  expect_equal(round(kurtosis(mtcars$hp), 3), 0.275)
  expect_equal(round(kurtosis(mtcars$drat), 3), -0.450)
  expect_equal(round(kurtosis(mtcars$wt), 3), 0.417)
  expect_equal(round(kurtosis(mtcars$qsec), 3), 0.865)

})


test_that('kurtosis returns the appropriate error', {

  expect_error(round(kurtosis('mtcars$mpg'), 3), 'x must be numeric')
  expect_error(round(kurtosis(as.factor(mtcars$mpg)), 3), 'x must be numeric')

})


test_that('stat_css matches `Sum Sq` from anova', {

  expect_equal(round(stat_css(mtcars$mpg)), round(anova(lm(mpg ~ 1, data = mtcars))[[2]]))
  expect_equal(round(stat_css(mtcars$disp)), round(anova(lm(disp ~ 1, data = mtcars))[[2]]))
  expect_equal(round(stat_css(mtcars$hp)), round(anova(lm(hp ~ 1, data = mtcars))[[2]]))
  expect_equal(round(stat_css(mtcars$drat)), round(anova(lm(drat ~ 1, data = mtcars))[[2]]))
  expect_equal(round(stat_css(mtcars$wt)), round(anova(lm(wt ~ 1, data = mtcars))[[2]]))
  expect_equal(round(stat_css(mtcars$qsec)), round(anova(lm(qsec ~ 1, data = mtcars))[[2]]))

})


test_that('stat_css returns the appropriate error', {

  expect_error(round(stat_css('mtcars$mpg')), 'x must be numeric')
  expect_error(round(stat_css(as.factor(mtcars$mpg))), 'x must be numeric')

})


test_that('output from stat_cvar matches the expected result', {

  expect_equal(round(stat_cvar(mtcars$mpg), 3), 29.999)
  expect_equal(round(stat_cvar(mtcars$disp), 3), 53.718)

})


test_that('stat_cvar returns the appropriate error', {

  expect_error(stat_cvar('mtcars$mpg'), 'x must be numeric')
  expect_error(stat_cvar(as.factor(mtcars$mpg)), 'x must be numeric')

})


test_that('output from stat_mode matches the expected result', {

  expect_equal(stat_mode(mtcars$mpg), 10.4)
  expect_equal(stat_mode(mtcars$disp), 275.8)
  expect_equal(stat_mode(mtcars$hp), 110)
  expect_equal(stat_mode(mtcars$drat), 3.07)
  expect_equal(stat_mode(mtcars$wt), 3.440)
  expect_equal(stat_mode(mtcars$qsec), 17.02)

})


test_that('stat_mode returns the appropriate error', {

  expect_error(stat_mode('mtcars$mpg'), 'x must be numeric')
  expect_error(stat_mode(as.factor(mtcars$mpg)), 'x must be numeric')

})


test_that('output from stat_range matches the expected result', {

  expect_equal(stat_range(mtcars$mpg), 23.5)
  expect_equal(stat_range(mtcars$disp), 400.9)
  expect_equal(stat_range(mtcars$hp), 283)
  expect_equal(stat_range(mtcars$drat), 2.17)
  expect_equal(stat_range(mtcars$wt), 3.911)
  expect_equal(stat_range(mtcars$qsec), 8.40)

})


test_that('stat_range returns the appropriate error', {

  expect_error(stat_range('mtcars$mpg'), 'data must be numeric')
  expect_error(stat_range(as.factor(mtcars$mpg)), 'data must be numeric')

})


test_that('output from stat_mdev matches the expected result', {

  expect_equal(round(stat_mdev(mtcars$mpg), 3), 4.714)
  expect_equal(round(stat_mdev(mtcars$disp), 3), 108.786)
  expect_equal(round(stat_mdev(mtcars$hp), 3), 56.48)
  expect_equal(round(stat_mdev(mtcars$drat), 3), 0.453)
  expect_equal(round(stat_mdev(mtcars$wt), 3), 0.73)
  expect_equal(round(stat_mdev(mtcars$qsec), 3), 1.376)

})


test_that('output from stat_mdev matches the expected result', {
    expect_equal(stat_mdev(c(92, 83, 88, 94, 91, 85, 89, 90)), 2.75)
    expect_equal(stat_mdev(c(3, 6, 6, 7, 8, 11, 15, 16)), 3.75)
})

test_that('stat_mdev returns the appropriate error', {
  expect_error(stat_mdev('mtcars$mpg'), 'x must be numeric')
  expect_error(stat_mdev(as.factor(mtcars$mpg)), 'x must be numeric')
})


test_that('output from hmean matches the expected output', {
    expect_equal(round(hmean(mtcars$mpg), 2), 18.44)
    expect_equal(round(hmean(mtcars$disp), 2), 166.8)
    expect_equal(round(hmean(mtcars$hp), 2), 118.23)
})

test_that('hmean throws the appropriate error', {
    expect_error(hmean('mtcars$mpg'), 'x must be numeric')
    expect_error(hmean(as.factor(mtcars$mpg)), 'x must be numeric')
})


test_that('output from gmean matches the expected output', {
    expect_equal(round(gmean(mtcars$mpg), 2), 19.25)
    expect_equal(round(gmean(mtcars$disp), 2), 197.32)
    expect_equal(round(gmean(mtcars$hp), 2), 131.88)
})


test_that('gmean throws the appropriate error', {
    expect_error(gmean('mtcars$mpg'), 'x must be numeric')
    expect_error(gmean(as.factor(mtcars$mpg)), 'x must be numeric')
})
