context("test-summary-stats")

test_that("output from ds_summary_stats is as expected", {
  
  actual <- round(ds_summary_stats(mtcarz, mpg)$variance, 2)
	expected <- 36.32
	expect_equal(actual, expected)

})

test_that("output from ds_summary_stats is as expected", {
  
  actual <- ds_summary_stats(mtcarz, mpg)
	expected <- "                        Univariate Analysis                          

 N                       32.00      Variance                36.32 
 Missing                  0.00      Std Deviation            6.03 
 Mean                    20.09      Range                   23.50 
 Median                  19.20      Interquartile Range      7.38 
 Mode                    10.40      Uncorrected SS       14042.31 
 Trimmed Mean            19.95      Corrected SS          1126.05 
 Skewness                 0.67      Coeff Variation         30.00 
 Kurtosis                -0.02      Std Error Mean           1.07 

                              Quantiles                               

              Quantile                            Value                

             Max                                  33.90                
             99%                                  33.44                
             95%                                  31.30                
             90%                                  30.09                
             Q3                                   22.80                
             Median                               19.20                
             Q1                                   15.43                
             10%                                  14.34                
             5%                                   12.00                
             1%                                   10.40                
             Min                                  10.40                

                            Extreme Values                            

                Low                                High                

  Obs                        Value       Obs                        Value 
  15                         10.4        20                         33.9  
  16                         10.4        18                         32.4  
  24                         13.3        19                         30.4  
   7                         14.3        28                         30.4  
  17                         14.7        26                         27.3"
	
	expect_output(print(actual), expected)
	
})

test_that("ds_multi_summary_stats throws appropriate errors", {
  fdata <- dplyr::select(mtcarz, cyl, gear, am, vs)
  expect_error(ds_multi_summary_stats(fdata), 'Data has no continuous variables.')
  expect_error(ds_multi_summary_stats(mtcarz, cyl, gear), 'Data has no continuous variables.')
})