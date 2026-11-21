
<!-- README.md is generated from README.Rmd. Please edit that file -->

# descriptr

> Generate descriptive statistics

<!-- badges: start -->

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/descriptr)](https://cran.r-project.org/package=descriptr)
[![R-CMD-check](https://github.com/rsquaredacademy/descriptr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rsquaredacademy/descriptr/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/rsquaredacademy/descriptr/graph/badge.svg)](https://app.codecov.io/gh/rsquaredacademy/descriptr)
<!-- badges: end -->

## Installation

``` r
# Install release version from CRAN
install.packages("descriptr")

# Install development version from GitHub
# install.packages("devtools")
devtools::install_github("rsquaredacademy/descriptr")

# Install the development version from `rsquaredacademy` universe
install.packages("descriptr", repos = "https://rsquaredacademy.r-universe.dev")
```

## Articles

- [Continuous
  Data](https://descriptr.rsquaredacademy.com/articles/continuous-data.html)
- [Categorical
  Data](https://descriptr.rsquaredacademy.com/articles/categorical-data.html)
- [Visualization](https://descriptr.rsquaredacademy.com/articles/visualization.html)

## Usage

We will use the `wholesale_customers` data set in the below examples. It
contains annual spending by 440 wholesale customers on six product
categories, together with the sales channel and region of each customer.
The data come from the UCI Machine Learning Repository (Cardoso 2013, CC
BY 4.0).

``` r
str(wholesale_customers)
#> 'data.frame':    440 obs. of  8 variables:
#>  $ channel         : Factor w/ 2 levels "hotel_restaurant_cafe",..: 2 2 2 1 2 2 2 2 1 2 ...
#>  $ region          : Factor w/ 3 levels "lisbon","porto",..: 3 3 3 3 3 3 3 3 3 3 ...
#>  $ fresh           : int  12669 7057 6353 13265 22615 9413 12126 7579 5963 6006 ...
#>  $ milk            : int  9656 9810 8808 1196 5410 8259 3199 4956 3648 11093 ...
#>  $ grocery         : int  7561 9568 7684 4221 7198 5126 6975 9426 6192 18881 ...
#>  $ frozen          : int  214 1762 2405 6404 3915 666 480 1669 425 1159 ...
#>  $ detergents_paper: int  2674 3293 3516 507 1777 1795 3140 3321 1716 7425 ...
#>  $ delicassen      : int  1338 1776 7844 1788 5185 1451 545 2566 750 2098 ...
```

### Continuous Data

#### Summary Statistics

Annual spending on fresh products averages 12000.30, with a median of
8504 and a standard deviation of 12647.33. The distribution is
right-skewed.

``` r
ds_summary_stats(wholesale_customers, fresh)
#> ------------------------------- Variable: fresh -------------------------------
#> 
#>                                Univariate Analysis                                 
#> 
#>  N                             440.00      Variance                159954927.42 
#>  Missing                         0.00      Std Deviation               12647.33 
#>  Mean                        12000.30      Range                      112148.00 
#>  Median                       8504.00      Interquartile Range         13806.00 
#>  Mode                            3.00      Uncorrected SS       133583357177.00 
#>  Trimmed Mean                10522.73      Corrected SS          70220213138.00 
#>  Skewness                        2.56      Coeff Variation               105.39 
#>  Kurtosis                       11.54      Std Error Mean                602.94 
#> 
#>                                      Quantiles                                      
#> 
#>                  Quantile                                    Value                   
#> 
#>                 Max                                        112151.00                 
#>                 99%                                        56082.61                  
#>                 95%                                        36818.50                  
#>                 90%                                        27090.50                  
#>                 Q3                                         16933.75                  
#>                 Median                                      8504.00                  
#>                 Q1                                          3127.75                  
#>                 10%                                         915.60                   
#>                 5%                                          401.90                   
#>                 1%                                           28.46                   
#>                 Min                                          3.00                    
#> 
#>                                    Extreme Values                                   
#> 
#>                     Low                                      High                    
#> 
#>   Obs                              Value        Obs                              Value  
#>    96                                3          182                              112151 
#>   339                                3          126                              76237  
#>    67                                9          285                              68951  
#>   219                                18          40                              56159  
#>    97                                23         259                              56083
```

#### Frequency Distribution

With four intervals, 400 of the 440 customers (90.91%) spend less than
28040 on fresh products.

``` r
ds_freq_table(wholesale_customers, fresh, 4)
#>                                Variable: fresh                                
#> |---------------------------------------------------------------------------|
#> |      Bins       | Frequency | Cum Frequency |   Percent    | Cum Percent  |
#> |---------------------------------------------------------------------------|
#> |   3    - 28040  |    400    |      400      |    90.91     |    90.91     |
#> |---------------------------------------------------------------------------|
#> | 28040  - 56077  |    34     |      434      |     7.73     |    98.64     |
#> |---------------------------------------------------------------------------|
#> | 56077  - 84114  |     5     |      439      |     1.14     |    99.77     |
#> |---------------------------------------------------------------------------|
#> | 84114  - 112151 |     1     |      440      |     0.23     |     100      |
#> |---------------------------------------------------------------------------|
#> |      Total      |    440    |       -       |    100.00    |      -       |
#> |---------------------------------------------------------------------------|
```

### Categorical Data

#### One Way Table

Most customers are in the hotel/restaurant/cafe channel: 298 (67.73%)
against 142 (32.27%) in retail.

``` r
ds_freq_table(wholesale_customers, channel)
#>                                   Variable: channel                                   
#> -------------------------------------------------------------------------------------
#>        Levels            Frequency    Cum Frequency       Percent        Cum Percent  
#> -------------------------------------------------------------------------------------
#> hotel_restaurant_cafe       298            298             67.73            67.73    
#> -------------------------------------------------------------------------------------
#>        retail               142            440             32.27             100     
#> -------------------------------------------------------------------------------------
#>         Total               440             -             100.00              -      
#> -------------------------------------------------------------------------------------
```

#### Two Way Table

Customers in both channels are concentrated in the other region: 70.81%
of hotel/restaurant/cafe customers and 73.94% of retail customers.

``` r
ds_cross_table(wholesale_customers, channel, region)
#>     Cell Contents
#>  |---------------|
#>  |     Frequency |
#>  |       Percent |
#>  |       Row Pct |
#>  |       Col Pct |
#>  |---------------|
#> 
#>  Total Observations:  440 
#> 
#> ----------------------------------------------------------------------------
#> |              |                          region                           |
#> ----------------------------------------------------------------------------
#> |      channel |       lisbon |        porto |        other |    Row Total |
#> ----------------------------------------------------------------------------
#> |hotel_restaurant_cafe |           59 |           28 |          211 |          298 |
#> |              |        13.41 |         6.36 |        47.95 |              |
#> |              |        19.80 |         9.40 |        70.81 |        67.73 |
#> |              |        76.62 |        59.57 |        66.77 |              |
#> ----------------------------------------------------------------------------
#> |       retail |           18 |           19 |          105 |          142 |
#> |              |         4.09 |         4.32 |        23.86 |              |
#> |              |        12.68 |        13.38 |        73.94 |        32.27 |
#> |              |        23.38 |        40.43 |        33.23 |              |
#> ----------------------------------------------------------------------------
#> | Column Total |           77 |           47 |          316 |          440 |
#> |              |        17.50 |        10.68 |        71.82 |              |
#> ----------------------------------------------------------------------------
```

### Group Summary

Grouping by channel shows that hotel/restaurant/cafe customers spend
more on fresh products on average (13475.56) than retail customers
(8904.32).

``` r
ds_group_summary(wholesale_customers, channel, fresh)
#>                          fresh by channel                           
#> -------------------------------------------------------------------
#> |     Statistic/Levels|hotel_restaurant_cafe|               retail|
#> -------------------------------------------------------------------
#> |                  Obs|                  298|                  142|
#> |              Missing|                    0|                    0|
#> |              Minimum|                    3|                   18|
#> |              Maximum|               112151|                44466|
#> |                 Mean|             13475.56|              8904.32|
#> |               Median|               9581.5|               5993.5|
#> |                 Mode|                    3|                   18|
#> |       Std. Deviation|             13831.69|              8987.71|
#> |             Variance|            191315579|             80779016|
#> |             Skewness|                 2.51|                 1.59|
#> |             Kurtosis|                10.59|                 2.81|
#> |       Uncorrected SS|         110934764005|          22648593172|
#> |         Corrected SS|          56820727011|          11389841317|
#> |      Coeff Variation|               102.64|               100.94|
#> |      Std. Error Mean|               801.25|               754.23|
#> |                Range|               112148|                44448|
#> |  Interquartile Range|              14204.5|                 9882|
#> -------------------------------------------------------------------
```

#### Multiple Variable Statistics

``` r
ds_tidy_stats(wholesale_customers, fresh, milk, grocery)
#> # A tibble: 3 × 16
#>   vars      min    max   mean t_mean median  mode  range   variance  stdev  skew
#>   <chr>   <int>  <int>  <dbl>  <dbl>  <dbl> <dbl>  <int>      <dbl>  <dbl> <dbl>
#> 1 fresh       3 112151 12000. 10523.  8504      3 112148 159954927. 12647.  2.56
#> 2 grocery     3  92780  7951.  6715.  4756.   683  92777  90310104.  9503.  3.59
#> 3 milk       55  73498  5796.  4728.  3627    577  73443  54469967.  7380.  4.05
#> # ℹ 5 more variables: kurtosis <dbl>, coeff_var <dbl>, q1 <dbl>, q3 <dbl>,
#> #   iqrange <dbl>
```

## Features

- Summary statistics
- Two way tables
- One way table
- Group wise summary
- Multiple variable statistics
- Multiple one way tables
- Multiple two way tables

## Getting Help

If you encounter a bug, please file a minimal reproducible example using
[reprex](https://reprex.tidyverse.org/index.html) on github. For
questions and clarifications, use
[StackOverflow](https://stackoverflow.com/).

<div data-rsa-signup="" data-site="descriptr.rsquaredacademy.com"
data-privacy-url="https://www.rsquaredacademy.com/privacy-policy.html">

</div>
