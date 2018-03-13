
<!-- README.md is generated from README.Rmd. Please edit that file -->

## descriptr: Generate descriptive statistics <img src="hex_descriptr.png" align="right" />

**Author:** [Aravind Hebbali](http://www.aravindhebbali.com)<br/>
**License:**
[MIT](https://opensource.org/licenses/MIT)

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/descriptr)](https://cran.r-project.org/package=descriptr)
[![Travis-CI Build
Status](https://travis-ci.org/rsquaredacademy/descriptr.svg?branch=master)](https://travis-ci.org/rsquaredacademy/descriptr)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/rsquaredacademy/descriptr?branch=master&svg=true)](https://ci.appveyor.com/project/rsquaredacademy/descriptr)
[![](https://cranlogs.r-pkg.org/badges/grand-total/descriptr)](https://cran.r-project.org/package=descriptr)
[![Coverage
status](https://codecov.io/gh/rsquaredacademy/descriptr/branch/master/graph/badge.svg)](https://codecov.io/github/rsquaredacademy/descriptr?branch=master)
![](https://img.shields.io/badge/lifecycle-maturing-blue.svg)

## Overview

The goal of descriptr is to ease the process of generating descriptive
statistics and exploring statistical distributions.

## Installation

``` r
# install descriptr from CRAN
install.packages("descriptr")

# the development version from github
# install.packages("devtools")
devtools::install_github("rsquaredacademy/descriptr")
```

## Features

### Descriptive Statistics

  - Summary statistics
  - Two way tables
  - One way table
  - One way table (Continuous Data)
  - Group wise summary
  - Multiple variable statistics
  - Multiple one way tables
  - Multiple two way tables

### Explore Distributions

  - Normal
  - Binomial
  - Chi Square
  - F
  - t

## Shiny App

Use `ds_launch_shiny_app()` to explore the package using a shiny app.

## Vignettes

  - [Descriptive
    Statistics](http://www.rsquaredacademy.com/descriptr/articles/descriptive-stats.html)
  - [Statistical
    Distributions](http://www.rsquaredacademy.com/descriptr/articles/distributions.html)

## Usage

We will use a modified version of the `mtcars` data set in the below
examples. The only difference between the data sets is related to the
variable types.

``` r
str(mtcarz)
#> 'data.frame':    32 obs. of  11 variables:
#>  $ mpg : num  21 21 22.8 21.4 18.7 18.1 14.3 24.4 22.8 19.2 ...
#>  $ cyl : Factor w/ 3 levels "4","6","8": 2 2 1 2 3 2 3 1 1 2 ...
#>  $ disp: num  160 160 108 258 360 ...
#>  $ hp  : num  110 110 93 110 175 105 245 62 95 123 ...
#>  $ drat: num  3.9 3.9 3.85 3.08 3.15 2.76 3.21 3.69 3.92 3.92 ...
#>  $ wt  : num  2.62 2.88 2.32 3.21 3.44 ...
#>  $ qsec: num  16.5 17 18.6 19.4 17 ...
#>  $ vs  : Factor w/ 2 levels "0","1": 1 1 2 2 1 2 1 2 2 2 ...
#>  $ am  : Factor w/ 2 levels "0","1": 2 2 2 1 1 1 1 1 1 1 ...
#>  $ gear: Factor w/ 3 levels "3","4","5": 2 2 2 1 1 1 1 2 2 2 ...
#>  $ carb: Factor w/ 6 levels "1","2","3","4",..: 4 4 1 1 2 1 4 2 2 4 ...
```

##### Summary Statistics

``` r
ds_summary_stats(mtcarz, mpg)
#>                         Univariate Analysis                          
#> 
#>  N                       32.00      Variance                36.32 
#>  Missing                  0.00      Std Deviation            6.03 
#>  Mean                    20.09      Range                   23.50 
#>  Median                  19.20      Interquartile Range      7.38 
#>  Mode                    10.40      Uncorrected SS       14042.31 
#>  Trimmed Mean            19.95      Corrected SS          1126.05 
#>  Skewness                 0.67      Coeff Variation         30.00 
#>  Kurtosis                -0.02      Std Error Mean           1.07 
#> 
#>                               Quantiles                               
#> 
#>               Quantile                            Value                
#> 
#>              Max                                  33.90                
#>              99%                                  33.44                
#>              95%                                  31.30                
#>              90%                                  30.09                
#>              Q3                                   22.80                
#>              Median                               19.20                
#>              Q1                                   15.43                
#>              10%                                  14.34                
#>              5%                                   12.00                
#>              1%                                   10.40                
#>              Min                                  10.40                
#> 
#>                             Extreme Values                            
#> 
#>                 Low                                High                
#> 
#>   Obs                        Value       Obs                        Value 
#>   15                         10.4        20                         33.9  
#>   16                         10.4        18                         32.4  
#>   24                         13.3        19                         30.4  
#>    7                         14.3        28                         30.4  
#>   17                         14.7        26                         27.3
```

##### Two Way Table

``` r
ds_cross_table(mtcarz, cyl, gear)
#>     Cell Contents
#>  |---------------|
#>  |     Frequency |
#>  |       Percent |
#>  |       Row Pct |
#>  |       Col Pct |
#>  |---------------|
#> 
#>  Total Observations:  32 
#> 
#> ----------------------------------------------------------------------------
#> |              |                           gear                            |
#> ----------------------------------------------------------------------------
#> |          cyl |            3 |            4 |            5 |    Row Total |
#> ----------------------------------------------------------------------------
#> |            4 |            1 |            8 |            2 |           11 |
#> |              |        0.031 |         0.25 |        0.062 |              |
#> |              |         0.09 |         0.73 |         0.18 |         0.34 |
#> |              |         0.07 |         0.67 |          0.4 |              |
#> ----------------------------------------------------------------------------
#> |            6 |            2 |            4 |            1 |            7 |
#> |              |        0.062 |        0.125 |        0.031 |              |
#> |              |         0.29 |         0.57 |         0.14 |         0.22 |
#> |              |         0.13 |         0.33 |          0.2 |              |
#> ----------------------------------------------------------------------------
#> |            8 |           12 |            0 |            2 |           14 |
#> |              |        0.375 |            0 |        0.062 |              |
#> |              |         0.86 |            0 |         0.14 |         0.44 |
#> |              |          0.8 |            0 |          0.4 |              |
#> ----------------------------------------------------------------------------
#> | Column Total |           15 |           12 |            5 |           32 |
#> |              |        0.468 |        0.375 |        0.155 |              |
#> ----------------------------------------------------------------------------
```

##### One Way Table

``` r
ds_freq_table(mtcarz, cyl)
#>                                Variable: cyl                                 
#> |--------------------------------------------------------------------------|
#> |                                Cumulative                    Cumulative  |
#> |    Levels    |  Frequency   |   Frequency  |   Percent    |    Percent   |
#> |--------------------------------------------------------------------------|
#> |       4      |      11      |      11      |     34.38    |     34.38    |
#> |--------------------------------------------------------------------------|
#> |       6      |       7      |      18      |     21.88    |     56.25    |
#> |--------------------------------------------------------------------------|
#> |       8      |      14      |      32      |     43.75    |      100     |
#> |--------------------------------------------------------------------------|
#> |     Total    |      32      |       -      |    100.00    |       -      |
#> |--------------------------------------------------------------------------|
```

##### One Way Table (Continuous Data)

``` r
ds_freq_cont(mtcarz, mpg)
#>                               Variable: mpg                               
#> |-----------------------------------------------------------------------|
#> |    Bins     | Frequency | Cum Frequency |   Percent    | Cum Percent  |
#> |-----------------------------------------------------------------------|
#> | 10.4 - 15.1 |     6     |       6       |    18.75     |    18.75     |
#> |-----------------------------------------------------------------------|
#> | 15.1 - 19.8 |    12     |      18       |     37.5     |    56.25     |
#> |-----------------------------------------------------------------------|
#> | 19.8 - 24.5 |     8     |      26       |      25      |    81.25     |
#> |-----------------------------------------------------------------------|
#> | 24.5 - 29.2 |     2     |      28       |     6.25     |     87.5     |
#> |-----------------------------------------------------------------------|
#> | 29.2 - 33.9 |     4     |      32       |     12.5     |     100      |
#> |-----------------------------------------------------------------------|
#> |    Total    |    32     |       -       |    100.00    |      -       |
#> |-----------------------------------------------------------------------|
```

##### Group Summary

``` r
ds_group_summary(mtcarz, cyl, mpg)
#>                                        mpg by cyl                                         
#> -----------------------------------------------------------------------------------------
#> |     Statistic/Levels|                    4|                    6|                    8|
#> -----------------------------------------------------------------------------------------
#> |                  Obs|                   11|                    7|                   14|
#> |              Minimum|                 21.4|                 17.8|                 10.4|
#> |              Maximum|                 33.9|                 21.4|                 19.2|
#> |                 Mean|                26.66|                19.74|                 15.1|
#> |               Median|                   26|                 19.7|                 15.2|
#> |                 Mode|                 22.8|                   21|                 10.4|
#> |       Std. Deviation|                 4.51|                 1.45|                 2.56|
#> |             Variance|                20.34|                 2.11|                 6.55|
#> |             Skewness|                 0.35|                -0.26|                -0.46|
#> |             Kurtosis|                -1.43|                -1.83|                 0.33|
#> |       Uncorrected SS|              8023.83|              2741.14|              3277.34|
#> |         Corrected SS|               203.39|                12.68|                 85.2|
#> |      Coeff Variation|                16.91|                 7.36|                16.95|
#> |      Std. Error Mean|                 1.36|                 0.55|                 0.68|
#> |                Range|                 12.5|                  3.6|                  8.8|
#> |  Interquartile Range|                  7.6|                 2.35|                 1.85|
#> -----------------------------------------------------------------------------------------
```

##### Multiple Variable Statistics

``` r
ds_multi_stats(mtcarz, mpg, disp, hp)
#> # A tibble: 3 x 16
#>   vars    min   max  mean t_mean median  mode range variance  stdev  skew
#>   <chr> <dbl> <dbl> <dbl>  <dbl>  <dbl> <dbl> <dbl>    <dbl>  <dbl> <dbl>
#> 1 disp   71.1 472   231    228    196   276   401    15361   124    0.420
#> 2 hp     52.0 335   147    144    123   110   283     4701    68.6  0.799
#> 3 mpg    10.4  33.9  20.1   20.0   19.2  10.4  23.5     36.3   6.03 0.672
#> # ... with 5 more variables: kurtosis <dbl>, coeff_var <dbl>, q1 <dbl>,
#> #   q3 <dbl>, iqrange <dbl>
```

Please note that this project is released with a [Contributor Code of
Conduct](CONDUCT.md). By participating in this project you agree to
abide by its terms.
