
<!-- README.md is generated from README.Rmd. Please edit that file -->
descriptr
=========

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/descriptr)](https://cran.r-project.org/package=descriptr) [![Travis-CI Build Status](https://travis-ci.org/rsquaredacademy/descriptr.svg?branch=master)](https://travis-ci.org/rsquaredacademy/descriptr) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/rsquaredacademy/descriptr?branch=master&svg=true)](https://ci.appveyor.com/project/rsquaredacademy/descriptr) [![Coverage Status](https://img.shields.io/codecov/c/github/rsquaredacademy/descriptr/master.svg)](https://codecov.io/github/rsquaredacademy/descriptr?branch=master) [![](https://cranlogs.r-pkg.org/badges/grand-total/descriptr)](https://cran.r-project.org/package=descriptr)

Overview
--------

The goal of descriptr is to ease the process of generating descriptive statistics and exploring statistical distributions.

Installation
------------

``` r
# install descriptr from CRAN
install.packages("descriptr")

# the development version from github
# install.packages("devtools")
devtools::install_github("rsquaredacademy/descriptr")
```

Features
--------

### Descriptive Statistics

-   Summary Statistics
-   Two Way Tables
-   One Way Table
-   One Way Table (Continuous Data)
-   Group Wise Summary
-   Multiple One Way Tables
-   Multiple Two Way Tables

### Explore Distributions

-   Normal
-   Binomial
-   Chi Square
-   F
-   t

Shiny App
---------

If you want to explore the package using a shiny app, please click [here](http://rsquaredlabs.com:3838/explorer/).

Vignettes
---------

-   [Descriptive Statistics](http://www.rsquaredacademy.com/descriptr/articles/descriptive-stats.html)
-   [Statistical Distributions](http://www.rsquaredacademy.com/descriptr/articles/distributions.html)

Usage
-----

##### Summary Statistics

``` r
summary_stats(mtcars$mpg)
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
#>              95%                                  30.09                
#>              90%                                  31.30                
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
cross_table(mtcars$cyl, mtcars$gear)
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
freq_table(mtcars$cyl)
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
```

##### One Way Table (Continuous Data)

``` r
freq_cont(mtcars$mpg)
#>                                 Variable: mpg                                 
#> |---------------------------------------------------------------------------|
#> |                                 Cumulative                    Cumulative  |
#> |     Bins      |  Frequency   |   Frequency  |   Percent    |    Percent   |
#> |---------------------------------------------------------------------------|
#> | 10.4  - 15.1  |      6       |      6       |        18.75 |        18.75 |
#> |---------------------------------------------------------------------------|
#> | 15.1  - 19.8  |      12      |      18      |         37.5 |        56.25 |
#> |---------------------------------------------------------------------------|
#> | 19.8  - 24.5  |      8       |      26      |           25 |        81.25 |
#> |---------------------------------------------------------------------------|
#> | 24.5  - 29.2  |      2       |      28      |         6.25 |         87.5 |
#> |---------------------------------------------------------------------------|
#> | 29.2  - 33.9  |      4       |      32      |         12.5 |          100 |
#> |---------------------------------------------------------------------------|
```

##### Group Summary

``` r
group_summary(mtcars$cyl, mtcars$mpg)
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

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
