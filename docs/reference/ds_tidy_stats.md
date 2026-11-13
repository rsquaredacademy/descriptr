# Tidy descriptive statistics

Descriptive statistics for multiple variables.

## Usage

``` r
ds_tidy_stats(data, ...)
```

## Arguments

- data:

  A `tibble` or a `data.frame`.

- ...:

  Columns in `x`.

## Value

A tibble.

## Deprecated Functions

`ds_multi_stats()` have been deprecated. Instead use `ds_tidy_stats()`.

## Examples

``` r
# all columns
ds_tidy_stats(mtcarz)
#> # A tibble: 6 × 16
#>   vars    min    max   mean t_mean median   mode  range  variance   stdev  skew
#>   <chr> <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>     <dbl>   <dbl> <dbl>
#> 1 disp  71.1  472    231.   228    196.   276.   401.   15361.    124.    0.420
#> 2 drat   2.76   4.93   3.60   3.58   3.70   3.07   2.17     0.286   0.535 0.293
#> 3 hp    52    335    147.   144.   123    110    283     4701.     68.6   0.799
#> 4 mpg   10.4   33.9   20.1   20.0   19.2   10.4   23.5     36.3     6.03  0.672
#> 5 qsec  14.5   22.9   17.8   17.8   17.7   17.0    8.4      3.19    1.79  0.406
#> 6 wt     1.51   5.42   3.22   3.20   3.32   3.44   3.91     0.957   0.978 0.466
#> # ℹ 5 more variables: kurtosis <dbl>, coeff_var <dbl>, q1 <dbl>, q3 <dbl>,
#> #   iqrange <dbl>

# multiple columns
ds_tidy_stats(mtcarz, mpg, disp, hp)
#> # A tibble: 3 × 16
#>   vars    min   max  mean t_mean median  mode range variance  stdev  skew
#>   <chr> <dbl> <dbl> <dbl>  <dbl>  <dbl> <dbl> <dbl>    <dbl>  <dbl> <dbl>
#> 1 disp   71.1 472   231.   228    196.  276.  401.   15361.  124.   0.420
#> 2 hp     52   335   147.   144.   123   110   283     4701.   68.6  0.799
#> 3 mpg    10.4  33.9  20.1   20.0   19.2  10.4  23.5     36.3   6.03 0.672
#> # ℹ 5 more variables: kurtosis <dbl>, coeff_var <dbl>, q1 <dbl>, q3 <dbl>,
#> #   iqrange <dbl>
```
