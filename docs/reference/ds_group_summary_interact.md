# Category wise descriptive statistics

Descriptive statistics of a continuous variable for the combination of
levels of two or more categorical variables.

## Usage

``` r
ds_group_summary_interact(data, col, ...)
```

## Arguments

- data:

  A `data.frame` or a `tibble`.

- col:

  Column in `data`; continuous variable.

- ...:

  Columns in `data`; categorical variables.

## See also

[`ds_group_summary`](https://descriptr.rsquaredacademy.com/reference/ds_group_summary.md)

## Examples

``` r
ds_group_summary_interact(mtcarz, mpg, cyl, gear)
#> # A tibble: 8 × 17
#>   cyl   gear    min   max  mean t_mean median  mode  range variance  stdev
#>   <chr> <chr> <dbl> <dbl> <dbl>  <dbl>  <dbl> <dbl>  <dbl>    <dbl>  <dbl>
#> 1 4     3      21.5  21.5  21.5   21.5   21.5  21.5  0       NA     NA    
#> 2 6     3      18.1  21.4  19.8   19.8   19.8  18.1  3.30     5.44   2.33 
#> 3 8     3      10.4  19.2  15.0   15.0   15.2  10.4  8.8      7.70   2.77 
#> 4 4     4      21.4  33.9  26.9   26.9   25.8  22.8 12.5     23.1    4.81 
#> 5 6     4      17.8  21    19.8   19.8   20.1  21    3.2      2.41   1.55 
#> 6 4     5      26    30.4  28.2   28.2   28.2  26    4.4      9.68   3.11 
#> 7 6     5      19.7  19.7  19.7   19.7   19.7  19.7  0       NA     NA    
#> 8 8     5      15    15.8  15.4   15.4   15.4  15    0.800    0.320  0.566
#> # ℹ 6 more variables: skew <dbl>, kurtosis <dbl>, coeff_var <dbl>, q1 <dbl>,
#> #   q3 <dbl>, iqrange <dbl>
```
