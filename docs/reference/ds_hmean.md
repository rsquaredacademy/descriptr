# Harmonic Mean

Computes the harmonic mean

## Usage

``` r
ds_hmean(data, x = NULL)
```

## Arguments

- data:

  A numeric vector or `data.frame`.

- x:

  Column in `data`.

## See also

[`ds_gmean`](https://descriptr.rsquaredacademy.com/reference/ds_gmean.md)
[`mean`](https://rdrr.io/r/base/mean.html)

## Examples

``` r
# vector
ds_hmean(mtcars$mpg)
#> [1] 18.44092

# data.frame
ds_hmean(mtcars, mpg)
#> [1] 18.44092
```
