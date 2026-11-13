# Geometric Mean

Computes the geometric mean

## Usage

``` r
ds_gmean(data, x = NULL)
```

## Arguments

- data:

  A numeric vector or `data.frame`.

- x:

  Column in `data`.

## See also

[`ds_hmean`](https://descriptr.rsquaredacademy.com/reference/ds_hmean.md)
[`mean`](https://rdrr.io/r/base/mean.html)

## Examples

``` r
# vector
ds_gmean(mtcars$mpg)
#> [1] 19.25006

# data.frame
ds_gmean(mtcars, mpg)
#> [1] 19.25006
```
