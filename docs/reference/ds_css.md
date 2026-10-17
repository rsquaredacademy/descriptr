# Corrected Sum of Squares

Compute the corrected sum of squares

## Usage

``` r
ds_css(data, x = NULL)
```

## Arguments

- data:

  A numeric vector or `data.frame`.

- x:

  Column in `data`.

## Examples

``` r
# vector
ds_css(mtcars$mpg)
#> [1] 1126.047

# data.frame
ds_css(mtcars, mpg)
#> [1] 1126.047
```
