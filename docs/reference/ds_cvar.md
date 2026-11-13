# Coefficient of Variation

Compute the coefficient of variation

## Usage

``` r
ds_cvar(data, x = NULL)
```

## Arguments

- data:

  A numeric vector or `data.frame`.

- x:

  Column in `data`.

## Examples

``` r
# vector
ds_cvar(mtcars$mpg)
#> [1] 29.99881

# data.frame
ds_cvar(mtcars, mpg)
#> [1] 29.99881
```
