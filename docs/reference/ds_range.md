# Range

Compute the range of a numeric vector

## Usage

``` r
ds_range(data, x = NULL)
```

## Arguments

- data:

  A numeric vector or `data.frame`.

- x:

  Column in `data`.

## Value

Range of `x`

## See also

[`range`](https://rdrr.io/r/base/range.html)

## Examples

``` r
# vector
ds_range(mtcars$mpg)
#> [1] 23.5

# data.frame
ds_range(mtcars, mpg)
#> [1] 23.5
```
