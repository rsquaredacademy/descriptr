# Mode

Compute the sample mode

## Usage

``` r
ds_mode(data, x = NULL)
```

## Arguments

- data:

  A numeric vector or `data.frame`.

- x:

  Column in `data`.

## Value

Mode of `x`

## Details

Any NA values are stripped from `x` before computation takes place.

## See also

[`mean`](https://rdrr.io/r/base/mean.html)
[`median`](https://rdrr.io/r/stats/median.html)

## Examples

``` r
# vector
ds_mode(mtcars$mpg)
#> [1] 10.4

# data.frame
ds_mode(mtcars, mpg)
#> [1] 10.4
```
