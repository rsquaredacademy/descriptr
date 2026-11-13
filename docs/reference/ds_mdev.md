# Mean Absolute Deviation

Compute the mean absolute deviation about the mean

## Usage

``` r
ds_mdev(data, x = NULL)
```

## Arguments

- data:

  A numeric vector or `data.frame`.

- x:

  Column in `data`.

## Details

The `ds_mdev` function computes the mean absolute deviation about the
mean. It is different from `mad` in `stats` package as the statistic
used to compute the deviations is not `median` but `mean`. Any NA values
are stripped from `x` before computation takes place

## See also

[`mad`](https://rdrr.io/r/stats/mad.html)

## Examples

``` r
# vector
ds_mdev(mtcars$mpg)
#> [1] 4.714453

# data.frame
ds_mdev(mtcars, mpg)
#> [1] 4.714453
```
