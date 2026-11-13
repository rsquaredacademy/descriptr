# Tail Observations

Returns the n highest/lowest observations from a numeric vector.

## Usage

``` r
ds_tailobs(data, n, type = c("low", "high"), decimals = 2)
```

## Arguments

- data:

  a numeric vector

- n:

  number of observations to be returned

- type:

  if `low`, the `n` lowest observations are returned, else the highest
  `n` observations are returned.

- decimals:

  An option to specify the exact number of decimal places to use. The
  default number of decimal places is 2.

## Value

`n` highest/lowest observations from `data`

## Details

Any NA values are stripped from `data` before computation takes place.

## See also

[`top_n`](https://dplyr.tidyverse.org/reference/top_n.html)

## Examples

``` r
# 5 lowest observations
ds_tailobs(mtcarz$mpg, 5)
#> [1] 10.4 10.4 13.3 14.3 14.7

# 5 highest observations
ds_tailobs(mtcarz$mpg, 5, type = "high")
#> [1] 33.9 32.4 30.4 30.4 27.3

# specify decimal places to display
ds_tailobs(mtcarz$mpg, 5, decimals = 3)
#> [1] 10.4 10.4 13.3 14.3 14.7
```
