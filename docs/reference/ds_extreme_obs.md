# Extreme observations

Returns the most extreme observations.

## Usage

``` r
ds_extreme_obs(data, col, decimals = 2)
```

## Arguments

- data:

  A numeric vector or `data.frame` or `tibble`.

- col:

  Column in `data`.

- decimals:

  An option to specify the exact number of decimal places to use. The
  default number of decimal places is 2.

## Examples

``` r
# data.frame
ds_extreme_obs(mtcarz, mpg)
#>    type value index
#> 1  high  33.9    20
#> 2  high  32.4    18
#> 3  high  30.4    19
#> 4  high  30.4    28
#> 5  high  27.3    26
#> 6   low  10.4    15
#> 7   low  10.4    16
#> 8   low  13.3    24
#> 9   low  14.3     7
#> 10  low  14.7    17

# vector
ds_extreme_obs(mtcarz$mpg)
#>    type value index
#> 1  high  33.9    20
#> 2  high  32.4    18
#> 3  high  30.4    19
#> 4  high  30.4    28
#> 5  high  27.3    26
#> 6   low  10.4    15
#> 7   low  10.4    16
#> 8   low  13.3    24
#> 9   low  14.3     7
#> 10  low  14.7    17

# decimal places
ds_extreme_obs(mtcarz$mpg, decimals = 3)
#>    type value index
#> 1  high  33.9    20
#> 2  high  32.4    18
#> 3  high  30.4    19
#> 4  high  30.4    28
#> 5  high  27.3    26
#> 6   low  10.4    15
#> 7   low  10.4    16
#> 8   low  13.3    24
#> 9   low  14.3     7
#> 10  low  14.7    17
```
