# Index Values

Returns index of values.

## Usage

``` r
ds_rindex(data, values)
```

## Arguments

- data:

  a numeric vector

- values:

  a numeric vector containing the values whose index is returned

## Value

Index of the `values` in `data`. In case, `data` does not contain
`index`, `NULL` is returned.

## Examples

``` r
# returns index of 21
ds_rindex(mtcars$mpg, 21)
#> [1] 1 2

# returns NULL
ds_rindex(mtcars$mpg, 22)
#> NULL
```
