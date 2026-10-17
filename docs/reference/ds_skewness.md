# Skewness

Compute the skewness of a probability distribution.

## Usage

``` r
ds_skewness(data, x = NULL)
```

## Arguments

- data:

  A numeric vector or `data.frame`.

- x:

  Column in `data`.

## References

Sheskin, D.J. (2000) Handbook of Parametric and Nonparametric
Statistical Procedures, Second Edition. Boca Raton, Florida: Chapman &
Hall/CRC.

## See also

`kurtosis`

## Examples

``` r
# vector
ds_skewness(mtcars$mpg)
#> [1] 0.6723771

# data.frame
ds_skewness(mtcars, mpg)
#> [1] 0.6723771
```
