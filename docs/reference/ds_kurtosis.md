# Kurtosis

Compute the kurtosis of a probability distribution.

## Usage

``` r
ds_kurtosis(data, x = NULL)
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

`ds_skewness`

## Examples

``` r
# vector
ds_kurtosis(mtcars$mpg)
#> [1] -0.02200629

# data.frame
ds_kurtosis(mtcars, mpg)
#> [1] -0.02200629
```
