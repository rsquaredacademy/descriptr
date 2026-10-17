# Percentiles

Returns the percentiles

## Usage

``` r
ds_percentiles(data, ..., decimals = 2)
```

## Arguments

- data:

  A `data.frame` or `tibble`.

- ...:

  Column(s) in `data`.

- decimals:

  An option to specify the exact number of decimal places to use. The
  default number of decimal places is 2.

## Examples

``` r
# single column
ds_percentiles(mtcarz, mpg)
#>   var  n  min per_1  per_5 per_10     q1 median   q3 per_90 per_95 per_99  max
#> 1 mpg 32 10.4  10.4 11.995  14.34 15.425   19.2 22.8  30.09   31.3 33.435 33.9

# multiple columns
ds_percentiles(mtcarz, mpg, disp)
#>    var  n  min  per_1  per_5 per_10      q1 median    q3 per_90 per_95  per_99
#> 1 disp 32 71.1 72.526 77.350  80.61 120.825  196.3 326.0 396.00  449.0 468.280
#> 2  mpg 32 10.4 10.400 11.995  14.34  15.425   19.2  22.8  30.09   31.3  33.435
#>     max
#> 1 472.0
#> 2  33.9

# all columns
ds_percentiles(mtcarz)
#>    var  n    min    per_1   per_5  per_10        q1  median     q3   per_90
#> 1 disp 32 71.100 72.52600 77.3500 80.6100 120.82500 196.300 326.00 396.0000
#> 2 drat 32  2.760  2.76000  2.8535  3.0070   3.08000   3.695   3.92   4.2090
#> 3   hp 32 52.000 55.10000 63.6500 66.0000  96.50000 123.000 180.00 243.5000
#> 4  mpg 32 10.400 10.40000 11.9950 14.3400  15.42500  19.200  22.80  30.0900
#> 5 qsec 32 14.500 14.53100 15.0455 15.5340  16.89250  17.710  18.90  19.9900
#> 6   wt 32  1.513  1.54462  1.7360  1.9555   2.58125   3.325   3.61   4.0475
#>      per_95    per_99     max
#> 1 449.00000 468.28000 472.000
#> 2   4.31450   4.77500   4.930
#> 3 253.55000 312.99000 335.000
#> 4  31.30000  33.43500  33.900
#> 5  20.10450  22.06920  22.900
#> 6   5.29275   5.39951   5.424

# vector
ds_percentiles(mtcarz$mpg)
#>    var  n  min per_1 per_5 per_10    q1 median   q3 per_90 per_95 per_99  max
#> 1% mpg 32 10.4  10.4    12  14.34 15.43   19.2 22.8  30.09   31.3  33.44 33.9

# vectors of different length
disp <- mtcarz$disp[1:10]
ds_percentiles(mtcarz$mpg, disp)
#>          var  n   min  per_1  per_5 per_10     q1 median     q3 per_90 per_95
#> 1 mtcarz$mpg 32  10.4  10.40  12.00  14.34  15.43   19.2  22.80  30.09   31.3
#> 2       disp 10 108.0 110.95 122.76 137.52 150.02  163.8 249.75 360.00  360.0
#>   per_99   max
#> 1  33.44  33.9
#> 2 360.00 360.0

# decimal places
ds_percentiles(mtcarz, disp, hp, decimals = 3)
#>    var  n  min  per_1 per_5 per_10      q1 median  q3 per_90 per_95 per_99 max
#> 1 disp 32 71.1 72.526 77.35  80.61 120.825  196.3 326  396.0 449.00 468.28 472
#> 2   hp 32 52.0 55.100 63.65  66.00  96.500  123.0 180  243.5 253.55 312.99 335
```
