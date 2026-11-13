# Measures of variation

Returns the measures of location such as range, variance and standard
deviation.

## Usage

``` r
ds_measures_variation(data, ..., decimals = 2)
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
ds_measures_variation(mtcarz, mpg)
#>   var  n range   iqr variance       sd coeff_var std_error
#> 1 mpg 32  23.5 7.375  36.3241 6.026948  29.99881  1.065424

# multiple columns
ds_measures_variation(mtcarz, mpg, disp)
#>    var  n range     iqr   variance         sd coeff_var std_error
#> 1 disp 32 400.9 205.175 15360.7998 123.938694  53.71779 21.909473
#> 2  mpg 32  23.5   7.375    36.3241   6.026948  29.99881  1.065424

# all columns
ds_measures_variation(mtcarz)
#>    var  n   range       iqr     variance          sd coeff_var   std_error
#> 1 disp 32 400.900 205.17500 1.536080e+04 123.9386938  53.71779 21.90947271
#> 2 drat 32   2.170   0.84000 2.858814e-01   0.5346787  14.86638  0.09451874
#> 3   hp 32 283.000  83.50000 4.700867e+03  68.5628685  46.74077 12.12031731
#> 4  mpg 32  23.500   7.37500 3.632410e+01   6.0269481  29.99881  1.06542396
#> 5 qsec 32   8.400   2.00750 3.193166e+00   1.7869432  10.01159  0.31588992
#> 6   wt 32   3.911   1.02875 9.573790e-01   0.9784574  30.41285  0.17296847

# vector
ds_measures_variation(mtcarz$mpg)
#>   variable  n range  iqr variance   sd coeff_var std_error
#> 1      mpg 32  23.5 7.38    36.32 6.03        30      1.07

# vectors of different length
disp <- mtcarz$disp[1:10]
ds_measures_variation(mtcarz$mpg, disp)
#>     variable  n range   iqr variance    sd coeff_var std_error
#> 1 mtcarz$mpg 32  23.5  7.38    36.32  6.03     30.00      1.07
#> 2       disp 10 252.0 99.73  8167.44 90.37     43.32     28.58

# decimal places
ds_measures_variation(mtcarz, disp, hp, decimals = 3)
#>    var  n range     iqr  variance        sd coeff_var std_error
#> 1 disp 32 400.9 205.175 15360.800 123.93869  53.71779  21.90947
#> 2   hp 32 283.0  83.500  4700.867  68.56287  46.74077  12.12032
```
