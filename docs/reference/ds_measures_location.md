# Measures of location

Returns the measures of location such as mean, median & mode.

## Usage

``` r
ds_measures_location(data, ..., trim = 0.05, decimals = 2)
```

## Arguments

- data:

  A `data.frame` or `tibble` or numeric vector.

- ...:

  Column(s) in `data` or numeric vectors.

- trim:

  The fraction of values to be trimmed before computing the mean.

- decimals:

  An option to specify the exact number of decimal places to use. The
  default number of decimal places is 2.

## Examples

``` r
# single column
ds_measures_location(mtcarz, mpg)
#>   variable  n missing  mean trim_mean median mode
#> 1      mpg 32       0 20.09     19.95   19.2 10.4

# multiple columns
ds_measures_location(mtcarz, mpg, disp)
#>   variable  n missing   mean trim_mean median  mode
#> 1     disp 32       0 230.72    228.00  196.3 275.8
#> 2      mpg 32       0  20.09     19.95   19.2  10.4

# all columns
ds_measures_location(mtcarz)
#>   variable  n missing   mean trim_mean median   mode
#> 1     disp 32       0 230.72    228.00 196.30 275.80
#> 2     drat 32       0   3.60      3.58   3.70   3.07
#> 3       hp 32       0 146.69    143.57 123.00 110.00
#> 4      mpg 32       0  20.09     19.95  19.20  10.40
#> 5     qsec 32       0  17.85     17.79  17.71  17.02
#> 6       wt 32       0   3.22      3.20   3.33   3.44

# vector
ds_measures_location(mtcarz$mpg)
#>   variable  n missing  mean trim_mean median mode
#> 1      mpg 32       0 20.09     19.95   19.2 10.4

# vectors of different length
disp <- mtcarz$disp[1:10]
ds_measures_location(mtcarz$mpg, disp)
#>     variable  n missing   mean trim_mean median  mode
#> 1       disp 10       0 208.61    208.61  163.8 160.0
#> 2 mtcarz$mpg 32       0  20.09     19.95   19.2  10.4

# decimal places
ds_measures_location(mtcarz, disp, hp, decimals = 3)
#>   variable  n missing    mean trim_mean median  mode
#> 1     disp 32       0 230.722   228.000  196.3 275.8
#> 2       hp 32       0 146.688   143.567  123.0 110.0
```
