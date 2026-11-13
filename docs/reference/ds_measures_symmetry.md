# Measures of symmetry

Returns the measures of symmetry such as skewness and kurtosis.

## Usage

``` r
ds_measures_symmetry(data, ..., decimals = 2)
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
ds_measures_symmetry(mtcarz, mpg)
#>   var  n  skewness    kurtosis
#> 1 mpg 32 0.6723771 -0.02200629

# multiple columns
ds_measures_symmetry(mtcarz, mpg, disp)
#>    var  n  skewness    kurtosis
#> 1 disp 32 0.4202331 -1.06752340
#> 2  mpg 32 0.6723771 -0.02200629

# all columns
ds_measures_symmetry(mtcarz)
#>    var  n  skewness    kurtosis
#> 1 disp 32 0.4202331 -1.06752340
#> 2 drat 32 0.2927802 -0.45043245
#> 3   hp 32 0.7994067  0.27521159
#> 4  mpg 32 0.6723771 -0.02200629
#> 5 qsec 32 0.4063466  0.86493065
#> 6   wt 32 0.4659161  0.41659467

# vector
ds_measures_symmetry(mtcarz$mpg)
#>   var  n skewness kurtosis
#> 1 mpg 32     0.67    -0.02

# vectors of different length
disp <- mtcarz$disp[1:10]
ds_measures_symmetry(mtcarz$mpg, disp)
#>          var  n skewness kurtosis
#> 1 mtcarz$mpg 32     0.67    -0.02
#> 2       disp 10     0.98    -0.38

# decimal places
ds_measures_symmetry(mtcarz, disp, hp, decimals = 3)
#>    var  n  skewness   kurtosis
#> 1 disp 32 0.4202331 -1.0675234
#> 2   hp 32 0.7994067  0.2752116
```
