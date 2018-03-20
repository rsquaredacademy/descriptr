# descriptr 0.4.1

## New Features

- Feature request: add turn off button in the app
([#29](https://github.com/rsquaredacademy/descriptr/issues/29))
- Show frequency of missing values in frequency table
([#32](https://github.com/rsquaredacademy/descriptr/issues/32))
- `ds_freq_cont()` should return a tibble
([#34](https://github.com/rsquaredacademy/descriptr/issues/34))

The following now return tibbles to facilitate further analysis:

- `ds_twoway_table()`
- `ds_freq_table()`
- `ds_freq_cont()`
- `ds_group_summary()`

New to descriptr are the following which generate summary statistics:

- [`ds_measures_location()`](https://descriptr.rsquaredacademy.com/reference/ds_measures_location.html)
- [`ds_measures_variation()`](https://descriptr.rsquaredacademy.com/reference/ds_measures_variation.html)
- [`ds_measures_symmetry()`](https://descriptr.rsquaredacademy.com/reference/ds_measures_symmetry.html)
- [`ds_percentiles()`](https://descriptr.rsquaredacademy.com/reference/ds_percentiles.html)
- [`ds_extreme_obs()`](https://descriptr.rsquaredacademy.com/reference/ds_extreme_obs.html)

## Bug Fixes

- `descriptr:::print_screen()` fails if a variable has more than one class 
([#26](https://github.com/rsquaredacademy/descriptr/issues/26))
- `ds_summary_stats()` does not show missing values 
([#30](https://github.com/rsquaredacademy/descriptr/issues/30))
- Multiple variable statistics throws error in the presence of NA
([#33](https://github.com/rsquaredacademy/descriptr/issues/33))
- Error in cross table in presence of NA
([#40](https://github.com/rsquaredacademy/descriptr/issues/40))

## Acknowledgements

A big thanks to [@GegznaV](https://github.com/GegznaV) and [@adam_medcalf](https://twitter.com/adam_medcalf) who contributed code, opened issues and provided valuable feedback.


# descriptr 0.4.0

## New Features

We have completely revamped the API. All the functions now
take a `data.frame` or `tibble` as the first argument followed 
by the variable names. The variable names need not be surrounded
by single/double quotes anymore. Please view the guide for 
more details.

- use ggplot for all plots
([#8](https://github.com/rsquaredacademy/descriptr/issues/8))

## Bug Fixes

- update shiny app
([#16](https://github.com/rsquaredacademy/descriptr/issues/16))
- error in percentiles in `ds_summary_stats`
([#22](https://github.com/rsquaredacademy/descriptr/issues/22))



# descriptr 0.3.0

## New Features

- shiny app for interactive analysis
- multi column statistics
([#6](https://github.com/rsquaredacademy/descriptr/issues/6))

# descriptr 0.2.0

This is a minor release containing bug fixes. 

## Bug Fixes

- multiple one way table returns an error ([#2](https://github.com/rsquaredacademy/descriptr/issues/2))
- ftable returned by freq_table must be data.frame or tibble ([#3](https://github.com/rsquaredacademy/descriptr/issues/3))
- multiple one way table returns an error ([#4](https://github.com/rsquaredacademy/descriptr/issues/4))

# descriptr 0.1.1

## Bug Fixes

- typo in `group_summary()`; maximum and minimum were reversed 
- `norm_plot()` was non-reactive to changes in standard deviation (#[1](https://github.com/rsquaredacademy/descriptr/issues/1)).

# descriptr 0.1.0

* First release



