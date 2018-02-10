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



