# descriptr 0.6.0

## New Features

- `ds_summary_stats()` can handle both `data.frame` and numeric vector as input.

## BUg Fixes

- Incorrect column name generates missing output ([#123](https://github.com/rsquaredacademy/descriptr/issues/123))
- Fix gglotp2 deprecated function warnings

## Other Changes

- Incorrect package alias ([#122](https://github.com/rsquaredacademy/descriptr/issues/122))
- Migrate to testthat editioin 3 ([#124](https://github.com/rsquaredacademy/descriptr/issues/124))
- Migrate to rhub v2 ([#125](https://github.com/rsquaredacademy/descriptr/issues/125))

# descriptr 0.5.2

This is a patch release to fix CRAN notification about vdiffr package 
and remove deprecated functions related to visualization of 
statistical distributions.

## Defunct

The following functions are defunct. Please use the **vistributions**
package for visualizing distributions.

- `dist_binom_plot()`
- `dist_binom_prob()`
- `dist_binom_perc()`
- `dist_chisquare_plot()` 
- `dist_chisquare_perc()` 
- `dist_chisquare_prob()`
- `dist_normal_plot()` 
- `dist_normal_perc()` 
- `dist_normal_prob()`
- `dist_f_plot()` 
- `dist_f_perc()` 
- `dist_f_prob()`
- `dist_t_plot()` 
- `dist_t_perc()` 
- `dist_t_prob()`

# descriptr 0.5.1

This is a patch release to fix CRAN errors.

# descriptr 0.5.0

## New Features

- `ds_tiy_stats()` will detect continuous variables in the data set and return
summary statistics for all of them. ([#68](https://github.com/rsquaredacademy/descriptr/issues/68))
- `ds_auto_summary_stats()` will detect all continuous variables in the data 
set and return summary statistics and frequency tables for all of them. 
([#69](https://github.com/rsquaredacademy/descriptr/issues/69))
- `ds_plot_*()` family of functions will detect continuous and categorical 
variables in the data set and generate appropriate plots. ([#70](https://github.com/rsquaredacademy/descriptr/issues/70))
- `ds_launch_shiny_app()` will check if suggested packages are available and 
offer to install the missing packages. ([#76](https://github.com/rsquaredacademy/descriptr/issues/76))
- `ds_measures_*()` family of functions now accept multiple arguments. ([#78](https://github.com/rsquaredacademy/descriptr/issues/78))
- `ds_auto_freq_table()` and `ds_auto_cross_table()` allow users to specify a 
subset of variables. ([#83](https://github.com/rsquaredacademy/descriptr/issues/83))
- `ds_freq_table()` now works for both continuous and categorical data. ([#90](https://github.com/rsquaredacademy/descriptr/issues/90))
- Some minor improvements to the error messages. ([#51](https://github.com/rsquaredacademy/descriptr/issues/51))

## Deprecation

All the `dist_*` functions related to visualizing probability distributions have
been soft deprecated and will be removed in the next release. Please use the 
[vistributions](https://cran.r-project.org/package=vistributions) package going forward.

The shiny app has been soft-deprecated and will be removed in the next release. 
Please use the [xplorerr](https://cran.r-project.org/package=xplorerr) package 
going forward.

The following functions have been soft-deprecated and will be removed in the 
next release:

- `ds_freq_cont()`
- `ds_oway_tables()`
- `ds_tway_tables()`

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

A big thanks to [@GegznaV](https://github.com/GegznaV) and [@adam_medcalf](https://dabblingwithdata.amedcalf.com) who contributed code, opened issues and provided valuable feedback.


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



