# descriptr (development version)

## Bug Fixes

- `ds_group_summary()` no longer errors when the continuous variable contains missing values; groups are summarised with `na.rm = TRUE` and the number of missing observations is reported.
- `ds_group_summary()` no longer errors when the grouping factor has unused levels; unused levels are dropped.
- `ds_measures_variation()`, `ds_measures_symmetry()`, `ds_percentiles()` and `ds_tidy_stats()` now work on data frames with a single numeric column.
- `ds_group_summary_interact()` no longer truncates factor levels containing `.`, and character grouping columns are now used instead of being silently dropped.
- `ds_summary_stats()` and `ds_extreme_obs()` now work when a variable has fewer than five non-missing values.
- `ds_freq_table()` no longer double-counts values that fall on a bin boundary: intervals are half-open with the last interval closed, so frequencies sum to the number of observations and the final cumulative percent is 100. It also gains a `closed` argument (`"left"` default, `"right"`, or `"both"` for the legacy double-counting behaviour) ([#131](https://github.com/rsquaredacademy/descriptr/issues/131)).
- `ds_measures_variation()`, `ds_measures_symmetry()`, `ds_percentiles()` and `ds_tidy_stats()` handle missing values per column instead of deleting rows listwise, so missing values in one variable no longer change results for another. A column with no non-missing values returns `NA` for every summary metric instead of `-Inf`/`Inf` or an error ([#132](https://github.com/rsquaredacademy/descriptr/issues/132)).
- `ds_extreme_obs()` and `ds_rindex()` report the original row indices of the data supplied, report ties on distinct rows, and `ds_extreme_obs()` now applies `decimals` to data-frame input ([#133](https://github.com/rsquaredacademy/descriptr/issues/133)).
- `ds_gmean()` computes on the log scale, avoiding overflow for large values and long vectors, and `ds_gmean()`/`ds_hmean()` return `NA` with a warning for non-positive values ([#134](https://github.com/rsquaredacademy/descriptr/issues/134)).
- `ds_measures_variation()`, `ds_measures_symmetry()` and `ds_percentiles()` now apply `decimals` to data-frame input, as they already did for numeric vectors.
- `print.ds_group_summary()` now titles the table with the variables, e.g. `mpg by cyl`, instead of an empty `by`.
- The missing row of a factor frequency table now reports its percent instead of a blank cell.
- `ds_tailobs()` checks `n` against the number of non-missing observations, so it no longer returns `NA` values when `n` exceeds that number.
- `ds_auto_summary_stats()` no longer prints a stray `NULL` after each summary.
- The deprecated `ds_freq_cont()` forwards its variable correctly and no longer fails with `object 'mpg' not found`.
- `bins` must be a positive whole number; values such as `0`, `2.7` and `NA` are rejected with a clear error instead of a base error or silent truncation.
- `ds_summary_stats()` validates every selected column before printing, so a mixed selection fails up front and names the offending column.
- `ds_twoway_table()` no longer prints a dplyr join message.
- Cross tables print the Percent, Row Pct and Col Pct cells as percentages; the values stored in the returned object remain proportions ([#152](https://github.com/rsquaredacademy/descriptr/issues/152)).
- `ds_measures_location()` keeps the column order selected by the user, and the data's column order when no columns are selected, instead of sorting the rows alphabetically ([#152](https://github.com/rsquaredacademy/descriptr/issues/152)).
- `ds_auto_cross_table()`, `ds_plot_scatter()`, `ds_plot_bar_stacked()` and `ds_plot_bar_grouped()` count usable variables, also when no columns are selected, and stop with a clear message when fewer than two are available instead of failing with a raw `combn()` error ([#152](https://github.com/rsquaredacademy/descriptr/issues/152)).
- Character columns are treated as categorical (converted with `factor()`) by the frequency tables, cross tables, group summaries, automatic tables and categorical plots, so results match the same data stored as factors ([#152](https://github.com/rsquaredacademy/descriptr/issues/152)).
- `ds_measures_location()`, `ds_measures_variation()`, `ds_measures_symmetry()` and `ds_percentiles()` stop with the `check_numeric()` message when a non-numeric column is selected explicitly; with no selection they use the numeric columns ([#152](https://github.com/rsquaredacademy/descriptr/issues/152)).
- Cross tables store the number of rows dropped for missing values in `utility$missing` and print `Missing Observations` when there are any; `ds_auto_cross_table()` prints each table's own observation count instead of the number of rows in the data ([#152](https://github.com/rsquaredacademy/descriptr/issues/152)).

# descriptr 0.6.0

## New Features

- `ds_summary_stats()` can handle both `data.frame` and numeric vector as input.

## Bug Fixes

- Incorrect column name generates missing output ([#123](https://github.com/rsquaredacademy/descriptr/issues/123))
- Fix ggplot2 deprecated function warnings

## Other Changes

- Incorrect package alias ([#122](https://github.com/rsquaredacademy/descriptr/issues/122))
- Migrate to testthat edition 3 ([#124](https://github.com/rsquaredacademy/descriptr/issues/124))
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

- `ds_tidy_stats()` will detect continuous variables in the data set and return
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



