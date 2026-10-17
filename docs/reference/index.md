# Package index

## Shiny App

Launches shiny app for interactive exploration of data sets.

- [`ds_launch_shiny_app()`](https://descriptr.rsquaredacademy.com/reference/ds_launch_shiny_app.md)
  : Launch Shiny App

## Data Screening

The `screener()` function will screen data frames and return details
such as variable names, class, levels and missing values. The
`plot.screener()` creates bar plots to visualize % of missing
observations for each variable in a data frame.

- [`ds_screener()`](https://descriptr.rsquaredacademy.com/reference/ds_screener.md)
  [`plot(`*`<ds_screener>`*`)`](https://descriptr.rsquaredacademy.com/reference/ds_screener.md)
  : Screen data

## Continuous Data

The following functions ease the process of generating and visualizing
descriptive statistics for continuous data.

- [`ds_auto_summary_stats()`](https://descriptr.rsquaredacademy.com/reference/ds_auto_summary_stats.md)
  : Descriptive statistics and frquency tables
- [`ds_extreme_obs()`](https://descriptr.rsquaredacademy.com/reference/ds_extreme_obs.md)
  : Extreme observations
- [`ds_freq_table()`](https://descriptr.rsquaredacademy.com/reference/ds_freq_table.md)
  [`plot(`*`<ds_freq_table>`*`)`](https://descriptr.rsquaredacademy.com/reference/ds_freq_table.md)
  : Frequency table
- [`ds_measures_location()`](https://descriptr.rsquaredacademy.com/reference/ds_measures_location.md)
  : Measures of location
- [`ds_measures_symmetry()`](https://descriptr.rsquaredacademy.com/reference/ds_measures_symmetry.md)
  : Measures of symmetry
- [`ds_measures_variation()`](https://descriptr.rsquaredacademy.com/reference/ds_measures_variation.md)
  : Measures of variation
- [`ds_percentiles()`](https://descriptr.rsquaredacademy.com/reference/ds_percentiles.md)
  : Percentiles
- [`ds_summary_stats()`](https://descriptr.rsquaredacademy.com/reference/ds_summary_stats.md)
  : Descriptive statistics
- [`ds_tidy_stats()`](https://descriptr.rsquaredacademy.com/reference/ds_tidy_stats.md)
  : Tidy descriptive statistics

## Categorical Data

The following functions ease the process of generating and visualizing
descriptive statistics for categorical data.

- [`ds_auto_freq_table()`](https://descriptr.rsquaredacademy.com/reference/ds_auto_freq_table.md)
  [`ds_auto_cross_table()`](https://descriptr.rsquaredacademy.com/reference/ds_auto_freq_table.md)
  : Multiple One & Two Way Tables
- [`ds_cross_table()`](https://descriptr.rsquaredacademy.com/reference/ds_cross_table.md)
  [`plot(`*`<ds_cross_table>`*`)`](https://descriptr.rsquaredacademy.com/reference/ds_cross_table.md)
  [`ds_twoway_table()`](https://descriptr.rsquaredacademy.com/reference/ds_cross_table.md)
  : Two way table
- [`ds_freq_table()`](https://descriptr.rsquaredacademy.com/reference/ds_freq_table.md)
  [`plot(`*`<ds_freq_table>`*`)`](https://descriptr.rsquaredacademy.com/reference/ds_freq_table.md)
  : Frequency table

## Grouped Summary

The following functions generate grouped summary statistics.

- [`ds_auto_group_summary()`](https://descriptr.rsquaredacademy.com/reference/ds_auto_group_summary.md)
  : Tabulation
- [`ds_group_summary()`](https://descriptr.rsquaredacademy.com/reference/ds_group_summary.md)
  [`plot(`*`<ds_group_summary>`*`)`](https://descriptr.rsquaredacademy.com/reference/ds_group_summary.md)
  : Groupwise descriptive statistics
- [`ds_group_summary_interact()`](https://descriptr.rsquaredacademy.com/reference/ds_group_summary_interact.md)
  : Category wise descriptive statistics

## Visualization

The following functions generate plots for different data types.

- [`ds_plot_bar()`](https://descriptr.rsquaredacademy.com/reference/ds_plot_bar.md)
  : Generate bar plots
- [`ds_plot_bar_grouped()`](https://descriptr.rsquaredacademy.com/reference/ds_plot_bar_grouped.md)
  : Generate grouped bar plots
- [`ds_plot_bar_stacked()`](https://descriptr.rsquaredacademy.com/reference/ds_plot_bar_stacked.md)
  : Generate stacked bar plots
- [`ds_plot_box_group()`](https://descriptr.rsquaredacademy.com/reference/ds_plot_box_group.md)
  : Compare distributions
- [`ds_plot_box_single()`](https://descriptr.rsquaredacademy.com/reference/ds_plot_box_single.md)
  : Generate box plots
- [`ds_plot_density()`](https://descriptr.rsquaredacademy.com/reference/ds_plot_density.md)
  : Generate density plots
- [`ds_plot_histogram()`](https://descriptr.rsquaredacademy.com/reference/ds_plot_histogram.md)
  : Generate histograms
- [`ds_plot_scatter()`](https://descriptr.rsquaredacademy.com/reference/ds_plot_scatter.md)
  : Generate scatter plots

## Other Functions

A set of functions used to generate `summary_stats`.

- [`ds_css()`](https://descriptr.rsquaredacademy.com/reference/ds_css.md)
  : Corrected Sum of Squares
- [`ds_cvar()`](https://descriptr.rsquaredacademy.com/reference/ds_cvar.md)
  : Coefficient of Variation
- [`ds_gmean()`](https://descriptr.rsquaredacademy.com/reference/ds_gmean.md)
  : Geometric Mean
- [`ds_hmean()`](https://descriptr.rsquaredacademy.com/reference/ds_hmean.md)
  : Harmonic Mean
- [`ds_kurtosis()`](https://descriptr.rsquaredacademy.com/reference/ds_kurtosis.md)
  : Kurtosis
- [`ds_mdev()`](https://descriptr.rsquaredacademy.com/reference/ds_mdev.md)
  : Mean Absolute Deviation
- [`ds_mode()`](https://descriptr.rsquaredacademy.com/reference/ds_mode.md)
  : Mode
- [`ds_range()`](https://descriptr.rsquaredacademy.com/reference/ds_range.md)
  : Range
- [`ds_rindex()`](https://descriptr.rsquaredacademy.com/reference/ds_rindex.md)
  : Index Values
- [`ds_skewness()`](https://descriptr.rsquaredacademy.com/reference/ds_skewness.md)
  : Skewness
- [`ds_std_error()`](https://descriptr.rsquaredacademy.com/reference/ds_std_error.md)
  : Standard error of mean
- [`ds_tailobs()`](https://descriptr.rsquaredacademy.com/reference/ds_tailobs.md)
  : Tail Observations

## Data

descriptr comes with a built-in dataset `hsb` which is used to
illustrate the functions as well as for running tests.

- [`hsb`](https://descriptr.rsquaredacademy.com/reference/hsb.md) : High
  School and Beyond Data Set
- [`mtcarz`](https://descriptr.rsquaredacademy.com/reference/mtcarz.md)
  : mtcarz
