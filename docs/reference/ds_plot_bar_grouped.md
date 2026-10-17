# Generate grouped bar plots

Creates grouped bar plots if the data has categorical variables.

## Usage

``` r
ds_plot_bar_grouped(data, ..., print_plot = TRUE)
```

## Arguments

- data:

  A `data.frame` or `tibble`.

- ...:

  Column(s) in `data`.

- print_plot:

  logical; if `TRUE`, prints the plot else returns a plot object.

## Examples

``` r
# subset data
mt <- dplyr::select(mtcarz, cyl, gear, am)

# grouped bar plot
ds_plot_bar_grouped(mtcarz, cyl, gear)


# plot all variables
ds_plot_bar_grouped(mt)


```
