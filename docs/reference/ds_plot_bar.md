# Generate bar plots

Creates bar plots if the data has categorical variables.

## Usage

``` r
ds_plot_bar(data, ..., fill = "blue", print_plot = TRUE)
```

## Arguments

- data:

  A `data.frame` or `tibble`.

- ...:

  Column(s) in `data`.

- fill:

  Color of the bars.

- print_plot:

  logical; if `TRUE`, prints the plot else returns a plot object.

## Examples

``` r
# plot single variable
ds_plot_bar(mtcarz, cyl)


# plot multiple variables
ds_plot_bar(mtcarz, cyl, gear)


# plot all variables
ds_plot_bar(mtcarz)


```
