# Generate box plots

Creates box plots if the data has continuous variables.

## Usage

``` r
ds_plot_box_single(data, ..., print_plot = TRUE)
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
# plot single variable
ds_plot_box_single(mtcarz, mpg)


# plot multiple variables
ds_plot_box_single(mtcarz, mpg, disp, hp)


# plot all variables
ds_plot_box_single(mtcarz)


```
